(in-package #:mime-protocol)

(defclass mime-entity ()
  ((headers :initarg :headers :accessor mime-headers :initform nil)
   (content-type :initarg :content-type :accessor mime-content-type :initform nil)
   (content-disposition :initarg :content-disposition :accessor mime-content-disposition
                        :initform nil)
   (transfer-encoding :initarg :transfer-encoding :accessor mime-transfer-encoding
                      :initform :7bit)
   (content :initarg :content :accessor mime-content :initform nil)
   (parts :initarg :parts :accessor mime-parts :initform nil)
   (preamble :initarg :preamble :accessor mime-preamble :initform nil)
   (epilogue :initarg :epilogue :accessor mime-epilogue :initform nil))
  (:documentation "One MIME entity: headers + decoded body or multipart parts."))

(defun mime-entity-p (object)
  (typep object 'mime-entity))

(defun multipart-p (entity)
  (and (mime-entity-p entity)
       (let ((ct (mime-content-type entity)))
         (and ct (string= (media-type-type ct) "multipart")))))

(defun header-value (entity name)
  (cdr (assoc (string-downcase name) (mime-headers entity) :test #'string=)))

(defun %char-description (char)
  (let ((code (char-code char)))
    (format nil "U+~4,'0X~@[ (~A)~]" code
            (case code (0 "NUL") (9 "HTAB") (10 "LF") (13 "CR") (32 "space") (58 "colon")))))

(defun %ftext-p (char)
  "RFC 5322 3.6.8 ftext: printable US-ASCII except colon."
  (let ((code (char-code char)))
    (and (<= 33 code 126) (/= code 58))))

(defun %header-name-problem (name)
  "Why NAME cannot name a header field, or NIL. SET-HEADER and PRINT-MIME
both ask it: a name with a blank, a colon, a control or a non-ASCII
character would end the header block early or merge the field into the one
before it in an RFC 5322 reader."
  (cond
   ((not (typep name '(or string symbol character)))
    (format nil "name ~S is not a string" name))
   ((zerop (length (string name)))
    "the name is empty")
   (t
    (let ((bad (find-if-not #'%ftext-p (string name))))
      (when bad
        (format nil "the name holds ~A; a field name is printable ASCII without a colon (RFC 5322 3.6.8)"
                (%char-description bad)))))))

(defun %forbidden-char-problem (value codes)
  (if (not (stringp value))
      (format nil "value ~S is not a string" value)
      (let ((i (position-if (lambda (c) (member (char-code c) codes)) value)))
        (when i
          (let ((char (char value i)))
            (format nil "the value holds ~A at ~D~:[; NUL is not allowed in a header (RFC 2045 2.7)~;; a line break ends the field, so the rest would be read as further fields or the body (header injection)~]"
                    (%char-description char) i (/= 0 (char-code char))))))))

(defun %header-value-problem (value)
  "Why SET-HEADER refuses VALUE (CR, LF or NUL in it), or NIL."
  (%forbidden-char-problem value '(13 10 0)))

(defun %printed-value-problem (value)
  "Why PRINT-MIME cannot write VALUE (CR or LF in it), or NIL."
  (%forbidden-char-problem value '(13 10)))

(defun %read-header-text ()
  (format *query-io* "~&Header text: ")
  (force-output *query-io*)
  (list (read-line *query-io*)))

(defun %checked-header-name (name problem)
  "NAME, unless PROBLEM (a function → reason or NIL) finds fault with it:
then signal MIME-HEADER-ERROR. Restart USE-VALUE takes another name, which
is checked the same way."
  (loop
    (let ((reason (funcall problem name)))
      (unless reason
        (return name))
      (restart-case (error 'mime-header-error :name name :message reason)
        (use-value (new-name)
          :report "Use another header name."
          :interactive %read-header-text
          (setf name new-name))))))

(defun %checked-header-value (name value problem)
  "VALUE of header NAME, unless PROBLEM finds fault with it: then signal
MIME-HEADER-ERROR. Restart USE-VALUE takes another value, which is checked
the same way."
  (loop
    (let ((reason (funcall problem value)))
      (unless reason
        (return value))
      (restart-case (error 'mime-header-error :name name :value value :message reason)
        (use-value (new-value)
          :report "Use another header value."
          :interactive %read-header-text
          (setf value new-value))))))

(defun set-header (entity name value)
  "Set the header field NAME of ENTITY to VALUE, replacing a field of that
name, and return VALUE. NAME is case-insensitive and stored lowercase.
NAME must be RFC 5322 ftext (printable ASCII without a colon) and VALUE a
string without CR, LF or NUL: a line break in a value would end the field
and let the rest of it forge further fields or start the body (header
injection). Either problem signals MIME-HEADER-ERROR with a USE-VALUE
restart. A long value needs no line breaks of its own: PRINT-MIME folds it."
  (let* ((name (%checked-header-name name #'%header-name-problem))
         (value (%checked-header-value name value #'%header-value-problem))
         (key (string-downcase name))
         (cell (assoc key (mime-headers entity) :test #'string=)))
    (if cell
        (setf (cdr cell) value)
        (setf (mime-headers entity) (nconc (mime-headers entity) (list (cons key value)))))
    value))

(defun %crlf-p (octets i)
  (and (< (1+ i) (length octets))
       (= (aref octets i) 13)
       (= (aref octets (1+ i)) 10)))

(defun %lf-p (octets i)
  (and (< i (length octets)) (= (aref octets i) 10)))

(defun %find-header-break (octets)
  "→ (values header-end body-start) or NIL. An entity that starts with a
blank line has no headers (a multipart body part may omit them)."
  (cond
   ((%crlf-p octets 0) (values 0 2))
   ((%lf-p octets 0) (values 0 1))
   (t
    (loop for i from 0 below (length octets)
          do (cond
              ((and (%crlf-p octets i)
                    (< (+ i 3) (length octets))
                    (%crlf-p octets (+ i 2)))
               (return (values i (+ i 4))))
              ((and (%lf-p octets i)
                    (< (1+ i) (length octets))
                    (%lf-p octets (1+ i)))
               (return (values i (+ i 2)))))))))

(defun parse-headers (source)
  "Parse RFC 5322-ish headers. SOURCE is a string. → alist of (lowercase-name . value)."
  (let ((lines '())
        (start 0)
        (n (length source)))
    (loop while (< start n)
          do (let ((end (or (position #\Newline source :start start) n)))
               (let* ((raw (subseq source start end))
                      (line (if (and (plusp (length raw))
                                     (char= (char raw (1- (length raw))) #\Return))
                                (subseq raw 0 (1- (length raw)))
                                raw)))
                 (cond
                   ((zerop (length line))
                    (return))
                   ((and lines (or (char= (char line 0) #\Space)
                                   (char= (char line 0) #\Tab)))
                    (setf (first lines)
                          (concatenate 'string (first lines) " "
                                       (string-left-trim '(#\Space #\Tab) line))))
                   (t (push line lines))))
               (setf start (1+ end))))
    (let ((headers '()))
      (dolist (line (nreverse lines) headers)
        (let ((colon (position #\: line)))
          (unless colon
            (error 'mime-parse-error :message (format nil "header missing ':': ~S" line)))
          (let ((name (string-downcase (string-trim '(#\Space #\Tab) (subseq line 0 colon))))
                (value (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
            (setf headers (nconc headers (list (cons name value))))))))))

(defun %octets-to-ascii (octets)
  (encoding-protocol:decode octets :encoding :iso-8859-1))

(defun %ascii-to-octets (string)
  (encoding-protocol:encode string :encoding :iso-8859-1))

(defun %header-field-text (value)
  "VALUE holds one header field's octets, read as ISO-8859-1. A field whose
octets are valid UTF-8 is UTF-8 text: browsers send multipart/form-data
part headers that way (RFC 7578 §5.1.3), and so does RFC 6532 mail. Any
other field keeps the ISO-8859-1 reading, one character per octet."
  (if (every (lambda (c) (< (char-code c) 128)) value)
      value
      (handler-case (encoding-protocol:decode-utf-8-strict (%ascii-to-octets value))
        (encoding-protocol:encoding-decode-error () value))))

(defun %parse-header-octets (octets)
  "PARSE-HEADERS over raw header OCTETS, each field value read by
%HEADER-FIELD-TEXT."
  (mapcar (lambda (header)
            (cons (car header) (%header-field-text (cdr header))))
          (parse-headers (%octets-to-ascii octets))))

(defun %search-octets (pattern haystack &optional (start 0))
  (let ((plen (length pattern))
        (hlen (length haystack)))
    (loop for i from start to (- hlen plen)
          when (loop for j from 0 below plen
                     always (= (aref pattern j) (aref haystack (+ i j))))
            do (return i))))

(defun %boundary-octets (boundary &optional body)
  "The octets of \"--BOUNDARY\" in BODY. RFC 2046 boundaries are ASCII; a
non-ASCII one is UTF-8 when PRINT-MIME wrote it, or one octet per character
when the Content-Type field was not UTF-8. With BODY, the form that starts
a line of BODY as a delimiter is used, else the UTF-8 one."
  (let ((utf-8 (concatenate '(vector (unsigned-byte 8)) #(45 45)
                            (encoding-protocol:encode boundary :encoding :utf-8)))
        (latin-1 (and (every (lambda (c) (< (char-code c) 256)) boundary)
                      (%ascii-to-octets (format nil "--~A" boundary)))))
    (cond
     ((or (null latin-1) (equalp latin-1 utf-8) (null body)) utf-8)
     ((%first-delimiter body utf-8) utf-8)
     ((%first-delimiter body latin-1) latin-1)
     (t utf-8))))

(defun %eol-after-boundary (body i)
  "Skip transport-padding and the line ending after a dash-boundary. → index."
  (loop while (and (< i (length body))
                   (or (= (aref body i) 32) (= (aref body i) 9)))
        do (incf i))
  (cond
    ((%crlf-p body i) (+ i 2))
    ((%lf-p body i) (1+ i))
    (t i)))

(defun %close-boundary-p (body i dash)
  (let ((after (+ i (length dash))))
    (and (<= (+ after 2) (length body))
         (= (aref body after) 45)
         (= (aref body (1+ after)) 45))))

(defun %delimiter-end-p (body i)
  "Does a dash-boundary that ends before I really end there? It must be
followed by \"--\", transport padding, a line break or the end of BODY, so
that \"--b\" is not found inside \"--b2\"."
  (or (>= i (length body))
      (member (aref body i) '(45 32 9 13 10))))

(defun %next-delimiter (body dash start)
  "Find the delimiter that ends the part starting at START. DASH is the
octets of \"--boundary\".
RFC 2046 §5.1.1: the CRLF before \"--boundary\" belongs to the delimiter,
not to the part, so a part whose content ends in CRLF keeps it. A bare LF
is accepted the same way, and a \"--boundary\" with no line break before it
as a last resort. → (values part-end dash-start) or NIL."
  (let ((lf-dash (concatenate '(vector (unsigned-byte 8)) #(10) dash)))
    (loop for from = start then (1+ lf)
          for lf = (%search-octets lf-dash body from)
          while lf
          when (%delimiter-end-p body (+ lf (length lf-dash)))
            do (return-from %next-delimiter
                 (values (if (and (> lf start) (= 13 (aref body (1- lf))))
                             (1- lf)
                             lf)
                         (1+ lf))))
    (loop for from = start then (1+ bare)
          for bare = (%search-octets dash body from)
          while bare
          when (%delimiter-end-p body (+ bare (length dash)))
            do (return (values bare bare)))))

(defun %first-delimiter (body dash)
  "Where the first dash-boundary DASH starts a line of BODY, or NIL."
  (loop for from = 0 then (1+ at)
        for at = (%search-octets dash body from)
        while at
        when (and (or (zerop at) (= 10 (aref body (1- at))))
                  (%delimiter-end-p body (+ at (length dash))))
          return at))

(defun %split-multipart (body boundary)
  "→ (values preamble parts-octets-list epilogue)"
  (let* ((dash (%boundary-octets boundary body))
         (start (%first-delimiter body dash)))
    (unless start
      (error 'mime-parse-error :message "multipart body missing boundary"))
    (let ((preamble (when (plusp start)
                      ;; The CRLF before the first delimiter is the delimiter's.
                      (let ((end (if (and (>= start 2) (%crlf-p body (- start 2)))
                                     (- start 2)
                                     (1- start))))
                        (when (plusp end) (subseq body 0 end)))))
          (parts '())
          (i start))
      (loop
        ;; I is at a "--boundary" dash-boundary.
        (unless (and (<= (+ i (length dash)) (length body))
                     (eql (%search-octets dash body i) i))
          (error 'mime-parse-error :message "multipart boundary desync"))
        (when (%close-boundary-p body i dash)
          (let ((after (%eol-after-boundary body (+ i (length dash) 2))))
            (return (values preamble (nreverse parts)
                            (when (< after (length body)) (subseq body after))))))
        (let ((content-start (%eol-after-boundary body (+ i (length dash)))))
          (multiple-value-bind (part-end next-dash)
              (%next-delimiter body dash content-start)
            (unless part-end
              (error 'mime-parse-error :message "truncated multipart part"))
            (push (subseq body content-start part-end) parts)
            (setf i next-dash)))))))

(defun %apply-headers (entity)
  (let ((ct (header-value entity "content-type"))
        (cd (header-value entity "content-disposition"))
        (te (header-value entity "content-transfer-encoding")))
    (when ct
      (setf (mime-content-type entity) (parse-media-type ct)))
    (when cd
      (setf (mime-content-disposition entity) (parse-content-disposition cd)))
    (when te
      (setf (mime-transfer-encoding entity) (normalize-transfer-encoding te)))
    entity))

(defun parse-mime (source &key (decode t))
  "Parse SOURCE (string, octets, or stream) as a MIME entity. A header field
whose octets are valid UTF-8 is read as UTF-8, any other as ISO-8859-1."
  (let ((octets (etypecase source
                  ((vector (unsigned-byte 8)) source)
                  (string (%ascii-to-octets source))
                  (stream (let ((buf (make-array 4096 :element-type '(unsigned-byte 8)
                                                      :adjustable t :fill-pointer 0)))
                            (loop with tmp = (make-array 4096 :element-type '(unsigned-byte 8))
                                  for n = (read-sequence tmp source)
                                  do (loop for i from 0 below n
                                           do (vector-push-extend (aref tmp i) buf))
                                  until (< n 4096))
                            (coerce buf '(vector (unsigned-byte 8))))))))
    (multiple-value-bind (header-end body-start)
        (%find-header-break octets)
      (let* ((header-octets (if header-end (subseq octets 0 header-end) octets))
             (body (if body-start (subseq octets body-start) #()))
             (entity (make-instance 'mime-entity
                                    :headers (%parse-header-octets header-octets))))
        (%apply-headers entity)
        (if (multipart-p entity)
            (let ((boundary (and (mime-content-type entity)
                                 (media-type-boundary (mime-content-type entity)))))
              (unless boundary
                (error 'mime-parse-error :message "multipart missing boundary"))
              (multiple-value-bind (preamble parts epilogue)
                  (%split-multipart body boundary)
                (setf (mime-preamble entity) preamble
                      (mime-epilogue entity) epilogue
                      (mime-parts entity)
                      (mapcar (lambda (p) (parse-mime p :decode decode)) parts))))
            (setf (mime-content entity)
                  (if decode
                      (decode-content body (mime-transfer-encoding entity))
                      body)))
        entity))))

(defparameter +header-name-acronyms+
  '("MIME" "ID" "MD5" "DKIM" "ARC" "SPF" "IP")
  "Header name words printed in capitals: MIME-Version, Message-ID,
Content-MD5, DKIM-Signature, ARC-Seal, Received-SPF, X-Originating-IP.")

(defun canonical-header-name (name)
  "NAME as mail and HTTP software print it: each hyphen-separated word
capitalized, acronyms in capitals. \"mime-version\" → \"MIME-Version\",
\"content-transfer-encoding\" → \"Content-Transfer-Encoding\"."
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let* ((end (or (position #\- name :start start) (length name)))
               (word (subseq name start end))
               (acronym (find word +header-name-acronyms+ :test #'string-equal)))
          (write-string (or acronym (string-capitalize word)) out)
          (when (>= end (length name))
            (return))
          (write-char #\- out)
          (setf start (1+ end)))))))

(defun %write-crlf (stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))

(defun %ensure-content-type-header (entity)
  (when (mime-content-type entity)
    (set-header entity "content-type" (format-media-type (mime-content-type entity))))
  (when (mime-content-disposition entity)
    (set-header entity "content-disposition"
                (format-content-disposition (mime-content-disposition entity))))
  (when (and (mime-transfer-encoding entity)
             (not (eq (mime-transfer-encoding entity) :7bit)))
    (set-header entity "content-transfer-encoding"
                (string-downcase (symbol-name (mime-transfer-encoding entity))))))

(defun %octet-string (string)
  "STRING the way PRINT-MIME writes header text: one character per UTF-8
octet. ASCII text comes back as it is."
  (if (every (lambda (c) (< (char-code c) 128)) string)
      string
      (%octets-to-ascii (encoding-protocol:encode string :encoding :utf-8))))

(defun %octet-text (source)
  "Preamble or epilogue SOURCE, octets (as parsed) or a string, one
character per octet."
  (if (stringp source)
      (%octet-string source)
      (%octets-to-ascii source)))

(defun %fold-point-p (line i)
  "Can LINE be folded before index I? Only before a single space between
two non-blank characters: unfolding (RFC 5322 2.2.3) then gives LINE back,
also through PARSE-HEADERS, and no folded line is blank."
  (and (< 0 i (1- (length line)))
       (char= (char line i) #\Space)
       (not (%ows-p (char line (1- i))))
       (not (%ows-p (char line (1+ i))))))

(defun %fold-header-line (line start limit)
  "LINE, one header field without its CRLF, as a list of lines of at most
LIMIT characters, cut before spaces at or after START (RFC 5322 2.2.3).
Each line after the first starts with the space it was cut before. A run
with no place to cut stays longer than LIMIT. LIMIT NIL: LINE as it is."
  (if (or (null limit) (<= (length line) limit))
      (list line)
      (let ((lines '())
            (from 0)
            (fit nil))
        (flet ((cut (at)
                 (push (subseq line from at) lines)
                 (setf from at
                       fit nil)))
          (loop for i from start below (length line)
                when (%fold-point-p line i)
                  do (loop while (> (- i from) limit)
                           do (if fit
                                  (cut fit)
                                  (progn (cut i) (return))))
                     (when (> i from)
                       (setf fit i)))
          (loop while (and fit (> (- (length line) from) limit))
                do (cut fit)))
        (push (subseq line from) lines)
        (nreverse lines))))

(defun %header-field-lines (name value max-line-length)
  "The header field NAME: VALUE as PRINT-MIME writes it, as a list of lines
without CRLF: NAME must be ftext and VALUE hold no CR or LF (see
%HEADER-NAME-PROBLEM, %PRINTED-VALUE-PROBLEM), the text is UTF-8 octets,
folded to MAX-LINE-LENGTH inside the value (never before its first word,
which would leave the name alone on a line and shorten nothing). With a
MAX-LINE-LENGTH, a line that stays longer than RFC 5322's 998 (2.1.1, a
MUST) for want of a space signals MIME-HEADER-ERROR: a relay would break
or refuse it. Each problem has a USE-VALUE restart."
  (let* ((name (%checked-header-name name #'%header-name-problem))
         (printed-name (canonical-header-name name)))
    (loop
      (setf value (%checked-header-value name value #'%printed-value-problem))
      (let* ((lines (%fold-header-line (concatenate 'string printed-name ": " (%octet-string value))
                                       (+ 2 (length printed-name))
                                       max-line-length))
             (long (and max-line-length
                        (find-if (lambda (line) (> (length line) 998)) lines))))
        (unless long
          (return lines))
        (restart-case
            (error 'mime-header-error
                   :name name :value value
                   :message (format nil "a line of ~D octets has no space to fold it within RFC 5322's 998 (2.1.1); a relay would break or refuse it"
                                    (length long)))
          (use-value (new-value)
            :report "Use another header value."
            :interactive %read-header-text
            (setf value new-value)))))))

(defun %delimiter-line-p (text boundary)
  "Does TEXT hold a line that starts with --BOUNDARY as a delimiter, so that
a parser would end a part there? A line starts after LF or CR, or at 0."
  (let ((dash (concatenate 'string "--" boundary)))
    (loop for at = (search dash text) then (search dash text :start2 (1+ at))
          while at
            thereis (and (or (zerop at)
                             (member (char text (1- at)) '(#\Newline #\Return)))
                         (let ((after (+ at (length dash))))
                           (or (>= after (length text))
                               (member (char text after)
                                       '(#\- #\Space #\Tab #\Return #\Newline))))))))

(defun %with-boundary (media-type boundary)
  "A copy of MEDIA-TYPE whose boundary parameter is BOUNDARY."
  (make-media-type (media-type-type media-type)
                   (media-type-subtype media-type)
                   (if (media-type-boundary media-type)
                       (mapcar (lambda (p)
                                 (if (string= "boundary" (car p))
                                     (cons "boundary" boundary)
                                     p))
                               (media-type-parameters media-type))
                       (append (media-type-parameters media-type)
                               (list (cons "boundary" boundary))))))

(defun %printed-parts (entity max-line-length)
  "The parts of multipart ENTITY, printed. Neither its preamble nor a part
may hold a delimiter line of its boundary: the part would end there and the
rest be read as further parts with their own headers. Else signal
MIME-BOUNDARY-ERROR; restart USE-VALUE takes another boundary. → strings."
  (loop
    (let ((boundary (and (mime-content-type entity)
                         (media-type-boundary (mime-content-type entity)))))
      (unless boundary
        (error 'mime-encode-error :message "multipart encode needs a boundary"))
      (let* ((printed (mapcar (lambda (part)
                                (print-mime part :max-line-length max-line-length))
                              (mime-parts entity)))
             (dash (%octet-string boundary))
             (clash (cond
                     ((and (mime-preamble entity)
                           (%delimiter-line-p (%octet-text (mime-preamble entity)) dash))
                      "the preamble")
                     (t
                      (let ((i (position-if (lambda (text) (%delimiter-line-p text dash))
                                            printed)))
                        (and i (format nil "part ~D" i)))))))
        (unless clash
          (return printed))
        (restart-case
            (error 'mime-boundary-error
                   :boundary boundary
                   :message (format nil "~A holds the delimiter line --~A, which would end it early"
                                    clash boundary))
          (use-value (new-boundary)
            :report "Use another boundary."
            :interactive %read-header-text
            (setf (mime-content-type entity)
                  (%with-boundary (mime-content-type entity) new-boundary))))))))

(defun print-mime (entity &key stream (max-line-length 998))
  "Serialize ENTITY. Returns a string unless STREAM is supplied.

The text is octets, one character per octet (ISO-8859-1): body octets as
they are and header text as UTF-8 (RFC 6532), so the string is written to
the wire through ISO-8859-1 (or use PRINT-MIME-OCTETS) and PARSE-MIME reads
it back whatever the header text.

Header names are printed with CANONICAL-HEADER-NAME (the model keeps them
lowercase). A header line longer than MAX-LINE-LENGTH octets is folded
before a space (RFC 5322 2.2.3); the default 998 is RFC 5322 2.1.1's limit,
mail passes 78, and NIL never folds (HTTP has no line limit). A field that
would forge others or break the header block, a value holding CR or LF, a
name that is not ftext, or a line that stays over 998 for want of a space,
signals MIME-HEADER-ERROR (restart USE-VALUE). A multipart whose preamble or
part holds its own delimiter line signals MIME-BOUNDARY-ERROR (restart
USE-VALUE takes another boundary). Everything, the body's encoding
included, is checked before anything is written to STREAM."
  (check-type entity mime-entity)
  (check-type max-line-length (or null (integer 1)))
  (let ((parts (when (multipart-p entity)
                 (%printed-parts entity max-line-length))))
    (%ensure-content-type-header entity)
    (let ((lines (loop for (name . value) in (mime-headers entity)
                       append (%header-field-lines name value max-line-length)))
          (body (unless (multipart-p entity)
                  (let ((wired (encode-content (or (mime-content entity) #())
                                               (mime-transfer-encoding entity))))
                    (if (stringp wired) wired (%octets-to-ascii wired))))))
      (flet ((emit (out)
               (dolist (line lines)
                 (write-string line out)
                 (%write-crlf out))
               (%write-crlf out)
               (if (multipart-p entity)
                   (let ((boundary (%octet-string (media-type-boundary (mime-content-type entity)))))
                     (when (mime-preamble entity)
                       (write-string (%octet-text (mime-preamble entity)) out)
                       (%write-crlf out))
                     (dolist (part parts)
                       (write-string "--" out)
                       (write-string boundary out)
                       (%write-crlf out)
                       (write-string part out)
                       (%write-crlf out))
                     (write-string "--" out)
                     (write-string boundary out)
                     (write-string "--" out)
                     (%write-crlf out)
                     (when (mime-epilogue entity)
                       (write-string (%octet-text (mime-epilogue entity)) out)))
                   (write-string body out))))
        (if stream
            (progn (emit stream) (values))
            (with-output-to-string (o)
              (emit o)))))))

(defun print-mime-octets (entity &key (max-line-length 998))
  "PRINT-MIME's text of ENTITY as the octets that go on the wire."
  (%ascii-to-octets (print-mime entity :max-line-length max-line-length)))

(defun make-text-entity (text &key (subtype "plain") (charset "utf-8")
                                disposition-type name filename)
  (let ((entity (make-instance 'mime-entity
                               :content-type (make-media-type "text" subtype
                                                              (when charset
                                                                (list (cons "charset" charset))))
                               :content (encoding-protocol:encode text)
                               :transfer-encoding :8bit)))
    (when (or disposition-type name filename)
      (setf (mime-content-disposition entity)
            (make-content-disposition (or disposition-type "inline")
                                      (append (when name (list (cons "name" name)))
                                              (when filename (list (cons "filename" filename)))))))
    entity))

(defun make-binary-entity (octets &key (type "application") (subtype "octet-stream")
                                    disposition-type name filename)
  (let ((entity (make-instance 'mime-entity
                               :content-type (make-media-type type subtype)
                               :content (%as-octets octets)
                               :transfer-encoding :base64)))
    (when (or disposition-type name filename)
      (setf (mime-content-disposition entity)
            (make-content-disposition (or disposition-type "attachment")
                                      (append (when name (list (cons "name" name)))
                                              (when filename (list (cons "filename" filename)))))))
    entity))

(defun make-multipart-entity (parts &key (subtype "mixed") boundary)
  "A multipart/SUBTYPE entity of PARTS. BOUNDARY defaults to 96 random bits
from the OS, so no part content can know it in advance (PRINT-MIME also
refuses a part that holds its delimiter line)."
  (let ((boundary (or boundary
                      (format nil "----cl-stack-~(~36R~)"
                              (random (expt 2 96) (make-random-state t))))))
    (make-instance 'mime-entity
                   :content-type (make-media-type "multipart" subtype
                                                  (list (cons "boundary" boundary)))
                   :parts parts
                   :transfer-encoding :7bit)))

(defun %field-entity (name value)
  (cond
    ((mime-entity-p value) value)
    ((and (consp value) (or (getf value :content) (getf value :filename)))
     (let* ((content (getf value :content))
            (filename (getf value :filename))
            (ct (getf value :content-type))
            (parsed (when ct (parse-media-type ct))))
       (if (or filename (and content (not (stringp content))))
           (make-binary-entity (or content #())
                               :type (if parsed (media-type-type parsed) "application")
                               :subtype (if parsed (media-type-subtype parsed) "octet-stream")
                               :disposition-type "form-data"
                               :name (string name)
                               :filename filename)
           (make-text-entity (if (stringp content) content "")
                             :disposition-type "form-data"
                             :name (string name)
                             :filename filename))))
    ((or (stringp value) (null value))
     (make-text-entity (or value "")
                       :disposition-type "form-data"
                       :name (string name)))
    ((vectorp value)
     (make-binary-entity value
                         :disposition-type "form-data"
                         :name (string name)))
    (t
     (error 'mime-encode-error
            :message (format nil "cannot make form field ~S from ~S" name (type-of value))))))

(defun make-form-data (fields &key boundary)
  "FIELDS is an alist of (name . value) or a hash-table. VALUE is string, octets, plist, or entity."
  (let ((parts (cond
                 ((hash-table-p fields)
                  (let ((acc '()))
                    (maphash (lambda (k v) (push (%field-entity k v) acc)) fields)
                    (nreverse acc)))
                 ((listp fields)
                  (mapcar (lambda (pair)
                            (%field-entity (car pair) (cdr pair)))
                          fields))
                 (t
                  (error 'mime-encode-error :message "make-form-data expects an alist or hash-table")))))
    (make-multipart-entity parts :subtype "form-data" :boundary boundary)))

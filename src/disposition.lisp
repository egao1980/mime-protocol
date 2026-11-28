(in-package #:mime-protocol)

;;; RFC 6266 Content-Disposition. filename* is RFC 5987 (charset ' lang ' value).

(defclass content-disposition ()
  ((type :initarg :type :accessor content-disposition-type)
   (parameters :initarg :parameters :accessor content-disposition-parameters :initform nil))
  (:documentation "Content-Disposition. TYPE is lowercase (inline/attachment/form-data)."))

(defun content-disposition-p (object)
  (typep object 'content-disposition))

(defun make-content-disposition (type &optional parameters)
  (make-instance 'content-disposition
                 :type (string-downcase type)
                 :parameters (mapcar (lambda (p)
                                       (cons (string-downcase (car p)) (cdr p)))
                                     parameters)))

(defun %hex-digit-p (char)
  (or (digit-char-p char)
      (find char "abcdefABCDEF" :test #'char=)))

(defun %hex-byte (a b)
  (let ((hi (digit-char-p a 16))
        (lo (digit-char-p b 16)))
    (unless (and hi lo)
      (error 'mime-parse-error :message "invalid percent-encoding"))
    (+ (* hi 16) lo)))

(defun %decode-rfc5987-value (encoded &optional (encoding :utf-8))
  "The %XX-encoded octets of ENCODED decoded in ENCODING (an
encoding-protocol charset, RFC 8187 3.2.1). Octets that are not text in ENCODING signal
MIME-PARSE-ERROR."
  (let ((octets (make-array (length encoded) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with i = 0
          while (< i (length encoded))
          do (let ((c (char encoded i)))
               (cond
                 ((char= c #\%)
                  (when (> (+ i 3) (length encoded))
                    (error 'mime-parse-error :message "truncated percent-encoding"))
                  (vector-push (%hex-byte (char encoded (1+ i)) (char encoded (+ i 2))) octets)
                  (incf i 3))
                 ((> (char-code c) 255)
                  (error 'mime-parse-error
                         :message (format nil "U+~4,'0X in an RFC 8187 ext-value" (char-code c))))
                 (t
                  (vector-push (char-code c) octets)
                  (incf i)))))
    (handler-case
        (encoding-protocol:decode (coerce octets '(vector (unsigned-byte 8))) :encoding encoding)
      (encoding-protocol:encoding-error (c)
        (error 'mime-parse-error
               :message (format nil "RFC 8187 ext-value is not ~(~A~): ~A" encoding c))))))

(defun %parse-rfc5987 (value)
  "charset ' [lang] ' value-chars → decoded string, in any charset
encoding-protocol knows (RFC 8187 requires UTF-8 and ISO-8859-1; RFC 2231
mail uses others). us-ascii is read as UTF-8, its superset. An unknown
charset signals MIME-PARSE-ERROR."
  (let ((q1 (position #\' value)))
    (unless q1
      (return-from %parse-rfc5987 value))
    (let ((q2 (position #\' value :start (1+ q1))))
      (unless q2
        (return-from %parse-rfc5987 value))
      (let* ((charset (subseq value 0 q1))
             (encoding (%charset-encoding charset))
             (encoded (subseq value (1+ q2))))
        (unless encoding
          (error 'mime-parse-error
                 :message (format nil "unsupported RFC 5987 charset ~S" charset)))
        (%decode-rfc5987-value encoded (if (eq encoding :ascii) :utf-8 encoding))))))

(defparameter +form-data-escapes+
  '(("%22" . #\") ("%0D" . #\Return) ("%0A" . #\Newline))
  "The escapes a browser writes in a form-data name or filename, and the
   characters they stand for. %FORM-DATA-ESCAPE writes the same three.")

(defun %form-data-name-parameter-p (type name)
  "Is NAME the name or filename parameter of a TYPE form-data disposition?"
  (and (string= type "form-data")
       (member name '("name" "filename") :test #'string-equal)))

(defun %form-data-unescape (string)
  "STRING with %22 %0D %0A decoded back to \" CR LF. Any other % is kept."
  (with-output-to-string (o)
    (let ((i 0)
          (n (length string)))
      (loop while (< i n)
            do (let ((escape (and (char= (char string i) #\%)
                                  (<= (+ i 3) n)
                                  (assoc (subseq string i (+ i 3)) +form-data-escapes+
                                         :test #'string=))))
                 (cond
                  (escape
                   (write-char (cdr escape) o)
                   (incf i 3))
                  (t
                   (write-char (char string i) o)
                   (incf i))))))))

(defun %read-form-data-value (string start)
  "A form-data name or filename value at START: a quoted string ends at the
   next \", a backslash in it is literal, and %22 %0D %0A decode to \" CR LF.
   A bare token is read as is. → (values value end)."
  (if (and (< start (length string)) (char= (char string start) #\"))
      (let ((close (position #\" string :start (1+ start))))
        (unless close
          (error 'mime-parse-error :message "unterminated quoted-string"))
        (values (%form-data-unescape (subseq string (1+ start) close))
                (1+ close)))
      (%read-token string start)))

(defun %continuation-name (name)
  "NAME as an RFC 2231 continuation (\"filename*1\", \"filename*0*\"):
→ (values base index encoded-p), or NIL."
  (let* ((encoded-p (and (> (length name) 1) (char= #\* (char name (1- (length name))))))
         (body (if encoded-p (subseq name 0 (1- (length name))) name))
         (star (position #\* body :from-end t)))
    (when (and star
               (plusp star)
               (< (1+ star) (length body))
               (every #'digit-char-p (subseq body (1+ star))))
      (values (subseq body 0 star)
              (parse-integer body :start (1+ star))
              encoded-p))))

(defun %attr-encode (text)
  "TEXT's UTF-8 octets with every octet outside attr-char as %XX."
  (with-output-to-string (o)
    (loop for b across (encoding-protocol:encode text :encoding :utf-8)
          do (if (and (< b 128) (%attr-char-p (code-char b)))
                 (write-char (code-char b) o)
                 (format o "%~2,'0X" b)))))

(defun %join-segments (base segments)
  "SEGMENTS, (index encoded-p value) in order, as one parameter: BASE* with
an ext-value when a segment is encoded (plain ones %-encoded into it), else
BASE with the values joined."
  (if (notany #'second segments)
      (cons base (format nil "~{~A~}" (mapcar #'third segments)))
      (cons (concatenate 'string base "*")
            (with-output-to-string (o)
              (loop for (index encoded-p value) in segments
                    do (cond
                         (encoded-p (write-string value o))
                         ((zerop index)
                          (write-string "utf-8''" o)
                          (write-string (%attr-encode value) o))
                         (t (write-string (%attr-encode value) o))))))))

(defun %join-continuations (parameters)
  "PARAMETERS with each set of RFC 2231 continuations (NAME*0..NAME*n, any
of them NAME*k* for an encoded one) joined into one parameter at the place
of its first segment. A set that does not run 0, 1, ... without a gap or a
repeat is kept as it came."
  (let ((sets (make-hash-table :test #'equal)))
    (loop for (name . value) in parameters
          do (multiple-value-bind (base index encoded-p) (%continuation-name name)
               (when base
                 (push (list index encoded-p value) (gethash base sets)))))
    (if (zerop (hash-table-count sets))
        parameters
        (let ((joined '())
              (out '()))
          (dolist (parameter parameters (nreverse out))
            (let ((base (%continuation-name (car parameter))))
              (if (null base)
                  (push parameter out)
                  (let ((segments (sort (copy-list (gethash base sets)) #'< :key #'first)))
                    (cond
                      ((member base joined :test #'string=))
                      ((loop for segment in segments
                             for i from 0
                             always (= i (first segment)))
                       (push (%join-segments base segments) out)
                       (push base joined))
                      (t (push parameter out)))))))))))

(defun %form-data-name-problem (parameters)
  "Why form-data PARAMETERS would name a different field or file to
different servers, or NIL: a repeated name, filename or filename*.
python-multipart and Werkzeug keep the last one, an assoc the first."
  (flet ((repeated-p (name)
           (< 1 (count name parameters :key #'car :test #'string=))))
    (let ((name (find-if #'repeated-p '("name" "filename" "filename*"))))
      (and name (format nil "a repeated ~A parameter" name)))))

(defun parse-content-disposition (source)
  "Parse a Content-Disposition value (RFC 6266). Quoted parameter values
   take RFC 822 quoted-pairs, except name and filename in a form-data
   disposition, which are read the way browsers write them (see
   %READ-FORM-DATA-VALUE). RFC 2231 continuations (filename*0*=...;
   filename*1*=...) are joined into one parameter, except in a form-data
   disposition: RFC 7578 4.2 has no continuations there, and python-multipart
   does not join them, so a name*0 / filename*0 must not override the plain
   name or filename the server reads. Signals MIME-PARSE-ERROR."
  (let* ((string (string-trim '(#\Space #\Tab #\Return #\Newline) source))
         (i 0)
         type parameters)
    (when (zerop (length string))
      (error 'mime-parse-error :message "empty Content-Disposition"))
    (multiple-value-bind (tok next) (%read-token string i)
      (setf type (string-downcase tok) i next))
    (loop
      (setf i (%skip-ows string i))
      (when (>= i (length string))
        (return))
      (unless (char= (char string i) #\;)
        (error 'mime-parse-error :message "expected ';' in Content-Disposition"))
      (setf i (%skip-ows string (1+ i)))
      (when (>= i (length string))
        (return))
      (multiple-value-bind (name next) (%read-token string i)
        (setf i next)
        (unless (and (< i (length string)) (char= (char string i) #\=))
          (error 'mime-parse-error :message (format nil "parameter ~A missing '='" name)))
        (incf i)
        (multiple-value-bind (value next)
            (if (%form-data-name-parameter-p type name)
                (%read-form-data-value string i)
                (%read-parameter-value string i))
          (setf i next)
          (push (cons (string-downcase name) value) parameters))))
    (setf parameters (nreverse parameters))
    (make-instance 'content-disposition
                   :type type
                   :parameters (if (string= type "form-data")
                                   (let ((problem (%form-data-name-problem parameters)))
                                     (when problem
                                       (error 'mime-parse-error
                                              :message (format nil "form-data Content-Disposition with ~A"
                                                               problem)))
                                     parameters)
                                   (%join-continuations parameters)))))

(defun %attr-char-p (char)
  "RFC 8187 attr-char: ALPHA / DIGIT / ! # $ & + - . ^ _ ` | ~"
  (and (< (char-code char) 128)
       (or (alphanumericp char)
           (find char "!#$&+-.^_`|~" :test #'char=))))

(defun %ascii-p (string)
  (every (lambda (c) (< (char-code c) 128)) string))

(defun encode-rfc5987-value (string)
  "STRING as an RFC 8187 (RFC 5987) ext-value: UTF-8''<percent-encoded octets>.
   Every octet outside attr-char is percent-encoded, so the result is plain
   ASCII and contains no quote, CR or LF."
  (with-output-to-string (o)
    (write-string "UTF-8''" o)
    (loop for b across (encoding-protocol:encode string :encoding :utf-8)
          for c = (code-char b)
          do (if (and (< b 128) (%attr-char-p c))
                 (write-char c o)
                 (format o "%~2,'0X" b)))))

(defun %ext-value-p (value)
  "Already an RFC 8187 ext-value (charset'language'attr-chars / %XX)? The
language is a tag (letters, digits, hyphens), so a caller's \"filename*\"
cannot carry \"; name=...\" in it and add parameters."
  (let* ((q1 (position #\' value))
         (q2 (and q1 (position #\' value :start (1+ q1)))))
    (and q2
         (plusp q1)
         (every #'%attr-char-p (subseq value 0 q1))
         (every (lambda (c) (or (char= c #\-) (and (< (char-code c) 128) (alphanumericp c))))
                (subseq value (1+ q1) q2))
         (every (lambda (c) (or (%attr-char-p c) (char= c #\%)))
                (subseq value (1+ q2))))))

(defparameter +continuation-length+ 60
  "Characters of an RFC 2231 continuation segment, so that \"; filename*N*=\"
and the segment fit on a folded 78-character line.")

(defun %ext-units (ext)
  "EXT, an RFC 8187 ext-value, as the pieces a continuation may not cut:
the charset'language' prefix, a character, or one character's %XX
octets (a UTF-8 lead octet and its continuation octets)."
  (let* ((q2 (position #\' ext :start (1+ (or (position #\' ext) -1))))
         (units (list (subseq ext 0 (1+ q2))))
         (i (1+ q2))
         (n (length ext)))
    (flet ((continuation-at-p (j)
             (and (< (+ j 2) n)
                  (char= #\% (char ext j))
                  (let ((octet (parse-integer ext :start (1+ j) :end (+ j 3) :radix 16
                                                  :junk-allowed t)))
                    (and octet (<= #x80 octet #xBF))))))
      (loop while (< i n)
            do (let ((end (if (and (char= #\% (char ext i)) (<= (+ i 3) n))
                              (+ i 3)
                              (1+ i))))
                 (loop while (continuation-at-p end)
                       do (incf end 3))
                 (push (subseq ext i end) units)
                 (setf i end))))
    (nreverse units)))

(defun %ext-segments (ext)
  "EXT cut into RFC 2231 continuation segments of at most
+CONTINUATION-LENGTH+ characters (a longer single unit stands alone),
never inside a character: the first holds the charset'language' prefix."
  (let ((segments '())
        (current ""))
    (dolist (unit (%ext-units ext))
      (when (and (plusp (length current))
                 (> (+ (length current) (length unit)) +continuation-length+))
        (push current segments)
        (setf current ""))
      (setf current (concatenate 'string current unit)))
    (when (plusp (length current))
      (push current segments))
    (nreverse segments)))

(defun %ext-parameter-p (name)
  "Does parameter NAME end in *, so its value is an RFC 8187 ext-value?"
  (and (plusp (length name)) (char= #\* (char name (1- (length name))))))

(defun %write-ext-parameter (o name value type)
  "Write \"; NAME=EXT\", NAME ending in *: VALUE as an RFC 8187 ext-value
(encoded unless it is one already, never quoted). One longer than
+CONTINUATION-LENGTH+ goes out as RFC 2231 continuations, NAME*0*=...;
NAME*1*=..., so the field can be folded (Python's EmailMessage does the
same), except in a form-data disposition: HTTP has no line limit and
servers do not read continuations."
  (let ((ext (if (%ext-value-p value) value (encode-rfc5987-value value))))
    (if (or (<= (length ext) +continuation-length+) (string= type "form-data"))
        (format o "; ~A=~A" name ext)
        (loop for segment in (%ext-segments ext)
              for i from 0
              do (format o "; ~A~D*=~A" name i segment)))))

(defun %write-disposition-parameter (o name value type parameters)
  "Write \"; NAME=VALUE\" of a disposition of TYPE whose PARAMETERS hold it.
A NAME* is an ext-value (%WRITE-EXT-PARAMETER). In a form-data disposition
a plain value is written whole, name and filename quoted the browser way.
Elsewhere, as Python's EmailMessage does, a short ASCII value is written
plain, and a non-ASCII one, or one longer than +CONTINUATION-LENGTH+ that
no fold could shorten, as NAME*=UTF-8''... in its place (RFC 2231),
continued NAME*0*=...; NAME*1*=... when long; none when PARAMETERS hold a
NAME* already, which readers prefer. Raw octets of an unknown charset (a
relayed field, see %MARK-RAW) have no UTF-8 reading: they stay plain, as
quoted NAME*0=\"...\"; NAME*1=\"...\" continuations when long."
  (let ((text (if (%ext-parameter-p name) value (%parameter-text value))))
    (cond
      ((%continuation-name name)
       ;; A segment kept as it came (a set with a gap): written as it is, never
       ;; continued again into NAME*k*0*=...; quoted unless it is ext-value text.
       (format o "; ~A=~A" name
               (if (and (%ext-parameter-p name)
                        (plusp (length value))
                        (every (lambda (c) (or (%attr-char-p c) (find c "%'"))) value))
                   value
                   (%quote-parameter-value text))))
      ((%ext-parameter-p name)
       (%write-ext-parameter o name value type))
      ((string= type "form-data")
       (if (and (%form-data-name-parameter-p type name)
                (not (and (plusp (length text)) (every #'%tchar-p text))))
           ;; Quoted the browser way, as PARSE-CONTENT-DISPOSITION reads it back.
           (format o "; ~A=\"~A\"" name (%form-data-escape text))
           (format o "; ~A=~A" name (%quote-parameter-value text))))
      ((%raw-value-p value)
       (if (<= (length text) +continuation-length+)
           (format o "; ~A=~A" name (%quote-parameter-value text))
           (loop for start from 0 below (length text) by +continuation-length+
                 for i from 0
                 do (format o "; ~A*~D=~A" name i
                            (%quote-parameter-value
                             (subseq text start (min (length text)
                                                     (+ start +continuation-length+))))))))
      ((and (%ascii-p value) (<= (length value) +continuation-length+))
       (format o "; ~A=~A" name (%quote-parameter-value text)))
      ((assoc (concatenate 'string name "*") parameters :test #'string=)
       ;; Checked as the plain value would have been, then left to the NAME*.
       (%check-header-text value))
      (t
       ;; A control is refused here as in a plain value, not hidden as %XX.
       (%check-header-text value)
       (%write-ext-parameter o (concatenate 'string name "*") value type)))))

(defun format-content-disposition (disposition)
  "Serialize DISPOSITION (RFC 6266). Parameter values are quoted as needed;
   a value with CR, LF or another control character signals MIME-ENCODE-ERROR.
   Parameters named NAME* are written as RFC 8187 ext-values, and a long
   ext-value goes out as RFC 2231 continuations (NAME*0*=...; NAME*1*=...).
   Outside form-data a non-ASCII or long plain value goes out as such a
   NAME* in its place, as Python's EmailMessage writes it (see
   %WRITE-DISPOSITION-PARAMETER).
   In a form-data disposition values are never continued, a non-ASCII
   filename is written as UTF-8 and also as filename*=UTF-8''..., and a name
   or filename that needs quotes is quoted the browser way, as
   FORMAT-FORM-DATA-DISPOSITION does: \" CR LF become %22 %0D %0A and a
   backslash stays literal."
  (check-type disposition content-disposition)
  (let ((params (content-disposition-parameters disposition))
        (type (content-disposition-type disposition)))
    (with-output-to-string (o)
      (write-string type o)
      (dolist (p params)
        (%write-disposition-parameter o (car p) (cdr p) type params))
      (let ((filename (cdr (assoc "filename" params :test #'string=))))
        (when (and filename
                   (string= type "form-data")
                   (not (%ascii-p filename))
                   ;; Raw octets of an unknown charset: no UTF-8 reading to add.
                   (not (%raw-value-p filename))
                   (not (assoc "filename*" params :test #'string=)))
          (%write-ext-parameter o "filename*" filename type))))))

(defun %form-data-escape (string)
  "RFC 7578 section 2 / WHATWG multipart/form-data: percent-encode \" CR LF in
   a quoted name or filename, so no value can end the quoted-string or the
   header line. Other characters (including UTF-8) are kept."
  ;; CR and LF are escaped below; any other control character is refused.
  (%check-header-text
   (remove-if (lambda (c) (member c '(#\Return #\Newline))) string))
  (with-output-to-string (o)
    (loop for c across string
          do (case c
               (#\" (write-string "%22" o))
               (#\Return (write-string "%0D" o))
               (#\Newline (write-string "%0A" o))
               (t (write-char c o))))))

(defun format-form-data-disposition (name &key filename)
  "Content-Disposition value for a multipart/form-data part (RFC 7578):
   form-data; name=\"NAME\"[; filename=\"FILENAME\"[; filename*=UTF-8''...]]

   NAME and FILENAME are always quoted, with \" CR LF percent-encoded (the
   browser rule), so they cannot add parameters or headers. A non-ASCII
   FILENAME is sent as UTF-8 in filename (RFC 7578 section 4.2) and also as an
   RFC 8187 filename* for receivers that only read that."
  (check-type name string)
  (check-type filename (or null string))
  (with-output-to-string (o)
    (format o "form-data; name=\"~A\"" (%form-data-escape name))
    (when filename
      (format o "; filename=\"~A\"" (%form-data-escape filename))
      (unless (%ascii-p filename)
        (format o "; filename*=~A" (encode-rfc5987-value filename))))))

(defun disposition-parameter (disposition name)
  (cdr (assoc (string-downcase name) (content-disposition-parameters disposition)
              :test #'string=)))

(defun disposition-name (disposition)
  (disposition-parameter disposition "name"))

(defun disposition-filename (disposition)
  "Prefer filename* (RFC 5987) over filename."
  (let ((star (disposition-parameter disposition "filename*")))
    (if star
        (%parse-rfc5987 star)
        (disposition-parameter disposition "filename"))))

(in-package #:mime-protocol)

;;; RFC 2047 encoded-words: non-ASCII text in mail header fields.
;;; Encoding writes B (base64) words in UTF-8; decoding reads B and Q in
;;; any charset encoding-protocol knows.

(defparameter +encoded-word-octets+ 39
  "Octets of text in one encoded-word. 39 octets are 52 base64 characters,
64 with \"=?utf-8?b?\" and \"?=\": a word fits after a common field name and
on a folded line, inside RFC 2047's 75 per word and 76 per line (section 2).")

(defun %utf-8-length (char)
  (let ((code (char-code char)))
    (cond ((< code #x80) 1)
          ((< code #x800) 2)
          ((< code #x10000) 3)
          (t 4))))

(defun encode-encoded-words (text &key name)
  "All of TEXT as RFC 2047 B encoded-words in UTF-8, separated by single
spaces. Each holds a whole number of characters (section 5) and is at most
64 characters long. A decoder drops the spaces between encoded-words (6.2),
so the result stands for TEXT in a phrase (a display name) or in
unstructured text. \"\" gives \"\". TEXT holding CR, LF or NUL signals
MIME-HEADER-ERROR for header NAME, as SET-HEADER does (restart USE-VALUE):
a line break is refused, not hidden in base64."
  (setf text (%checked-header-value name text #'%header-value-problem))
  (with-output-to-string (o)
    (let ((start 0)
          (octets 0))
      (flet ((word (end)
               (when (plusp start)
                 (write-char #\Space o))
               (format o "=?utf-8?b?~A?="
                       (encoding-protocol:encode
                        (encoding-protocol:encode (subseq text start end) :encoding :utf-8)
                        :encoding :base64))))
        (loop for i from 0 below (length text)
              for n = (%utf-8-length (char text i))
              do (when (> (+ octets n) +encoded-word-octets+)
                   (word i)
                   (setf start i
                         octets 0))
                 (incf octets n))
        (when (< start (length text))
          (word (length text)))))))

(defun %split-blanks (text)
  "TEXT as its runs of blanks (SP, HTAB) and of other characters, in order.
→ list of (blank-p . string)."
  (let ((segments '())
        (start 0)
        (n (length text)))
    (loop while (< start n)
          do (let* ((blank-p (%ows-p (char text start)))
                    (end (or (position-if (if blank-p (complement #'%ows-p) #'%ows-p)
                                          text :start start)
                             n)))
               (push (cons blank-p (subseq text start end)) segments)
               (setf start end)))
    (nreverse segments)))

(defun %encoded-word-needed-p (word)
  "Must WORD of unstructured text go out as an encoded-word? When it holds
a character outside printable ASCII, holds \"=?\" (a decoder could take it
for an encoded-word, and it would not come back as written), or is too long
for a folded line."
  (or (> (length word) 76)
      (search "=?" word)
      (some (lambda (c)
              (let ((code (char-code c)))
                (or (< code 33) (> code 126))))
            word)))

(defun encode-header-text (text &key name)
  "Unstructured header TEXT (RFC 5322 3.2.5: Subject, Comments) ready for
the wire. Each run of words that needs it (non-ASCII or control characters,
\"=?\", or longer than a line), with the blanks inside the run, becomes
encoded-words (RFC 2047 5 (1)). Other words, and the blanks around a run,
stay as they are, so plain ASCII text comes back unchanged.
TEXT holding CR, LF or NUL signals MIME-HEADER-ERROR for header NAME, as
SET-HEADER does (restart USE-VALUE): a line break is refused, not encoded."
  (let ((text (%checked-header-value name text #'%header-value-problem))
        (out (make-string-output-stream))
        (run '())
        (blank nil))
    (flet ((flush ()
             (when run
               (write-string (encode-encoded-words (format nil "~{~A~}" (reverse run))) out)
               (setf run '()))))
      (dolist (segment (%split-blanks text))
        (destructuring-bind (blank-p . string) segment
          (cond
           (blank-p
            (setf blank string))
           ((%encoded-word-needed-p string)
            (if run
                (when blank (push blank run))
                (when blank (write-string blank out)))
            (push string run)
            (setf blank nil))
           (t
            (flush)
            (when blank (write-string blank out))
            (write-string string out)
            (setf blank nil)))))
      (flush)
      (when blank
        (write-string blank out))
      (get-output-stream-string out))))

(defun %charset-encoding (charset)
  "The encoding-protocol name for MIME CHARSET (an RFC 2231 *language
suffix dropped), or NIL when there is none. Never interns a symbol."
  (let* ((name (string-downcase (subseq charset 0 (position #\* charset))))
         (name (cond ((string= name "utf8") "utf-8")
                     ((and (> (length name) 8) (string= "windows-" name :end2 8))
                      (concatenate 'string "cp" (subseq name 8)))
                     (t name)))
         (keyword (and (plusp (length name))
                       (find-symbol (string-upcase name) :keyword))))
    (and keyword
         (let ((encoding (encoding-protocol:normalize-encoding keyword)))
           (and (not (member encoding '(:base64 :base64url :base32 :base32hex :base16
                                        :quoted-printable :rle)))
                encoding)))))

(defun %decode-q (text)
  "RFC 2047 4.2 Q encoding: _ is a space, =XX an octet. → octets, or NIL."
  (let ((out (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0)
        (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                ((char= c #\_)
                 (vector-push 32 out)
                 (incf i))
                ((char= c #\=)
                 (let ((hi (and (< (+ i 2) n) (digit-char-p (char text (+ i 1)) 16)))
                       (lo (and (< (+ i 2) n) (digit-char-p (char text (+ i 2)) 16))))
                   (unless (and hi lo)
                     (return-from %decode-q nil))
                   (vector-push (+ (* 16 hi) lo) out)
                   (incf i 3)))
                ((< 32 (char-code c) 127)
                 (vector-push (char-code c) out)
                 (incf i))
                (t
                 (return-from %decode-q nil)))))
    (coerce out '(vector (unsigned-byte 8)))))

(defun %decode-b (text)
  "RFC 2047 4.1 B encoding (base64). → octets, or NIL."
  (handler-case (let ((octets (encoding-protocol:decode text :encoding :base64)))
                  (and (typep octets '(vector (unsigned-byte 8))) octets))
    (encoding-protocol:encoding-error () nil)))

(defun %token-encoded-words (token)
  "TOKEN, a run without blanks, as one or more encoded-words with nothing
between them. → list of (encoding . octets), or NIL when it is anything else."
  (let ((i 0)
        (n (length token))
        (words '()))
    (loop
      (when (>= i n)
        (return (nreverse words)))
      (unless (and (< (1+ i) n)
                   (char= #\= (char token i))
                   (char= #\? (char token (1+ i))))
        (return nil))
      (let* ((q1 (position #\? token :start (+ i 2)))
             (q2 (and q1 (position #\? token :start (1+ q1))))
             (close (and q2 (search "?=" token :start2 (1+ q2)))))
        (unless (and close (> q1 (+ i 2)) (= q2 (+ q1 2)))
          (return nil))
        (let* ((encoding (%charset-encoding (subseq token (+ i 2) q1)))
               (text (subseq token (1+ q2) close))
               (octets (case (char-upcase (char token (1+ q1)))
                         (#\B (%decode-b text))
                         (#\Q (%decode-q text)))))
          (unless (and encoding octets)
            (return nil))
          (push (cons encoding octets) words)
          (setf i (+ close 2)))))))

(defun %decode-encoded-run (run)
  "RUN, a list of (encoding . octets) in order, as one string: adjacent
octets in one encoding are decoded together. NIL when one does not decode."
  (let ((groups '()))
    (dolist (word run)
      (if (and groups (eq (car (first groups)) (car word)))
          (setf (cdr (first groups))
                (concatenate '(vector (unsigned-byte 8)) (cdr (first groups)) (cdr word)))
          (push (cons (car word) (cdr word)) groups)))
    (handler-case
        (let ((strings (mapcar (lambda (group)
                                 (encoding-protocol:decode (cdr group) :encoding (car group)))
                               (nreverse groups))))
          (and (every #'stringp strings)
               (format nil "~{~A~}" strings)))
      (encoding-protocol:encoding-error () nil))))

(defun decode-header-text (text)
  "TEXT, unstructured header text or a phrase, with its RFC 2047
encoded-words decoded (section 6). Blanks between two encoded-words are
dropped (6.2), and adjacent words in one charset are decoded together, so a
character split across two words survives. A token that is not made of
well-formed encoded-words, or that does not decode, is kept as written."
  (let ((out (make-string-output-stream))
        (run '())
        (run-text '())
        (blank nil))
    (flet ((flush ()
             (when run
               (write-string (or (%decode-encoded-run (reverse run))
                                 (format nil "~{~A~}" (reverse run-text)))
                             out)
               (setf run '()
                     run-text '()))))
      (dolist (segment (%split-blanks text))
        (destructuring-bind (blank-p . string) segment
          (if blank-p
              (setf blank string)
              (let ((words (%token-encoded-words string)))
                (cond
                 (words
                  (if run
                      (when blank (push blank run-text))
                      (when blank (write-string blank out)))
                  (setf run (append (reverse words) run))
                  (push string run-text))
                 (t
                  (flush)
                  (when blank (write-string blank out))
                  (write-string string out)))
                (setf blank nil)))))
      (flush)
      (when blank
        (write-string blank out))
      (get-output-stream-string out))))

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

(defun %decode-rfc5987-value (encoded)
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
                 (t
                  (vector-push (char-code c) octets)
                  (incf i)))))
    (encoding-protocol:decode (coerce octets '(vector (unsigned-byte 8))))))

(defun %parse-rfc5987 (value)
  "charset ' [lang] ' value-chars → decoded string."
  (let ((q1 (position #\' value)))
    (unless q1
      (return-from %parse-rfc5987 value))
    (let ((q2 (position #\' value :start (1+ q1))))
      (unless q2
        (return-from %parse-rfc5987 value))
      (let ((charset (string-downcase (subseq value 0 q1)))
            (encoded (subseq value (1+ q2))))
        (unless (member charset '("utf-8" "utf8" "us-ascii" "iso-8859-1") :test #'string=)
          (error 'mime-parse-error
                 :message (format nil "unsupported RFC 5987 charset ~S" charset)))
        (%decode-rfc5987-value encoded)))))

(defun parse-content-disposition (source)
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
        (multiple-value-bind (value next) (%read-parameter-value string i)
          (setf i next)
          (push (cons (string-downcase name) value) parameters))))
    (make-instance 'content-disposition
                   :type type
                   :parameters (nreverse parameters))))

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
  "Already an RFC 8187 ext-value (charset'lang'attr-chars / %XX)?"
  (let* ((q1 (position #\' value))
         (q2 (and q1 (position #\' value :start (1+ q1)))))
    (and q2
         (plusp q1)
         (every #'%attr-char-p (subseq value 0 q1))
         (every (lambda (c) (or (%attr-char-p c) (char= c #\%)))
                (subseq value (1+ q2))))))

(defun %write-disposition-parameter (o name value)
  (write-string "; " o)
  (write-string name o)
  (write-char #\= o)
  (if (and (plusp (length name)) (char= #\* (char name (1- (length name)))))
      ;; ext-value (RFC 8187): never quoted; encode unless already encoded.
      (write-string (if (%ext-value-p value) value (encode-rfc5987-value value)) o)
      (write-string (%quote-parameter-value value) o)))

(defun format-content-disposition (disposition)
  "Serialize DISPOSITION (RFC 6266). Parameter values are quoted as needed;
   a value with CR, LF or another control character signals MIME-ENCODE-ERROR.
   Parameters named NAME* are written as RFC 8187 ext-values, and a non-ASCII
   filename without a filename* also gets filename*=UTF-8''..."
  (check-type disposition content-disposition)
  (let ((params (content-disposition-parameters disposition)))
    (with-output-to-string (o)
      (write-string (content-disposition-type disposition) o)
      (dolist (p params)
        (%write-disposition-parameter o (car p) (cdr p)))
      (let ((filename (cdr (assoc "filename" params :test #'string=))))
        (when (and filename
                   (not (%ascii-p filename))
                   (not (assoc "filename*" params :test #'string=)))
          (%write-disposition-parameter o "filename*" filename))))))

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

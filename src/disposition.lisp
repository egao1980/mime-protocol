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
    (babel:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :encoding :utf-8)))

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

(defun format-content-disposition (disposition)
  (check-type disposition content-disposition)
  (with-output-to-string (o)
    (write-string (content-disposition-type disposition) o)
    (dolist (p (content-disposition-parameters disposition))
      (write-string "; " o)
      (write-string (car p) o)
      (write-char #\= o)
      (write-string (%quote-parameter-value (cdr p)) o))))

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

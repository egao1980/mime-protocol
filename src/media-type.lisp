(in-package #:mime-protocol)

;;; RFC 2045 / RFC 9110 media-type: type "/" subtype *( OWS ";" OWS parameter )

(defclass media-type ()
  ((type :initarg :type :accessor media-type-type)
   (subtype :initarg :subtype :accessor media-type-subtype)
   (parameters :initarg :parameters :accessor media-type-parameters :initform nil))
  (:documentation "MIME media type. TYPE/SUBTYPE are lowercase; PARAMETERS is an alist of lowercase names."))

(defun media-type-p (object)
  (typep object 'media-type))

(defun make-media-type (type subtype &optional parameters)
  (make-instance 'media-type
                 :type (string-downcase type)
                 :subtype (string-downcase subtype)
                 :parameters (mapcar (lambda (p)
                                       (cons (string-downcase (car p)) (cdr p)))
                                     parameters)))

(defun %ows-p (char)
  (or (char= char #\Space) (char= char #\Tab)))

(defun %skip-ows (string start)
  (loop for i from start below (length string)
        while (%ows-p (char string i))
        finally (return i)))

(defun %tchar-p (char)
  (or (alphanumericp char)
      (find char "!#$%&'*+-.^_`|~" :test #'char=)))

(defun %read-token (string start)
  (let ((end (or (position-if-not #'%tchar-p string :start start) (length string))))
    (when (= end start)
      (error 'mime-parse-error :message (format nil "expected token at ~D" start)))
    (values (subseq string start end) end)))

(defun %read-quoted-string (string start)
  (unless (and (< start (length string)) (char= (char string start) #\"))
    (error 'mime-parse-error :message "expected quoted-string"))
  (let ((out (make-string-output-stream))
        (i (1+ start)))
    (loop
      (when (>= i (length string))
        (error 'mime-parse-error :message "unterminated quoted-string"))
      (let ((c (char string i)))
        (cond
          ((char= c #\\)
           (incf i)
           (when (>= i (length string))
             (error 'mime-parse-error :message "truncated quoted-pair"))
           (write-char (char string i) out)
           (incf i))
          ((char= c #\")
           (return (values (get-output-stream-string out) (1+ i))))
          (t
           (write-char c out)
           (incf i)))))))

(defun %read-parameter-value (string start)
  (if (and (< start (length string)) (char= (char string start) #\"))
      (%read-quoted-string string start)
      (%read-token string start)))

(defun parse-media-type (source)
  "Parse a Content-Type / media-type string. Signals MIME-PARSE-ERROR."
  (let* ((string (string-trim '(#\Space #\Tab #\Return #\Newline) source))
         (i 0)
         type subtype parameters)
    (when (zerop (length string))
      (error 'mime-parse-error :message "empty media type"))
    (multiple-value-bind (tok next) (%read-token string i)
      (setf type (string-downcase tok) i next))
    (unless (and (< i (length string)) (char= (char string i) #\/))
      (error 'mime-parse-error :message "media type missing '/'"))
    (incf i)
    (multiple-value-bind (tok next) (%read-token string i)
      (setf subtype (string-downcase tok) i next))
    (loop
      (setf i (%skip-ows string i))
      (when (>= i (length string))
        (return))
      (unless (char= (char string i) #\;)
        (error 'mime-parse-error
               :message (format nil "expected ';' at ~D, got ~S" i (char string i))))
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
    (make-instance 'media-type
                   :type type
                   :subtype subtype
                   :parameters (nreverse parameters))))

(defun %quote-parameter-value (value)
  (if (and (plusp (length value)) (every #'%tchar-p value))
      value
      (with-output-to-string (o)
        (write-char #\" o)
        (loop for c across value
              do (when (or (char= c #\\) (char= c #\"))
                   (write-char #\\ o))
                 (write-char c o))
        (write-char #\" o))))

(defun format-media-type (media-type)
  "Serialize MEDIA-TYPE to a Content-Type string."
  (check-type media-type media-type)
  (with-output-to-string (o)
    (write-string (media-type-type media-type) o)
    (write-char #\/ o)
    (write-string (media-type-subtype media-type) o)
    (dolist (p (media-type-parameters media-type))
      (write-string "; " o)
      (write-string (car p) o)
      (write-char #\= o)
      (write-string (%quote-parameter-value (cdr p)) o))))

(defun media-type-parameter (media-type name)
  (cdr (assoc (string-downcase name) (media-type-parameters media-type) :test #'string=)))

(defun media-type-charset (media-type)
  (media-type-parameter media-type "charset"))

(defun media-type-boundary (media-type)
  (media-type-parameter media-type "boundary"))

(defun media-type-essentials (media-type)
  "Lowercase type/subtype without parameters."
  (format nil "~A/~A" (media-type-type media-type) (media-type-subtype media-type)))

(defun media-type-match-p (candidate pattern)
  "RFC 7231 Accept-style match. PATTERN and CANDIDATE are media-type or strings.
   */* matches all; type/* matches the type. Extra candidate parameters are ignored;
   parameters on PATTERN must match."
  (let ((c (if (media-type-p candidate) candidate (parse-media-type candidate)))
        (p (if (media-type-p pattern) pattern (parse-media-type pattern))))
    (and (or (string= (media-type-type p) "*")
             (string= (media-type-type p) (media-type-type c)))
         (or (string= (media-type-subtype p) "*")
             (string= (media-type-subtype p) (media-type-subtype c)))
         (every (lambda (pair)
                  (equal (media-type-parameter c (car pair)) (cdr pair)))
                (media-type-parameters p)))))

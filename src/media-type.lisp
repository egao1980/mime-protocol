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

;;; Raw header text. A parsed header field whose octets are not UTF-8 is
;;; kept as one character per octet, and so are the non-ASCII parameter
;;; values parsed out of it. The raw state belongs to that value object, not
;;; to a field name or an entity: two fields of one name each keep their own,
;;; a field moved to another entity takes it along, and a new value (SET-HEADER)
;;; has none. The table is weak on its keys where the Lisp offers that.

(defvar *raw-values*
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized t)
  #+(or ecl abcl) (make-hash-table :test 'eq :weakness :key)
  #+(or ccl clisp) (make-hash-table :test 'eq :weak :key)
  #+lispworks (make-hash-table :test 'eq :weak-kind :key)
  #+allegro (make-hash-table :test 'eq :weak-keys t)
  #-(or sbcl ecl abcl ccl clisp lispworks allegro) (make-hash-table :test 'eq)
  "Raw value strings → T. See %MARK-RAW.")

(defun %mark-raw (string)
  "Record STRING (one character per octet) as raw parsed header text, which
PRINT-MIME writes back as those octets. → STRING."
  (setf (gethash string *raw-values*) t)
  string)

(defun %raw-value-p (value)
  "Is VALUE a string %MARK-RAW recorded?"
  (and (stringp value) (values (gethash value *raw-values*))))

(defvar *octet-parameters* nil
  "True while a Content-Type or Content-Disposition that holds a raw
parameter value is formatted for PRINT-MIME: every plain parameter value is
then written as octets, one character each (a raw one as it is, any other as
its UTF-8), so the whole field can go out as octets.")

(defun %parameter-text (value)
  "VALUE as a plain parameter is written: as it is, or as octets under
*OCTET-PARAMETERS*."
  (if (and *octet-parameters* (not (%raw-value-p value)))
      (%octet-string value)
      value))

(defun %ows-p (char)
  (or (char= char #\Space) (char= char #\Tab)))

(defun %skip-ows (string start)
  (loop for i from start below (length string)
        while (%ows-p (char string i))
        finally (return i)))

(defun %tchar-p (char)
  ;; RFC 9110 tchar is ASCII only.
  (or (and (< (char-code char) 128) (alphanumericp char))
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

(defun %check-header-text (value)
  "Refuse control characters (CR, LF, NUL, ...; HTAB is allowed) in a header
   parameter value: a CR or LF would end the header line and let the rest of
   VALUE forge further headers."
  (let ((bad (find-if (lambda (c)
                        (let ((code (char-code c)))
                          (or (and (< code 32) (/= code 9)) (= code 127))))
                      value)))
    (when bad
      (error 'mime-encode-error
             :message (format nil "control character U+~4,'0X in header parameter ~S"
                              (char-code bad) value))))
  value)

(defun %quote-parameter-value (value)
  (%check-header-text value)
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
      (write-string (%quote-parameter-value (%parameter-text (cdr p))) o))))

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

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

(defun set-header (entity name value)
  (let* ((key (string-downcase name))
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
  "→ (values header-end body-start) or NIL."
  (loop for i from 0 below (length octets)
        do (cond
             ((and (%crlf-p octets i)
                   (< (+ i 3) (length octets))
                   (%crlf-p octets (+ i 2)))
              (return (values i (+ i 4))))
             ((and (%lf-p octets i)
                   (< (1+ i) (length octets))
                   (%lf-p octets (1+ i)))
              (return (values i (+ i 2)))))))

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

(defun %search-octets (pattern haystack &optional (start 0))
  (let ((plen (length pattern))
        (hlen (length haystack)))
    (loop for i from start to (- hlen plen)
          when (loop for j from 0 below plen
                     always (= (aref pattern j) (aref haystack (+ i j))))
            do (return i))))

(defun %boundary-octets (boundary)
  (%ascii-to-octets (format nil "--~A" boundary)))

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

(defun %split-multipart (body boundary)
  "→ (values preamble parts-octets-list epilogue)"
  (let* ((dash (%boundary-octets boundary))
         (start (%search-octets dash body 0)))
    (unless start
      (error 'mime-parse-error :message "multipart body missing boundary"))
    (let ((preamble (when (plusp start) (subseq body 0 start)))
          (parts '())
          (i start))
      (loop
        (when (%close-boundary-p body i dash)
          (let ((after (%eol-after-boundary body (+ i (length dash) 2))))
            (return (values preamble (nreverse parts)
                            (when (< after (length body)) (subseq body after))))))
        (unless (and (<= (+ i (length dash)) (length body))
                     (eql (%search-octets dash body i) i))
          (error 'mime-parse-error :message "multipart boundary desync"))
        (setf i (%eol-after-boundary body (+ i (length dash))))
        (let* ((crlf-dash (%ascii-to-octets
                           (format nil "~C~C--~A" #\Return #\Newline boundary)))
               (lf-dash (%ascii-to-octets (format nil "~C--~A" #\Newline boundary)))
               (next (or (%search-octets crlf-dash body i)
                         (%search-octets lf-dash body i)
                         (%search-octets dash body i))))
          (unless next
            (error 'mime-parse-error :message "truncated multipart part"))
          (let ((part-end next))
            (when (and (>= part-end 2) (%crlf-p body (- part-end 2)))
              (setf part-end (- part-end 2)))
            (push (subseq body i part-end) parts)
            (setf i (if (and (>= next 2) (%crlf-p body (- next 2)))
                        next
                        (or (%search-octets dash body next) next)))))))))

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
  "Parse SOURCE (string, octets, or stream) as a MIME entity."
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
                                    :headers (parse-headers (%octets-to-ascii header-octets)))))
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

(defun print-mime (entity &key stream)
  "Serialize ENTITY. Returns a string unless STREAM is supplied."
  (check-type entity mime-entity)
  (%ensure-content-type-header entity)
  (flet ((emit (out)
           (dolist (h (mime-headers entity))
             (write-string (car h) out)
             (write-string ": " out)
             (write-string (cdr h) out)
             (%write-crlf out))
           (%write-crlf out)
           (if (multipart-p entity)
               (let* ((boundary (or (and (mime-content-type entity)
                                         (media-type-boundary (mime-content-type entity)))
                                    (error 'mime-encode-error
                                           :message "multipart encode needs a boundary"))))
                 (when (mime-preamble entity)
                   (write-string (%octets-to-ascii (mime-preamble entity)) out)
                   (%write-crlf out))
                 (dolist (part (mime-parts entity))
                   (write-string "--" out)
                   (write-string boundary out)
                   (%write-crlf out)
                   (write-string (print-mime part) out)
                   (%write-crlf out))
                 (write-string "--" out)
                 (write-string boundary out)
                 (write-string "--" out)
                 (%write-crlf out)
                 (when (mime-epilogue entity)
                   (write-string (%octets-to-ascii (mime-epilogue entity)) out)))
               (let ((wired (encode-content (or (mime-content entity) #())
                                            (mime-transfer-encoding entity))))
                 (write-string (if (stringp wired) wired (%octets-to-ascii wired)) out)))))
    (if stream
        (progn (emit stream) (values))
        (with-output-to-string (o)
          (emit o)))))

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
  (let ((boundary (or boundary (format nil "----cl-stack-~A" (random (expt 36 10))))))
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

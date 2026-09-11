(in-package #:mime-protocol)

(defparameter *mime-extension-pairs*
  '(("txt" . "text/plain")
    ("text" . "text/plain")
    ("html" . "text/html")
    ("htm" . "text/html")
    ("css" . "text/css")
    ("csv" . "text/csv")
    ("md" . "text/markdown")
    ("xml" . "application/xml")
    ("json" . "application/json")
    ("ndjson" . "application/x-ndjson")
    ("yaml" . "application/yaml")
    ("yml" . "application/yaml")
    ("js" . "text/javascript")
    ("mjs" . "text/javascript")
    ("pdf" . "application/pdf")
    ("zip" . "application/zip")
    ("gz" . "application/gzip")
    ("bz2" . "application/x-bzip2")
    ("xz" . "application/x-xz")
    ("lzma" . "application/x-lzma")
    ("tar" . "application/x-tar")
    ("tgz" . "application/x-tar")
    ("tbz" . "application/x-tar")
    ("tbz2" . "application/x-tar")
    ("txz" . "application/x-tar")
    ("wasm" . "application/wasm")
    ("bin" . "application/octet-stream")
    ("cbor" . "application/cbor")
    ("avro" . "application/avro")
    ("msgpack" . "application/msgpack")
    ("mpk" . "application/msgpack")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp")
    ("svg" . "image/svg+xml")
    ("ico" . "image/x-icon")
    ("mp3" . "audio/mpeg")
    ("mp4" . "video/mp4")
    ("woff" . "font/woff")
    ("woff2" . "font/woff2")
    ("ttf" . "font/ttf")
    ("form" . "application/x-www-form-urlencoded")))

(defparameter *mime-extensions*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair *mime-extension-pairs* ht)
      (setf (gethash (car pair) ht) (cdr pair)))))

(defparameter *mime-type-extensions*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair *mime-extension-pairs* ht)
      (unless (gethash (cdr pair) ht)
        (setf (gethash (cdr pair) ht) (car pair))))))

(defun %extension-of (name)
  (let* ((s (string-downcase (etypecase name
                               (pathname (file-namestring name))
                               (string name))))
         (dot (position #\. s :from-end t)))
    (if dot
        (subseq s (1+ dot))
        (if (find #\/ s)
            nil
            s))))

(defun lookup-mime (name &optional (default "application/octet-stream"))
  "Filename, pathname, or extension → media-type string."
  (or (gethash (%extension-of name) *mime-extensions*)
      default))

(defun mime-type (name &optional (default "application/octet-stream"))
  "Alias of LOOKUP-MIME."
  (lookup-mime name default))

(defun lookup-extension (media-type)
  "Media-type string or MEDIA-TYPE → filename extension (no leading dot).
   First table match wins (html before htm, jpg before jpeg)."
  (let ((essentials (cond
                      ((null media-type) nil)
                      ((media-type-p media-type)
                       (media-type-essentials media-type))
                      ((stringp media-type)
                       (let ((mt (ignore-errors (parse-media-type media-type))))
                         (and mt (media-type-essentials mt))))
                      (t nil))))
    (and essentials (gethash essentials *mime-type-extensions*))))

(defparameter *content-encodings*
  '(("gz" . "gzip")
    ("bz2" . "bzip2")
    ("xz" . "xz")
    ("lzma" . "lzma")
    ("z" . "compress")))

(defparameter *suffix-map*
  '(("tgz" . "tar.gz")
    ("taz" . "tar.gz")
    ("tbz" . "tar.bz2")
    ("tbz2" . "tar.bz2")
    ("txz" . "tar.xz")))

(defun %filename-of (name)
  (string-downcase (etypecase name
                     (pathname (file-namestring name))
                     (string (let ((slash (position #\/ name :from-end t)))
                               (if slash (subseq name (1+ slash)) name))))))

(defun %apply-suffix-map (filename)
  (let* ((dot (position #\. filename :from-end t))
         (ext (and dot (subseq filename (1+ dot))))
         (mapped (and ext (cdr (assoc ext *suffix-map* :test #'string=)))))
    (if mapped
        (concatenate 'string (subseq filename 0 (1+ dot)) mapped)
        filename)))

(defun guess-type (name)
  "Python mimetypes.guess_type — (values type encoding).
   `.tar.gz` → application/x-tar + gzip. Unknown type is NIL, not octet-stream."
  (let* ((filename (%apply-suffix-map (%filename-of name)))
         (encoding nil)
         (dot (position #\. filename :from-end t)))
    (when dot
      (let* ((ext (subseq filename (1+ dot)))
             (enc (cdr (assoc ext *content-encodings* :test #'string=))))
        (when enc
          (setf encoding enc
                filename (subseq filename 0 dot)
                dot (position #\. filename :from-end t)))))
    (values (and dot (gethash (subseq filename (1+ dot)) *mime-extensions*))
            encoding)))

(defun add-type (type extension)
  "Register EXTENSION (with or without leading dot) → TYPE. Updates both tables."
  (check-type type string)
  (let* ((ext (string-downcase
               (string-left-trim "." (etypecase extension
                                       (string extension)
                                       (symbol (string extension))))))
         (mime (string-downcase type)))
    (setf (gethash ext *mime-extensions*) mime)
    (unless (gethash mime *mime-type-extensions*)
      (setf (gethash mime *mime-type-extensions*) ext))
    mime))

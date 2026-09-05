(in-package #:mime-protocol)

;;;; serdes-protocol implementor (:mime / :multipart).

(defclass mime-serdes-backend (serdes-protocol:serdes-backend) ()
  (:documentation "Whole MIME entity ↔ print-mime / parse-mime."))

(defclass multipart-serdes-backend (serdes-protocol:serdes-backend) ()
  (:documentation "Alist/hash-table ↔ multipart/form-data."))

(defun make-mime-serdes-backend ()
  (make-instance 'mime-serdes-backend))

(defun make-multipart-serdes-backend ()
  (make-instance 'multipart-serdes-backend))

(defmethod serdes-protocol:backend-media-type ((backend mime-serdes-backend))
  "message/rfc822")

(defmethod serdes-protocol:backend-media-type ((backend multipart-serdes-backend))
  "multipart/form-data")

(defmethod serdes-protocol:backend-encode ((backend mime-serdes-backend) value &key stream)
  (declare (ignore backend))
  (print-mime (if (mime-entity-p value)
                  value
                  (error 'mime-encode-error
                         :message ":mime encode expects a mime-entity"))
              :stream stream))

(defmethod serdes-protocol:backend-decode ((backend mime-serdes-backend) source &key)
  (declare (ignore backend))
  (parse-mime source))

(defun %parts-table (entity)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (part (mime-parts entity) ht)
      (let* ((cd (mime-content-disposition part))
             (name (or (and cd (disposition-name cd))
                       (format nil "part-~D" (hash-table-count ht))))
             (filename (and cd (disposition-filename cd)))
             (body (if (multipart-p part)
                       (%parts-table part)
                       (mime-content part))))
        (setf (gethash name ht)
              (if filename
                  (list :filename filename
                        :content-type (and (mime-content-type part)
                                           (format-media-type (mime-content-type part)))
                        :content body)
                  body))))))

(defmethod serdes-protocol:backend-encode ((backend multipart-serdes-backend) value &key stream)
  (declare (ignore backend))
  (print-mime (cond
                ((mime-entity-p value) value)
                ((or (hash-table-p value) (listp value)) (make-form-data value))
                (t (error 'mime-encode-error
                          :message ":multipart encode expects a mime-entity, alist, or hash-table")))
              :stream stream))

(defmethod serdes-protocol:backend-decode ((backend multipart-serdes-backend) source &key)
  (declare (ignore backend))
  (let ((entity (parse-mime source)))
    (if (multipart-p entity)
        (%parts-table entity)
        entity)))

(defun %entity-to-string (source)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8)) (%octets-to-ascii source))
    (stream (with-output-to-string (o)
              (loop for c = (read-char source nil nil)
                    while c do (write-char c o))))))

(defclass mime-character-input-stream (serdes-protocol:serdes-character-input-stream)
  ((done :initform nil :accessor mime-stream-done-p)))

(defclass mime-character-output-stream (serdes-protocol:serdes-character-output-stream) ())

(defmethod serdes-protocol:backend-make-input-stream ((backend mime-serdes-backend)
                                                      underlying
                                                      &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'mime-error :message "mime streams are character"))
  (make-instance 'mime-character-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend mime-serdes-backend)
                                                       underlying
                                                       &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'mime-error :message "mime streams are character"))
  (make-instance 'mime-character-output-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-input-stream ((backend multipart-serdes-backend)
                                                      underlying
                                                      &key (element-type 'character))
  (serdes-protocol:backend-make-input-stream (make-mime-serdes-backend)
                                             underlying :element-type element-type))

(defmethod serdes-protocol:backend-make-output-stream ((backend multipart-serdes-backend)
                                                       underlying
                                                       &key (element-type 'character))
  (serdes-protocol:backend-make-output-stream (make-mime-serdes-backend)
                                              underlying :element-type element-type))

(defmethod serdes-protocol:stream-decode-value ((stream mime-character-input-stream) &key)
  (if (mime-stream-done-p stream)
      :eof
      (progn
        (setf (mime-stream-done-p stream) t)
        (parse-mime (%entity-to-string (serdes-protocol:underlying-stream stream))))))

(defmethod serdes-protocol:stream-encode-value ((stream mime-character-output-stream) value &key)
  (print-mime (if (mime-entity-p value)
                  value
                  (make-form-data value))
              :stream (serdes-protocol:underlying-stream stream))
  value)

(defun install-http-mime-hooks ()
  "If http-protocol is loaded, register :mime / :multipart data (de)serializers."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-mime-hooks nil))
    (flet ((push-codec (table-sym type fn)
             (let ((s (find-symbol table-sym pkg)))
               (when (and s (boundp s))
                 (setf (symbol-value s)
                       (acons type fn (remove type (symbol-value s) :key #'car)))))))
      (push-codec "*DATA-SERIALIZERS*" :mime #'print-mime)
      (push-codec "*DATA-SERIALIZERS*" :multipart
                  (lambda (value) (print-mime (if (mime-entity-p value)
                                                  value
                                                  (make-form-data value)))))
      (push-codec "*DATA-DESERIALIZERS*" :mime #'parse-mime)
      (push-codec "*DATA-DESERIALIZERS*" :multipart
                  (lambda (source)
                    (let ((e (parse-mime source)))
                      (if (multipart-p e) (%parts-table e) e)))))
    t))

(defun use-mime-serdes-backend ()
  (let ((backend (make-mime-serdes-backend)))
    (serdes-protocol:register-format :mime backend :media-type "message/rfc822")
    backend))

(defun use-multipart-serdes-backend ()
  (let ((backend (make-multipart-serdes-backend)))
    (serdes-protocol:register-format :multipart backend
                                     :media-type "multipart/form-data")
    backend))

(eval-when (:load-toplevel :execute)
  (use-mime-serdes-backend)
  (use-multipart-serdes-backend)
  (install-http-mime-hooks))

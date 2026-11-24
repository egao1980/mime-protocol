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

(defmethod serdes-protocol:backend-binary-p ((backend mime-serdes-backend))
  "PRINT-MIME's wire form is octets (header text UTF-8, body as it is)."
  (declare (ignore backend))
  t)

(defmethod serdes-protocol:backend-binary-p ((backend multipart-serdes-backend))
  (declare (ignore backend))
  t)

(defun %serdes-print (entity stream max-line-length)
  "ENTITY for serdes: its octets without STREAM; the octets to a binary
STREAM, one character per octet to a character STREAM."
  (cond
   ((null stream)
    (print-mime-octets entity :max-line-length max-line-length))
   ((subtypep (stream-element-type stream) 'character)
    (print-mime entity :stream stream :max-line-length max-line-length))
   (t
    (write-sequence (print-mime-octets entity :max-line-length max-line-length) stream)
    (values))))

(defun %source-octets (stream)
  "All of STREAM as PARSE-MIME's source: octets from a binary stream, the
characters (one per octet) of a character stream."
  (if (subtypep (stream-element-type stream) 'character)
      (with-output-to-string (o)
        (loop for c = (read-char stream nil nil)
              while c
              do (write-char c o)))
      (let ((buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (loop for b = (read-byte stream nil nil)
              while b
              do (vector-push-extend b buffer))
        (coerce buffer '(vector (unsigned-byte 8))))))

(defmethod serdes-protocol:backend-encode ((backend mime-serdes-backend) value &key stream)
  "The entity's octets (a binary format: ENCODE-TO-OCTETS passes them on as
they are); see %SERDES-PRINT for STREAM."
  (declare (ignore backend))
  (%serdes-print (if (mime-entity-p value)
                     value
                     (error 'mime-encode-error
                            :message ":mime encode expects a mime-entity"))
                 stream 998))

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
  "multipart/form-data for HTTP, as octets: no line limit, so no folding
(RFC 7578)."
  (declare (ignore backend))
  (%serdes-print (cond
                  ((mime-entity-p value) value)
                  ((or (hash-table-p value) (listp value)) (make-form-data value))
                  (t (error 'mime-encode-error
                            :message ":multipart encode expects a mime-entity, alist, or hash-table")))
                 stream nil))

(defmethod serdes-protocol:backend-decode ((backend multipart-serdes-backend) source &key)
  (declare (ignore backend))
  (let ((entity (parse-mime source)))
    (if (multipart-p entity)
        (%parts-table entity)
        entity)))

(defclass mime-input-stream ()
  ((done :initform nil :accessor mime-stream-done-p))
  (:documentation "A serdes input stream holding one MIME entity."))

(defclass mime-character-input-stream (mime-input-stream
                                       serdes-protocol:serdes-character-input-stream)
  ())

(defclass mime-binary-input-stream (mime-input-stream
                                    serdes-protocol:serdes-binary-input-stream)
  ())

(defclass mime-output-stream ()
  ()
  (:documentation "A serdes output stream that writes MIME entities."))

(defclass mime-character-output-stream (mime-output-stream
                                        serdes-protocol:serdes-character-output-stream)
  ())

(defclass mime-binary-output-stream (mime-output-stream
                                     serdes-protocol:serdes-binary-output-stream)
  ())

(defun %mime-stream-class (direction element-type)
  "The MIME stream class for DIRECTION (:input or :output) and ELEMENT-TYPE:
octets (the default, :mime and :multipart being binary formats) or
characters, one per octet."
  (cond
   ((subtypep element-type 'character)
    (if (eq direction :input) 'mime-character-input-stream 'mime-character-output-stream))
   ((subtypep element-type '(unsigned-byte 8))
    (if (eq direction :input) 'mime-binary-input-stream 'mime-binary-output-stream))
   (t
    (error 'mime-error
           :message (format nil "mime streams are octets or characters, not ~S" element-type)))))

(defmethod serdes-protocol:backend-make-input-stream ((backend mime-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (make-instance (%mime-stream-class :input element-type)
                 :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend mime-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (make-instance (%mime-stream-class :output element-type)
                 :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-input-stream ((backend multipart-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (serdes-protocol:backend-make-input-stream (make-mime-serdes-backend)
                                             underlying :element-type element-type))

(defmethod serdes-protocol:backend-make-output-stream ((backend multipart-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (serdes-protocol:backend-make-output-stream (make-mime-serdes-backend)
                                              underlying :element-type element-type))

(defmethod serdes-protocol:stream-decode-value ((stream mime-input-stream) &key)
  (if (mime-stream-done-p stream)
      :eof
      (progn
        (setf (mime-stream-done-p stream) t)
        (parse-mime (%source-octets (serdes-protocol:underlying-stream stream))))))

(defmethod serdes-protocol:stream-encode-value ((stream mime-output-stream) value &key)
  "Write VALUE, an entity or form fields (multipart/form-data, not folded),
to the underlying stream: octets, or one character per octet."
  (if (mime-entity-p value)
      (%serdes-print value (serdes-protocol:underlying-stream stream) 998)
      (%serdes-print (make-form-data value) (serdes-protocol:underlying-stream stream) nil))
  value)

(defun install-http-mime-hooks ()
  "If http-protocol is loaded, register :mime / :multipart data (de)serializers.
The serializers return octets (PRINT-MIME-OCTETS): http-protocol encodes a
string body as UTF-8, which would encode PRINT-MIME's octet text twice.
HTTP has no line limit, so form-data is not folded."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-mime-hooks nil))
    (flet ((push-codec (table-sym type fn)
             (let ((s (find-symbol table-sym pkg)))
               (when (and s (boundp s))
                 (setf (symbol-value s)
                       (acons type fn (remove type (symbol-value s) :key #'car)))))))
      (push-codec "*DATA-SERIALIZERS*" :mime #'print-mime-octets)
      (push-codec "*DATA-SERIALIZERS*" :multipart
                  (lambda (value) (print-mime-octets (if (mime-entity-p value)
                                                         value
                                                         (make-form-data value))
                                                     :max-line-length nil)))
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

(in-package #:mime-protocol)

(define-condition mime-error (error)
  ((message :initarg :message :reader mime-error-message :initform nil))
  (:report (lambda (c s)
             (format s "MIME error~@[: ~a~]" (mime-error-message c)))))

(define-condition mime-parse-error (mime-error) ())

(define-condition mime-encode-error (mime-error) ())

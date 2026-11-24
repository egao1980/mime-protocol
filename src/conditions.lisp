(in-package #:mime-protocol)

(define-condition mime-error (error)
  ((message :initarg :message :reader mime-error-message :initform nil))
  (:report (lambda (c s)
             (format s "MIME error~@[: ~a~]" (mime-error-message c)))))

(define-condition mime-parse-error (mime-error) ())

(define-condition mime-encode-error (mime-error) ())

(define-condition mime-header-error (mime-encode-error)
  ((name :initarg :name :reader mime-header-error-name :initform nil
         :documentation "The header field name, as given.")
   (value :initarg :value :reader mime-header-error-value :initform nil
          :documentation "The refused value, or NIL when the name is at fault."))
  (:report (lambda (c s)
             (format s "MIME header error~@[ in ~S~]: ~A"
                     (mime-header-error-name c)
                     (mime-error-message c))))
  (:documentation
   "A header field that cannot be written as one field. A CR or LF in the
value would end the field and start another one (header injection, RFC 5322
2.2), and a NUL is not allowed in a header (RFC 2045 2.7); a name must be
RFC 5322 ftext (printable ASCII, no colon). SET-HEADER and PRINT-MIME signal
it with a USE-VALUE restart that takes another value (or name), which is
checked again."))

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
RFC 5322 ftext (printable ASCII, no colon), or a reader ends the header
block there; a line must fold within 998 octets (RFC 5322 2.1.1).
SET-HEADER and PRINT-MIME signal it with a USE-VALUE restart that takes
another value (or name), which is checked again."))

(define-condition mime-boundary-error (mime-encode-error)
  ((boundary :initarg :boundary :reader mime-boundary-error-boundary :initform nil
             :documentation "The multipart boundary that clashes."))
  (:report (lambda (c s)
             (format s "MIME boundary error~@[ (~S)~]: ~A"
                     (mime-boundary-error-boundary c)
                     (mime-error-message c))))
  (:documentation
   "A multipart part or preamble holds its own delimiter line, so it would
end early and the rest pose as further parts. PRINT-MIME signals it with a
USE-VALUE restart that takes another boundary."))

(in-package #:mime-protocol)

;;; Content-Transfer-Encoding (RFC 2045). Not HTTP Content-Encoding.

(defun normalize-transfer-encoding (encoding)
  (let* ((raw (etypecase encoding
                (null "7bit")
                (keyword (string-downcase (symbol-name encoding)))
                (string (string-downcase (string-trim '(#\Space #\Tab) encoding)))))
         (name (if (zerop (length raw)) "7bit" raw)))
    (cond
      ((member name '("7bit" "7-bit") :test #'string=) :7bit)
      ((member name '("8bit" "8-bit") :test #'string=) :8bit)
      ((member name '("binary") :test #'string=) :binary)
      ((member name '("base64") :test #'string=) :base64)
      ((member name '("quoted-printable" "quotedprintable" "qp") :test #'string=)
       :quoted-printable)
      (t (error 'mime-parse-error
                :message (format nil "unknown Content-Transfer-Encoding ~S" encoding))))))

(defun %as-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (string (encoding-protocol:encode source))
    (vector
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'mime-encode-error :message "cannot treat value as octets")))))

(defun %as-ascii (source)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8))
     (encoding-protocol:decode source :encoding :ascii))
    (vector
     (encoding-protocol:decode (coerce source '(vector (unsigned-byte 8))) :encoding :ascii))))

(defun encode-base64 (octets)
  "RFC 2045 Base64 with 76-column wrapping."
  (encoding-protocol:encode octets :encoding :base64 :columns 76))

(defun decode-base64 (text)
  (handler-case
      (encoding-protocol:decode text :encoding :base64)
    (encoding-protocol:encoding-decode-error (c)
      (error 'mime-parse-error
             :message (or (encoding-protocol:encoding-error-message c)
                          (princ-to-string c))))))

(defun encode-quoted-printable (octets)
  "RFC 2045 quoted-printable."
  (encoding-protocol:encode octets :encoding :quoted-printable))

(defun decode-quoted-printable (text)
  (handler-case
      (encoding-protocol:decode text :encoding :quoted-printable)
    (encoding-protocol:encoding-decode-error (c)
      (error 'mime-parse-error
             :message (or (encoding-protocol:encoding-error-message c)
                          (princ-to-string c))))))

(defun encode-content (source encoding)
  "Encode SOURCE (string or octets) with Content-Transfer-Encoding.
   Returns a string for :base64 / :quoted-printable, octets otherwise."
  (let ((enc (normalize-transfer-encoding encoding))
        (octets (%as-octets source)))
    (ecase enc
      ((:7bit :8bit :binary) octets)
      (:base64 (encode-base64 octets))
      (:quoted-printable (encode-quoted-printable octets)))))

(defun decode-content (source encoding)
  "Decode SOURCE with Content-Transfer-Encoding → octets."
  (let ((enc (normalize-transfer-encoding encoding)))
    (ecase enc
      ((:7bit :8bit :binary) (%as-octets source))
      (:base64 (decode-base64 (%as-ascii source)))
      (:quoted-printable (decode-quoted-printable (%as-ascii source))))))

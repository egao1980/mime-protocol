(in-package #:mime-protocol/tests)

(deftest base64-roundtrip
  (let* ((raw (encoding-protocol:encode "hi?" :encoding :utf-8))
         (b64 (encode-content raw :base64))
         (back (decode-content b64 :base64)))
    (ok (stringp b64))
    (ok (equalp raw back))))

(deftest quoted-printable-roundtrip
  (let* ((raw (encoding-protocol:encode (format nil "a=~C~%b" #\Space) :encoding :utf-8))
         (qp (encode-content raw :quoted-printable))
         (back (decode-content qp :quoted-printable)))
    (ok (find #\= qp :test #'char=))
    (ok (equalp raw back))))

(deftest identity-cte
  (let ((raw (encoding-protocol:encode "plain" :encoding :utf-8)))
    (ok (equalp raw (decode-content (encode-content raw :7bit) :7bit)))
    (ok (equalp raw (decode-content (encode-content raw :binary) :8bit)))))

(deftest normalize-transfer-encoding-names
  (ok (eq :quoted-printable (normalize-transfer-encoding "Quoted-Printable")))
  (ok (eq :base64 (normalize-transfer-encoding :base64)))
  (ok (signals (normalize-transfer-encoding "gzip") 'mime-parse-error)))

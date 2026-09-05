(in-package #:mime-protocol)

;;; Content-Transfer-Encoding (RFC 2045). Not HTTP Content-Encoding.

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

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
    (string (babel:string-to-octets source :encoding :utf-8))
    (vector
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'mime-encode-error :message "cannot treat value as octets")))))

(defun %as-ascii (source)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8))
     (babel:octets-to-string source :encoding :ascii))
    (vector
     (babel:octets-to-string (coerce source '(vector (unsigned-byte 8))) :encoding :ascii))))

(defun encode-base64 (octets)
  (let* ((n (length octets))
         (out (make-string-output-stream))
         (col 0))
    (labels ((emit (char)
               (write-char char out)
               (incf col)
               (when (= col 76)
                 (write-char #\Return out)
                 (write-char #\Newline out)
                 (setf col 0))))
      (loop for i from 0 below n by 3
            for b0 = (aref octets i)
            for b1 = (if (< (1+ i) n) (aref octets (1+ i)) 0)
            for b2 = (if (< (+ i 2) n) (aref octets (+ i 2)) 0)
            for leftover = (- n i)
            for n24 = (logior (ash b0 16) (ash b1 8) b2)
            do (emit (char +base64-alphabet+ (ldb (byte 6 18) n24)))
               (emit (char +base64-alphabet+ (ldb (byte 6 12) n24)))
               (if (>= leftover 2)
                   (emit (char +base64-alphabet+ (ldb (byte 6 6) n24)))
                   (emit #\=))
               (if (>= leftover 3)
                   (emit (char +base64-alphabet+ (ldb (byte 6 0) n24)))
                   (emit #\=)))
      (let ((s (get-output-stream-string out)))
        (if (and (>= (length s) 2)
                 (char= (char s (- (length s) 2)) #\Return)
                 (char= (char s (1- (length s))) #\Newline))
            (subseq s 0 (- (length s) 2))
            s)))))

(defun decode-base64 (text)
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (acc 0)
        (bits 0))
    (loop for c across text
          do (cond
               ((or (char= c #\Space) (char= c #\Tab)
                    (char= c #\Return) (char= c #\Newline))
                nil)
               ((char= c #\=)
                (setf bits 0))
               (t
                (let ((v (position c +base64-alphabet+ :test #'char=)))
                  (unless v
                    (error 'mime-parse-error
                           :message (format nil "invalid base64 character ~S" c)))
                  (setf acc (logior (ash acc 6) v)
                        bits (+ bits 6))
                  (when (>= bits 8)
                    (decf bits 8)
                    (vector-push (ldb (byte 8 bits) acc) octets))))))
    (coerce octets '(vector (unsigned-byte 8)))))

(defun %qp-hex (n)
  (char "0123456789ABCDEF" n))

(defun encode-quoted-printable (octets)
  (let ((out (make-string-output-stream))
        (col 0))
    (labels ((soft-break ()
               (write-char #\= out)
               (write-char #\Return out)
               (write-char #\Newline out)
               (setf col 0))
             (emit-raw (char)
               (when (>= col 75)
                 (soft-break))
               (write-char char out)
               (incf col))
             (emit-encoded (byte)
               (when (>= col 73)
                 (soft-break))
               (write-char #\= out)
               (write-char (%qp-hex (ash byte -4)) out)
               (write-char (%qp-hex (logand byte 15)) out)
               (incf col 3)))
      (loop for i from 0 below (length octets)
            for b = (aref octets i)
            do (cond
                 ((or (<= 33 b 60) (<= 62 b 126))
                  (emit-raw (code-char b)))
                 ((or (= b 9) (= b 32))
                  (let ((last-on-line
                          (or (= (1+ i) (length octets))
                              (and (< (1+ i) (length octets))
                                   (or (= (aref octets (1+ i)) 13)
                                       (= (aref octets (1+ i)) 10))))))
                    (if last-on-line
                        (emit-encoded b)
                        (emit-raw (code-char b)))))
                 ((or (= b 13) (= b 10))
                  (write-char (code-char b) out)
                  (setf col 0))
                 (t (emit-encoded b))))
      (get-output-stream-string out))))

(defun decode-quoted-printable (text)
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0)
        (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((char= c #\=)
                  (let ((a (if (< (1+ i) n) (char text (1+ i)) nil))
                        (b (if (< (+ i 2) n) (char text (+ i 2)) nil)))
                    (cond
                      ((and a (or (char= a #\Return) (char= a #\Newline)))
                       (incf i (if (and (char= a #\Return) b (char= b #\Newline)) 3 2)))
                      ((and a b (%hex-digit-p a) (%hex-digit-p b))
                       (vector-push (%hex-byte a b) octets)
                       (incf i 3))
                      (t
                       (error 'mime-parse-error :message "invalid quoted-printable escape")))))
                 ((or (char= c #\Return) (char= c #\Newline))
                  (vector-push (char-code c) octets)
                  (incf i))
                 (t
                  (vector-push (char-code c) octets)
                  (incf i)))))
    (coerce octets '(vector (unsigned-byte 8)))))

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

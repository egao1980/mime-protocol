(in-package #:mime-protocol/tests)

;;; 0.3.4 (mime-protocol#19): raw state per value, raw parameters relayed,
;;; COPY-HEADERS, no form-data continuations, continued plain values, C0
;;; controls refused. Python 3.13 email / python-multipart / Werkzeug
;;; comparisons, 2026-11-26.

(deftest copy-headers-moves-fields-with-their-octets
  (let* ((field (%r2-octets #xCF #xF0 #xE8 #xE2 #xE5 #xF2))
         (old (parse-mime (%r2-octets "Subject: " field 13 10
                                      "X-A: 1" 13 10
                                      "Content-Type: text/plain; charset=windows-1251" 13 10
                                      13 10 "body")))
         (new (make-multipart-entity (list old) :boundary "b")))
    (ok (eq new (copy-headers old new
                              :test (lambda (name) (not (eql 0 (search "content-" name))))
                              :remove t)))
    (ok (equal '("subject" "x-a") (mapcar #'car (mime-headers new))))
    (ok (equal '("content-type") (mapcar #'car (mime-headers old))) "the fields left OLD")
    (ok (search (%r2-octets "Subject: " field 13 10) (print-mime-octets new))
        "the moved Subject goes out as its octets")))

(deftest a-long-raw-filename-is-relayed-as-plain-continuations
  ;; Python 3.13 reads the same filename (compat32 and policy.default) from
  ;; the input and from this output.
  (let* ((name (concatenate '(vector (unsigned-byte 8))
                            (loop repeat 40 append '(#xCF #xF0 #xE8))
                            (%r2-octets ".txt")))
         (raw (%r2-octets "Content-Disposition: attachment; filename=\"" name "\"" 13 10
                          13 10 "body"))
         (in (parse-mime raw))
         (wire (print-mime-octets in :max-line-length 78))
         (text (%r3-text (%r3-header-block wire))))
    (ok (search "filename*0=\"" text) text)
    (ok (not (search "filename*0*=" text)) "no UTF-8 reading of unknown octets")
    (ok (every (lambda (line) (<= (length line) 998)) (%r2-header-lines text)))
    (ok (equal (disposition-filename (mime-content-disposition in))
               (disposition-filename (mime-content-disposition (parse-mime wire))))
        "the same octets read back")))

(deftest-parametrize form-data-reads-no-continuations
    ((value filename)
     ;; python-multipart: no filename; Werkzeug joins it to "evil.exe".
     ("form-data; name=\"f\"; filename*0*=UTF-8''evil; filename*1=\".exe\"" nil)
     ;; Both: "safe.txt".
     ("form-data; name=\"f\"; filename=\"safe.txt\"; filename*0=\"evil.exe\"" "safe.txt"))
  (ok (equal filename (disposition-filename (parse-content-disposition value)))))

(deftest form-data-still-writes-utf-8-filename-and-filename*
  (let ((text (format-content-disposition
               (make-content-disposition "form-data" '(("name" . "f") ("filename" . "é.txt"))))))
    (ok (equal "form-data; name=f; filename=\"é.txt\"; filename*=UTF-8''%C3%A9.txt" text) text)))

(deftest a-long-value-with-a-control-is-refused-not-encoded
  (dolist (filename (list (format nil "~A~C~CX: y" (make-string 100 :initial-element #\a)
                                  #\Return #\Newline)
                          (format nil "caf~C~CX: y" (code-char #xE9) (code-char 12))))
    (ok (signals (format-content-disposition
                  (make-content-disposition "attachment" (list (cons "filename" filename))))
                 'mime-encode-error))))

(deftest print-mime-refuses-a-parsed-control
  ;; A parsed field may hold FF; PRINT-MIME does not pass it on.
  (let ((e (parse-mime (%r2-octets "X-A: a" 12 "Bcc: eve@example.com" 13 10 13 10 "body"))))
    (ok (signals (print-mime e) 'mime-header-error))))

(deftest encoded-words-still-take-other-controls
  (ok (search "=?utf-8?b?" (encode-header-text (format nil "a~Cb" (code-char 12))))))

(deftest text-a-charset-cannot-encode-is-a-mime-encode-error
  (ok (signals (make-text-entity (string (code-char #x4E2D)) :charset "iso-8859-1")
               'mime-encode-error)))

(deftest a-bad-ext-value-is-a-mime-parse-error
  (ok (signals (disposition-filename (parse-content-disposition "attachment; filename*=utf-8''%FF"))
               'mime-parse-error)))

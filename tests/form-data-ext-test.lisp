(in-package #:mime-protocol/tests)

;;; 0.3.6 (mime-protocol#21): RFC 7578 4.2 says filename* MUST NOT be used
;;; in form-data, and servers disagree on it. For
;;; filename="safe.txt"; filename*=UTF-8''evil.exe mime-protocol and
;;; Werkzeug read evil.exe, python-multipart safe.txt; filename* alone is no
;;; file at all to python-multipart. Werkzeug joins name*0 / filename*0
;;; continuations, python-multipart does not. So a form-data disposition
;;; refuses every name* / filename* parameter, except a filename* that reads
;;; as the filename itself, and never writes one.

(deftest-parametrize form-data-ext-parameters-are-refused
    ((value)
     ("form-data; name=\"f\"; filename=\"safe.txt\"; filename*=UTF-8''evil.exe")
     ("form-data; name=\"f\"; filename*=UTF-8''evil.exe; filename=\"safe.txt\"")
     ("form-data; name=\"f\"; filename*=UTF-8''alone.txt")
     ("form-data; name*=UTF-8''admin; name=\"user\"")
     ("form-data; name*0=\"ad\"; name*1=\"min\"; name=\"user\"")
     ("form-data; name=\"f\"; filename*0=\"evil.exe\"; filename=\"safe.txt\"")
     ("form-data; name=\"f\"; filename*0*=UTF-8''evil; filename*1=\".exe\""))
  (ok (signals (parse-content-disposition value) 'mime-parse-error) value))

(deftest form-data-filename*-that-matches-is-read
  (let ((d (parse-content-disposition
            "form-data; name=\"f\"; filename=\"caf\\é.txt\"; filename*=UTF-8''caf%5C%C3%A9.txt")))
    (ok (equal (format nil "caf\\~C.txt" (code-char #xE9)) (disposition-filename d))))
  (ok (equal "Prelude.svg"
             (disposition-filename
              (parse-content-disposition
               "form-data; name=\"a\"; filename=\"Prelude.svg\"; filename*=UTF-8''Prelude.svg")))))

(deftest form-data-writes-no-filename*
  (let ((name (format nil "r~Csum~C.pdf" (code-char #xE9) (code-char #xE9))))
    (ok (equal (format nil "form-data; name=\"file\"; filename=\"~A\"" name)
               (format-form-data-disposition "file" :filename name)))
    (ok (equal (format nil "form-data; name=f; filename=\"~A\"" name)
               (format-content-disposition
                (make-content-disposition "form-data" `(("name" . "f") ("filename" . ,name))))))
    (ok (equal "form-data; name=f; filename=a.txt"
               (format-content-disposition
                (make-content-disposition "form-data" '(("name" . "f") ("filename" . "a.txt")
                                                        ("filename*" . "UTF-8''a.txt")))))
        "a filename* parameter is not written either")
    (ok (equal name (disposition-filename
                     (parse-content-disposition (format-form-data-disposition "f" :filename name))))
        "the UTF-8 filename reads back")))

(deftest attachment-filename*-still-wins
  (ok (equal (format nil "~C.txt" (code-char #xE9))
             (disposition-filename
              (parse-content-disposition
               "attachment; filename=\"fallback.txt\"; filename*=UTF-8''%C3%A9.txt")))
      "outside form-data RFC 6266 holds: filename* is preferred"))

(in-package #:mime-protocol/tests)

;;; Header-safe Content-Disposition output (http-protocol#44).

(defun %crlf (&rest parts)
  (format nil "~{~A~}" (mapcar (lambda (p) (if (eq p :crlf) (format nil "~C~C" #\Return #\Newline) p))
                              parts)))

(deftest rfc5987-encode
  (ok (string= "UTF-8''na%C3%AFve.txt" (encode-rfc5987-value "naïve.txt")))
  (ok (string= "UTF-8''a%20b%22%0D%0A" (encode-rfc5987-value (%crlf "a b\"" :crlf))))
  (ok (string= "naïve.txt"
               (disposition-filename
                (parse-content-disposition
                 (format nil "attachment; filename*=~A" (encode-rfc5987-value "naïve.txt")))))
      "round-trips through parse-content-disposition"))

(deftest form-data-disposition-escapes
  (let ((v (format-form-data-disposition
            (%crlf "f\"; x=\"1" :crlf "X-Evil: 1")
            :filename (%crlf "a.txt\"" :crlf "Content-Type: text/html"))))
    (ok (not (find #\Return v)) "no CR")
    (ok (not (find #\Newline v)) "no LF")
    (ok (string= (%crlf "form-data; name=\"f%22; x=%221%0D%0AX-Evil: 1\"; "
                        "filename=\"a.txt%22%0D%0AContent-Type: text/html\"")
                 v))
    (let ((cd (parse-content-disposition v)))
      (ok (equal '("name" "filename")
                 (mapcar #'car (content-disposition-parameters cd)))
          "exactly two parameters, nothing forged"))))

(deftest form-data-disposition-non-ascii
  (let ((v (format-form-data-disposition "file" :filename "résumé.pdf")))
    (ok (string= "form-data; name=\"file\"; filename=\"résumé.pdf\"; filename*=UTF-8''r%C3%A9sum%C3%A9.pdf" v))
    (ok (string= "résumé.pdf" (disposition-filename (parse-content-disposition v)))))
  (ok (string= "form-data; name=\"a\"" (format-form-data-disposition "a")))
  (ok (signals (format-form-data-disposition (format nil "a~Cb" (code-char 0)))
               'mime-encode-error)))

(deftest format-content-disposition-safe
  (ok (signals (format-content-disposition
                (make-content-disposition "attachment"
                                          (list (cons "filename" (%crlf "a" :crlf "X: y")))))
               'mime-encode-error)
      "CR/LF refused")
  (ok (string= "attachment; filename=\"a\\\"b\""
               (format-content-disposition
                (make-content-disposition "attachment" '(("filename" . "a\"b"))))))
  (ok (string= "attachment; filename*=UTF-8''%C3%A9.txt"
               (format-content-disposition
                (make-content-disposition "attachment" '(("filename" . "é.txt")))))
      ;; 0.3.4: Python's EmailMessage writes only filename* too; compat32's
      ;; get_filename() read the raw UTF-8 filename= first, as surrogates.
      "non-ASCII filename goes out as filename* alone")
  (ok (string= "attachment; filename*=UTF-8''%C3%A9"
               (format-content-disposition
                (make-content-disposition "attachment" '(("filename*" . "UTF-8''%C3%A9")))))
      "ext-value kept unquoted"))

(deftest media-type-parameter-crlf-refused
  (ok (signals (format-media-type
                (make-media-type "text" "plain"
                                 (list (cons "charset" (%crlf "utf-8" :crlf "X: y")))))
               'mime-encode-error))
  (ok (string= "text/plain; charset=\"café\""
               (format-media-type (make-media-type "text" "plain" '(("charset" . "café")))))
      "non-ASCII is quoted, not a bare token"))

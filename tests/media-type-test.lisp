(in-package #:mime-protocol/tests)

(deftest parse-media-type-vectors
  (dolist (row '(("text/plain" "text" "plain" nil)
                 ("TEXT/HTML; charset=UTF-8" "text" "html" "UTF-8")
                 ("application/json; charset=\"utf-8\"" "application" "json" "utf-8")
                 ("multipart/form-data; boundary=----x" "multipart" "form-data" nil)))
    (destructuring-bind (input type subtype charset) row
      (let ((mt (parse-media-type input)))
        (ok (string= type (media-type-type mt)))
        (ok (string= subtype (media-type-subtype mt)))
        (ok (equal charset (media-type-charset mt)))
        (ok (string= (media-type-essentials mt)
                     (format nil "~A/~A" type subtype)))))))

(deftest media-type-roundtrip
  (let* ((mt (parse-media-type "text/plain; charset=utf-8; foo=\"a b\""))
         (again (parse-media-type (format-media-type mt))))
    (ok (string= "text" (media-type-type again)))
    (ok (string= "utf-8" (media-type-charset again)))
    (ok (string= "a b" (media-type-parameter again "foo")))))

(deftest media-type-match
  (ok (media-type-match-p "text/plain" "*/*"))
  (ok (media-type-match-p "text/plain" "text/*"))
  (ok (media-type-match-p "text/plain; charset=utf-8" "text/plain"))
  (ok (media-type-match-p "text/plain; charset=utf-8" "text/plain; charset=utf-8"))
  (ng (media-type-match-p "text/plain" "text/plain; charset=utf-8"))
  (ng (media-type-match-p "application/json" "text/*")))

(deftest media-type-errors
  (ok (signals (parse-media-type "") 'mime-parse-error))
  (ok (signals (parse-media-type "noslash") 'mime-parse-error)))

(deftest disposition-filename-star
  (let ((cd (parse-content-disposition
             "attachment; filename=\"fallback.txt\"; filename*=UTF-8''na%C3%AFve.txt")))
    (ok (string= "attachment" (content-disposition-type cd)))
    (ok (string= "naïve.txt" (disposition-filename cd)))))

(deftest disposition-form-data
  (let ((cd (parse-content-disposition "form-data; name=\"upload\"; filename=\"a.bin\"")))
    (ok (string= "upload" (disposition-name cd)))
    (ok (string= "a.bin" (disposition-filename cd)))
    (ok (search "name=" (format-content-disposition cd)))))

(deftest lookup-common-types
  (ok (string= "application/json" (lookup-mime "foo.json")))
  (ok (string= "application/cbor" (lookup-mime "x.cbor")))
  (ok (string= "application/avro" (mime-type "t.avro")))
  (ok (string= "application/octet-stream" (lookup-mime "noext"))))

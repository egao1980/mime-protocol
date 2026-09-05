(in-package #:mime-protocol/tests)

(defun %utf8 (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(deftest text-entity-roundtrip
  (let* ((e (make-text-entity "hello" :subtype "plain"))
         (wired (print-mime e))
         (again (parse-mime wired)))
    (ok (search "content-type: text/plain" (string-downcase wired)))
    (ok (string= "hello" (%utf8 (mime-content again))))))

(deftest form-data-roundtrip
  (let* ((e (make-form-data '(("title" . "hi")
                              ("file" . (:filename "a.txt"
                                         :content-type "text/plain"
                                         :content "abc")))
                            :boundary "----test"))
         (wired (print-mime e))
         (again (parse-mime wired)))
    (ok (multipart-p again))
    (ok (= 2 (length (mime-parts again))))
    (let ((title (first (mime-parts again)))
          (file (second (mime-parts again))))
      (ok (string= "title" (disposition-name (mime-content-disposition title))))
      (ok (string= "hi" (%utf8 (mime-content title))))
      (ok (string= "a.txt" (disposition-filename (mime-content-disposition file))))
      (ok (string= "abc" (%utf8 (mime-content file)))))))

(deftest serdes-mime-and-multipart
  (let* ((e (make-text-entity "x"))
         (wired (serdes-protocol:encode e :format :mime))
         (back (serdes-protocol:decode wired :format :mime)))
    (ok (mime-entity-p back))
    (ok (string= "x" (%utf8 (mime-content back)))))
  (let* ((wired (serdes-protocol:encode '(("a" . "1")) :format :multipart))
         (table (serdes-protocol:decode wired :format :multipart)))
    (ok (hash-table-p table))
    (ok (string= "1" (%utf8 (gethash "a" table)))))
  (ok (string= "message/rfc822" (serdes-protocol:format-media-type :mime)))
  (ok (eq :cbor (serdes-protocol:find-format-for-media-type "application/cbor"))))

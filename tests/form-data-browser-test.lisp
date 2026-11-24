(in-package #:mime-protocol/tests)

;;; egao1980/mime-protocol#14: multipart/form-data part headers as a browser
;;; sends them (RFC 7578 §2, §4.2, §5.1.3; HTML "multipart/form-data encoding
;;; algorithm"): UTF-8 octets, no filename*, a backslash taken literally, and
;;; only " CR LF percent-encoded (%22 %0D %0A).

(defun %wire (&rest pieces)
  "PIECES as octets: a string is UTF-8, an integer one raw octet, :CRLF CR LF."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (piece pieces (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase piece
        ((eql :crlf) (vector-push-extend 13 out) (vector-push-extend 10 out))
        (integer (vector-push-extend piece out))
        (string (loop for b across (encoding-protocol:encode piece :encoding :utf-8)
                      do (vector-push-extend b out)))))))

(defun %browser-form (&rest dispositions)
  "Parse a multipart/form-data body with one part per DISPOSITIONS entry, a
list of %WIRE pieces for that part's Content-Disposition value. → the parts."
  (mime-parts
   (parse-mime
    (apply #'%wire
           "Content-Type: multipart/form-data; boundary=b" :crlf :crlf
           (append
            (loop for d in dispositions
                  append (append '("--b" :crlf "Content-Disposition: ") d
                                 '(:crlf "Content-Type: application/octet-stream" :crlf
                                   :crlf "x" :crlf)))
            '("--b--" :crlf))))))

(defun %part-name (part)
  (disposition-name (mime-content-disposition part)))

(defun %part-filename (part)
  (disposition-filename (mime-content-disposition part)))

(deftest browser-utf-8-names
  (let ((part (first (%browser-form '("form-data; name=\"имя\"; filename=\"Prélude.svg\"")))))
    (ok (equal "Prélude.svg" (%part-filename part)) "filename is UTF-8, not ISO-8859-1")
    (ok (equal "имя" (%part-name part)) "so is name")
    (ok (null (disposition-parameter (mime-content-disposition part) "filename*"))
        "a browser sends no filename*")))

(deftest browser-backslash-is-literal
  (let ((parts (%browser-form '("form-data; name=\"w\"; filename=\"a\\b.svg\"")
                              '("form-data; name=\"dir\\\"; filename=\"C:\\fakepath\\x.svg\""))))
    (ok (equal "a\\b.svg" (%part-filename (first parts))) "not ab.svg")
    (ok (equal "dir\\" (%part-name (second parts))) "a backslash before the closing quote")
    (ok (equal "C:\\fakepath\\x.svg" (%part-filename (second parts))))))

(deftest-parametrize form-data-filename-quoted-string
    ((wire expected)
     ("a\\b.svg" "a\\b.svg")
     ("a%22b.svg" "a\"b.svg")
     ("a%0D%0Ab.svg" (format nil "a~C~Cb.svg" #\Return #\Newline))
     ("100%25 %41.txt" "100%25 %41.txt")
     ("%2" "%2"))
  (ok (equal expected
             (disposition-filename
              (parse-content-disposition
               (format nil "form-data; name=\"f\"; filename=\"~A\"" wire))))
      wire))

(deftest browser-percent-escapes-decode
  (let ((part (first (%browser-form
                      '("form-data; name=\"a%22b%0D%0Ac\"; filename=\"say %22hi%22 100%25.txt\"")))))
    (ok (equal (format nil "a\"b~C~Cc" #\Return #\Newline) (%part-name part)))
    (ok (equal "say \"hi\" 100%25.txt" (%part-filename part)) "only %22 %0D %0A are escapes")))

(deftest non-utf-8-octet-falls-back-to-iso-8859-1
  (let ((parts (%browser-form (list "form-data; name=\"f\"; filename=\"caf" #xE9 ".txt\"")
                              (list "form-data; name=\"g\"; filename=\"" #xC3 #xA9 #xE9 "\""))))
    (ok (equal "café.txt" (%part-filename (first parts))) "a lone E9 is ISO-8859-1 é")
    (ok (equal (coerce (list (code-char #xC3) (code-char #xA9) (code-char #xE9)) 'string)
               (%part-filename (second parts)))
        "a field that is not valid UTF-8 is ISO-8859-1 throughout")))

(deftest our-encoder-output-parses-as-before
  ;; http-protocol's multipart writes FORMAT-FORM-DATA-DISPOSITION as UTF-8.
  (let* ((name (format nil "f\"; x=\"1~C~CX-Evil: 1" #\Return #\Newline))
         (parts (%browser-form
                 (list (format-form-data-disposition "file" :filename "résumé.pdf"))
                 (list (format-form-data-disposition name :filename "a\\b.txt"))))
         (first-cd (mime-content-disposition (first parts)))
         (second-cd (mime-content-disposition (second parts))))
    (ok (equal "résumé.pdf" (disposition-filename first-cd)))
    (ok (equal "UTF-8''r%C3%A9sum%C3%A9.pdf" (disposition-parameter first-cd "filename*")))
    (ok (equal "résumé.pdf" (disposition-parameter first-cd "filename"))
        "filename itself is UTF-8 too")
    (ok (equal name (disposition-name second-cd)) "escaped name comes back")
    (ok (equal "a\\b.txt" (disposition-filename second-cd)))
    (ok (equal '("name" "filename") (mapcar #'car (content-disposition-parameters second-cd)))
        "nothing forged"))
  (ok (equal "é.txt"
             (disposition-filename
              (parse-content-disposition
               "form-data; name=\"f\"; filename=\"fallback.txt\"; filename*=UTF-8''%C3%A9.txt")))
      "filename* still wins"))

(deftest make-form-data-round-trips-browser-names
  (let* ((name (format nil "n\"~C~Cx" #\Return #\Newline))
         (wire (print-mime (make-form-data `((,name . "v")
                                             ("w" :filename "a\\b.svg" :content "x")
                                             ("q" :filename "say \"hi\".txt" :content "y"))
                                           :boundary "b")))
         (parts (mime-parts (parse-mime wire))))
    (ok (search "filename=\"a\\b.svg\"" wire) "backslash written literally")
    (ok (search "filename=\"say %22hi%22.txt\"" wire) "quote written as %22")
    (ok (search "name=\"n%22%0D%0Ax\"" wire) "CR LF written as %0D %0A")
    (ok (equal (list name "w" "q") (mapcar #'%part-name parts)))
    (ok (equal '("a\\b.svg" "say \"hi\".txt") (mapcar #'%part-filename (rest parts))))))

(deftest quoted-pairs-outside-form-data-names
  (ok (equal "a\"b\\c.txt"
             (disposition-filename
              (parse-content-disposition "attachment; filename=\"a\\\"b\\\\c.txt\"")))
      "attachment keeps RFC 822 quoted-pairs")
  (ok (equal "a\"b"
             (disposition-parameter
              (parse-content-disposition "form-data; name=\"n\"; x=\"a\\\"b\"")
              "x"))
      "so does any other form-data parameter")
  (ok (equal "a\\b" (media-type-parameter (parse-media-type "text/plain; x=\"a\\\\b\"") "x"))
      "and a media type")
  (ok (equal "attachment; filename=\"a\\\\b\""
             (format-content-disposition
              (make-content-disposition "attachment" '(("filename" . "a\\b")))))
      "attachment output still escapes a backslash"))

(deftest utf-8-header-field-in-mail
  (let ((e (parse-mime (%wire "Subject: Prélude" :crlf "Content-Type: text/plain" :crlf :crlf "hi"))))
    (ok (equal "Prélude" (header-value e "subject")))))

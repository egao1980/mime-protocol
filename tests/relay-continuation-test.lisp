(in-package #:mime-protocol/tests)

;;; 0.3.3, after the round-2 review of #17: RFC 2231 continuations, a
;;; relay keeps non-UTF-8 header octets, :mime / :multipart serdes are
;;; binary, and smaller fixes (charset, ext-value language, case).

(defun %ja-name ()
  ;; 日本語のファイル名.pdf
  (concatenate 'string
               (coerce (mapcar #'code-char '(#x65E5 #x672C #x8A9E #x306E #x30D5 #x30A1
                                             #x30A4 #x30EB #x540D))
                       'string)
               ".pdf"))

(deftest a-long-filename-goes-out-as-continuations
  (let* ((filename (concatenate 'string (make-string 200 :initial-element (code-char #x65E5)) ".pdf"))
         (e (make-binary-entity (coerce #(1 2 3) '(vector (unsigned-byte 8))) :filename filename))
         (wire (print-mime e :max-line-length 78)))
    (ok (search "filename*0*=UTF-8''" wire))
    (ok (search "filename*1*=" wire))
    (ok (every (lambda (l) (<= (length (string-right-trim '(#\Return) l)) 78))
               (remove-if (lambda (l) (search "filename=\"" l))
                          (uiop:split-string (subseq wire 0 (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) wire))
                                             :separator '(#\Newline))))
        "every continuation line within 78")
    (ok (equal filename (disposition-filename (mime-content-disposition (parse-mime wire)))))))

(deftest continuation-segments-hold-whole-characters
  (let* ((filename (make-string 100 :initial-element (code-char #x65E5)))
         (text (format-content-disposition
                (make-content-disposition "attachment" (list (cons "filename*" filename)))))
         (segments (loop for start = (search "*=" text) then (search "*=" text :start2 (1+ start))
                         while start
                         collect (let* ((from (+ start 2))
                                        (end (or (position #\; text :start from) (length text))))
                                   (subseq text from end)))))
    (flet ((octets (segment)
             (let ((s (subseq segment (if (search "''" segment) (+ 2 (search "''" segment)) 0))))
               (coerce (loop with i = 0
                             while (< i (length s))
                             collect (if (char= #\% (char s i))
                                         (prog1 (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)
                                           (incf i 3))
                                         (prog1 (char-code (char s i)) (incf i))))
                       '(vector (unsigned-byte 8))))))
      (ok (< 1 (length segments)) (format nil "~D segments" (length segments)))
      (ok (every (lambda (s) (<= (length s) 60)) segments))
      (ok (every (lambda (s)
                   (handler-case (progn (encoding-protocol:decode-utf-8-strict (octets s)) t)
                     (error () nil)))
                 segments)
          "each segment decodes to whole UTF-8 characters"))))

(deftest-parametrize continuations-are-read
    ((value filename)
     ;; Python 3.13 EmailMessage.add_attachment(filename="日本語のファイル名.pdf")
     ("attachment; filename*0*=utf-8''%E6%97%A5%E6%9C%AC%E8%AA%9E%E3%81%AE%E3%83%95%E3%82%A1; filename*1*=%E3%82%A4%E3%83%AB%E5%90%8D.pdf"
      (%ja-name))
     ("attachment; filename*0=\"abc\"; filename*1=\"def.txt\"" "abcdef.txt")
     ("attachment; filename*1*=%C3%A9.txt; filename*0*=utf-8''caf" (format nil "caf~C.txt" (code-char #xE9)))
     ("attachment; filename*0*=utf-8''%C3%A9; filename*1=x" (format nil "~Cx" (code-char #xE9))))
  (ok (equal filename (disposition-filename (parse-content-disposition value)))
      (format nil "~S → ~S" value (disposition-filename (parse-content-disposition value)))))

(deftest broken-continuations-are-kept-as-they-came
  (let ((d (parse-content-disposition "attachment; filename*0=\"a\"; filename*2=\"b\"")))
    (ok (equal '("filename*0" "filename*2") (mapcar #'car (content-disposition-parameters d))))))

(deftest form-data-never-uses-continuations
  (let ((text (format-content-disposition
               (make-content-disposition "form-data"
                                         (list (cons "name" "f")
                                               (cons "filename" (make-string 100 :initial-element (code-char #x65E5))))))))
    ;; 0.3.6 (#21): the UTF-8 filename whole, no filename* at all.
    (ng (search "filename*" text))
    (ok (search (make-string 100 :initial-element (code-char #x65E5)) text))))

(deftest a-caller-filename-star-cannot-add-parameters
  (let* ((d (make-content-disposition "attachment" '(("filename*" . "UTF-8'; name=evil; x='abc"))))
         (back (parse-content-disposition (format-content-disposition d))))
    (ok (equal '("filename*") (mapcar #'car (content-disposition-parameters back)))
        (format nil "~S" (content-disposition-parameters back)))
    (ok (equal "UTF-8'; name=evil; x='abc" (disposition-filename back)) "the text survives, encoded")))

(deftest a-relay-keeps-latin-1-octets-until-the-field-is-set
  (let* ((raw (coerce (append (map 'list #'char-code "Subject: caf") '(#xE9 13 10 13 10)
                              (map 'list #'char-code "body"))
                      '(vector (unsigned-byte 8))))
         (e (parse-mime raw)))
    (ok (equal (format nil "caf~C" (code-char #xE9)) (header-value e "subject")) "read as ISO-8859-1")
    (ok (search #(99 97 102 233 13 10) (print-mime-octets e)) "printed as the octets it came with")
    (set-header e "Subject" (format nil "caf~C" (code-char #xE9)))
    (ok (search #(99 97 102 195 169 13 10) (print-mime-octets e)) "a value set later is UTF-8")))

(deftest header-names-are-case-insensitive
  (let ((e (make-text-entity "x")))
    (setf (mime-headers e) (list (cons "Content-Type" "text/html") (cons "X-A" "1")))
    (ok (equal "1" (header-value e "x-a")))
    (set-header e "x-a" "2")
    (ok (= 2 (length (mime-headers e))) "set-header replaces the capitalised field")
    (let ((wire (print-mime e)))
      (ok (= 1 (count-if (lambda (l) (eql 0 (search "Content-Type:" l)))
                         (uiop:split-string wire :separator '(#\Newline))))
          "one Content-Type"))))

(deftest make-text-entity-encodes-in-its-charset
  (ok (equalp #(99 97 102 233)
              (mime-content (make-text-entity (format nil "caf~C" (code-char #xE9)) :charset "iso-8859-1"))))
  (ok (equalp #(99 97 102 195 169)
              (mime-content (make-text-entity (format nil "caf~C" (code-char #xE9))))))
  (ok (signals (make-text-entity "x" :charset "no-such-charset") 'mime-encode-error)))

(deftest encode-encoded-words-refuses-line-breaks
  (ok (signals (encode-encoded-words (format nil "a~C~Cb" #\Return #\Newline)) 'mime-header-error)))

(deftest parse-mime-refuses-a-wide-string
  (ok (signals (parse-mime (format nil "Subject: ~C~C~C~Cx" (code-char #x2603) #\Return #\Newline #\Return))
               'mime-parse-error)))

(deftest mime-serdes-is-binary
  (let ((e (make-text-entity (format nil "caf~C" (code-char #xE9)))))
    (set-header e "Subject" (format nil "~C" (code-char #x2603)))
    (ok (serdes-protocol:format-binary-p :mime))
    (ok (serdes-protocol:format-binary-p :multipart))
    (let ((octets (serdes-protocol:encode e :format :mime)))
      (ok (typep octets '(vector (unsigned-byte 8))) "encode gives octets")
      (ok (equalp octets (print-mime-octets e))))
    (uiop:with-temporary-file (:pathname path :keep nil)
      (with-open-file (file path :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
        (serdes-protocol:stream-encode-value (serdes-protocol:make-output-stream file :format :mime) e))
      (let ((back (with-open-file (file path :element-type '(unsigned-byte 8))
                    (serdes-protocol:stream-decode-value
                     (serdes-protocol:make-input-stream file :format :mime)))))
        (ok (equal (string (code-char #x2603)) (header-value back "subject"))
            "binary stream round trip")))
    (let* ((text (with-output-to-string (s)
                   (serdes-protocol:stream-encode-value
                    (serdes-protocol:make-output-stream s :format :mime :element-type 'character) e)))
           (back (serdes-protocol:stream-decode-value
                  (serdes-protocol:make-input-stream (make-string-input-stream text)
                                                     :format :mime :element-type 'character))))
      (ok (equal (string (code-char #x2603)) (header-value back "subject")) "character stream round trip"))))

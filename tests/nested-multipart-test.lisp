(in-package #:mime-protocol/tests)

;;; egao1980/mime-protocol#9: a part whose content ends in CRLF (a nested
;;; multipart always does) must parse, and keep its CRLF. Also canonical
;;; header names on print.

(defun %crlf-text (&rest lines)
  "LINES joined with CRLF."
  (with-output-to-string (s)
    (loop for (l . more) on lines
          do (write-string l s)
             (when more
               (write-char #\Return s)
               (write-char #\Newline s)))))

(defun %type-of (entity)
  (let ((mt (mime-content-type entity)))
    (format nil "~(~A/~A~)" (media-type-type mt) (media-type-subtype mt))))

(defun %mail-layout ()
  (make-multipart-entity
   (list (make-multipart-entity
          (list (make-text-entity "hello")
                (make-text-entity "<p>hello</p>" :subtype "html"))
          :subtype "alternative")
         (make-binary-entity (coerce #(1 2 3) '(vector (unsigned-byte 8)))
                             :subtype "pdf" :filename "r.pdf"))
   :subtype "mixed"))

(deftest nested-multipart-round-trips
  (let* ((parsed (parse-mime (print-mime (%mail-layout))))
         (parts (mime-parts parsed))
         (alt (first parts)))
    (ok (equal "multipart/mixed" (%type-of parsed)))
    (ok (equal '("multipart/alternative" "application/pdf") (mapcar #'%type-of parts)))
    (ok (equal '("text/plain" "text/html") (mapcar #'%type-of (mime-parts alt))))
    (ok (equal '("hello" "<p>hello</p>")
               (mapcar (lambda (p) (%utf8 (mime-content p))) (mime-parts alt))))
    (ok (equalp #(1 2 3) (mime-content (second parts))))
    (ok (equal "r.pdf" (disposition-filename (mime-content-disposition (second parts)))))))

(deftest part-ending-in-crlf-keeps-it
  (let* ((text (format nil "line one~C~C" #\Return #\Newline))
         (parsed (parse-mime
                  (print-mime
                   (make-multipart-entity (list (make-text-entity text)
                                                (make-text-entity "two"))))))
         (parts (mime-parts parsed)))
    (ok (= 2 (length parts)))
    (ok (equal text (%utf8 (mime-content (first parts)))) "the part's own CRLF survives")
    (ok (equal "two" (%utf8 (mime-content (second parts)))))))

(deftest-parametrize part-content-round-trips
    ((content)
     ("") ("x") ((format nil "~C~C" #\Return #\Newline))
     ((format nil "a~C~C~C~Cb~C~C" #\Return #\Newline #\Return #\Newline #\Return #\Newline))
     ((format nil "--not-the-boundary~C~C" #\Return #\Newline)))
  (let* ((e (make-multipart-entity (list (make-text-entity content)) :boundary "b"))
         (back (first (mime-parts (parse-mime (print-mime e))))))
    (ok (equal content (%utf8 (mime-content back))) (format nil "~S" content))))

(deftest printed-output-is-a-fixed-point
  (let* ((once (print-mime (%mail-layout)))
         (twice (print-mime (parse-mime once))))
    (ok (string= once twice) "print ∘ parse ∘ print = print")))

(deftest preamble-and-epilogue-round-trip
  (let* ((wire (%crlf-text "Content-Type: multipart/mixed; boundary=b" ""
                           "This is the preamble."
                           "--b" "" "one"
                           "--b--"
                           "epilogue"))
         (e (parse-mime wire)))
    (ok (equal "This is the preamble." (%utf8 (mime-preamble e))) "no delimiter CRLF")
    (ok (equal '("one") (mapcar (lambda (p) (%utf8 (mime-content p))) (mime-parts e))))
    (ok (equal "epilogue" (%utf8 (mime-epilogue e))))
    (ok (string= (print-mime e) (print-mime (parse-mime (print-mime e)))))))

(deftest boundary-prefix-of-another-is-not-a-delimiter
  ;; "--b" inside "--b2" (a nested part's boundary) must not end the outer part.
  (let* ((inner (make-multipart-entity (list (make-text-entity "in")) :boundary "b2"))
         (outer (make-multipart-entity (list inner (make-text-entity "out")) :boundary "b"))
         (parsed (parse-mime (print-mime outer))))
    (ok (equal '("multipart/mixed" "text/plain") (mapcar #'%type-of (mime-parts parsed))))
    (ok (equal "in" (%utf8 (mime-content (first (mime-parts (first (mime-parts parsed))))))))))

(deftest lf-only-multipart-parses
  (let* ((wire (format nil "Content-Type: multipart/mixed; boundary=b~%~%--b~%~%one~%--b~%~%two~%--b--~%"))
         (e (parse-mime wire)))
    (ok (equal '("one" "two") (mapcar (lambda (p) (%utf8 (mime-content p))) (mime-parts e))))))

(deftest-parametrize canonical-header-names
    ((name expected)
     ("from" "From")
     ("subject" "Subject")
     ("mime-version" "MIME-Version")
     ("message-id" "Message-ID")
     ("content-transfer-encoding" "Content-Transfer-Encoding")
     ("content-md5" "Content-MD5")
     ("dkim-signature" "DKIM-Signature")
     ("x-mailer" "X-Mailer")
     ("Content-Type" "Content-Type"))
  (ok (equal expected (canonical-header-name name))))

(deftest print-mime-writes-canonical-names
  (let* ((e (make-text-entity "hi")))
    (set-header e "mime-version" "1.0")
    (let ((wire (print-mime e)))
      (ok (search "MIME-Version: 1.0" wire))
      (ok (search "Content-Type: text/plain" wire))
      (ok (search "Content-Transfer-Encoding: 8bit" wire))
      (ok (equal "1.0" (header-value (parse-mime wire) "MIME-Version"))
          "lookups stay case-insensitive"))))

(in-package #:mime-protocol/tests)

;;; egao1980/mime-protocol#12: a CR or LF in a header value forged further
;;; fields (header injection). SET-HEADER refuses it and PRINT-MIME never
;;; writes one, as Python's EmailMessage does ("Header values may not
;;; contain linefeed or carriage return characters"). Long values are
;;; folded instead (RFC 5322 2.2.3). Also #16: PRINT-MIME's string is octets,
;;; so PARSE-MIME reads it back whatever the header text.

(defun %cr-lf (&rest parts)
  "PARTS joined with CR LF."
  (format nil "~{~A~^~C~C~}"
          (loop for (p . more) on parts
                collect p
                when more append (list #\Return #\Newline))))

(defun %header-lines (wire)
  "The header block of WIRE (up to the first blank line) split on CRLF."
  (let ((end (search (%cr-lf "" "" "") wire)))
    (uiop:split-string (subseq wire 0 end) :separator '(#\Newline))))

(defun %only-crlf-p (text)
  "Is every CR in TEXT followed by LF, and every LF preceded by CR?"
  (loop for i from 0 below (length text)
        for c = (char text i)
        always (case c
                 (#\Return (and (< (1+ i) (length text))
                                (char= #\Newline (char text (1+ i)))))
                 (#\Newline (and (plusp i) (char= #\Return (char text (1- i)))))
                 (t t))))

(deftest-parametrize set-header-refuses-line-breaks
    ((value)
     ((%cr-lf "Hi" "Bcc: eve@example.com" "" "Injected body"))
     ((format nil "Hi~CBcc: eve@example.com" #\Return))
     ((format nil "Hi~CBcc: eve@example.com" #\Newline))
     ((format nil "Hi~C~C folded" #\Return #\Newline))
     ((format nil "x~C" #\Newline))
     ((format nil "nul~Chere" (code-char 0))))
  (let ((e (make-text-entity "body")))
    (ok (signals (set-header e "Subject" value) 'mime-header-error) (format nil "~S" value))
    (ok (null (header-value e "subject")) "nothing stored")
    (ok (typep (handler-case (set-header e "Subject" value) (error (c) c)) 'mime-encode-error)
        "a mime-encode-error, so existing handlers see it")))

(deftest-parametrize set-header-refuses-bad-names
    ((name)
     ("Bcc: eve@example.com") ((%cr-lf "X" "Bcc")) ("two words") ("") ((format nil "X~CY" #\Tab)))
  (ok (signals (set-header (make-text-entity "x") name "v") 'mime-header-error) (format nil "~S" name)))

(deftest set-header-error-names-the-field
  (let ((c (handler-case (set-header (make-text-entity "x") "Subject" (%cr-lf "a" "b"))
             (mime-header-error (c) c))))
    (ok (equal "Subject" (mime-header-error-name c)))
    (ok (equal (%cr-lf "a" "b") (mime-header-error-value c)))
    (ok (search "CR" (princ-to-string c)) (princ-to-string c))))

(deftest set-header-use-value-restart
  ;; The restarting forms run outside OK: Rove's own handler would see the
  ;; error first.
  (let* ((e (make-text-entity "x"))
         (returned (handler-bind ((mime-header-error (lambda (c) (use-value "Hi" c))))
                     (set-header e "Subject" (%cr-lf "Hi" "Bcc: eve@example.com")))))
    (ok (equal "Hi" returned) "set-header returns the replacement")
    (ok (equal "Hi" (header-value e "subject")) "the replacement is stored")
    (let ((tries 0))
      (handler-bind ((mime-header-error
                      (lambda (c)
                        (incf tries)
                        (use-value (if (= tries 1) (%cr-lf "still" "bad") "fine") c))))
        (set-header e "Subject" (%cr-lf "a" "b")))
      (ok (= 2 tries) "a replacement is checked again")
      (ok (equal "fine" (header-value e "subject"))))
    (handler-bind ((mime-header-error (lambda (c) (use-value "X-Ok" c))))
      (set-header e "X Bad" "v"))
    (ok (equal "v" (header-value e "x-ok")) "a name restart too")))

(deftest print-mime-refuses-a-line-break-set-behind-set-header
  (let ((e (make-text-entity "x")))
    (setf (mime-headers e) (list (cons "subject" (%cr-lf "Hi" "Bcc: eve@example.com"))))
    (ok (signals (print-mime e) 'mime-header-error))
    (let ((out (make-string-output-stream)))
      (ok (signals (print-mime e :stream out) 'mime-header-error))
      (ok (equal "" (get-output-stream-string out)) "nothing written to the stream"))
    (let ((wire (handler-bind ((mime-header-error (lambda (c) (use-value "Hi" c))))
                  (print-mime e))))
      (ok (search (%cr-lf "Subject: Hi" "") wire) "use-value prints the replacement")
      (ng (search "Bcc" wire)))))

(deftest print-mime-refuses-a-parsed-bare-cr
  ;; PARSE-HEADERS splits on LF, so a bare CR stays in the value. A server
  ;; that ends a line at a bare CR would read "Bcc: eve" as a field.
  (let ((e (parse-mime (format nil "Subject: Hi~CBcc: eve@example.com~C~C~C~Cbody"
                               #\Return #\Return #\Newline #\Return #\Newline))))
    (ok (signals (print-mime e) 'mime-header-error)))
  (let ((e (parse-mime (format nil "X~CBcc: eve@example.com~C~C~C~Cbody"
                               #\Return #\Return #\Newline #\Return #\Newline))))
    (ok (signals (print-mime e) 'mime-header-error) "a bare CR in a parsed name")))

(deftest printed-text-has-crlf-line-ends-only
  (let ((e (make-multipart-entity
            (list (make-text-entity "a")
                  (make-binary-entity (coerce #(13 10 13 0 255) '(vector (unsigned-byte 8)))
                                      :filename "x.bin"))
            :boundary "b")))
    (set-header e "Subject" (format nil "~{~A~^ ~}" (loop repeat 100 collect "word")))
    (set-header e "X-Token" (make-string 1200 :initial-element #\x))
    (ok (%only-crlf-p (print-mime e)) "headers, folded headers, delimiters and a base64 part")
    (ok (%only-crlf-p (print-mime e :max-line-length 78)))
    (ok (%only-crlf-p (print-mime (make-text-entity "x" :filename (make-string 2000 :initial-element #\y))))
        "a long parameter")))

(deftest-parametrize long-header-values-fold-and-unfold
    ((value limit)
     ((format nil "~{~A~^ ~}" (loop for i below 60 collect (format nil "word~D" i))) 78)
     ((format nil "~{~A~^ ~}" (loop for i below 300 collect (format nil "w~D" i))) 998)
     ((format nil "short and ~A" (make-string 120 :initial-element #\z)) 78)
     ((format nil "~{~A~^ ~}" (loop for i below 40 collect (format nil "Caf~C~D" (code-char #xE9) i))) 78))
  (let* ((e (make-text-entity "body"))
         (_ (set-header e "Subject" value))
         (wire (print-mime e :max-line-length limit))
         (subject-lines (loop for line in (%header-lines wire)
                              for l = (string-right-trim '(#\Return) line)
                              with in = nil
                              do (setf in (or (eql 0 (search "Subject:" l))
                                              (and in (plusp (length l)) (char= #\Space (char l 0)))))
                              when in collect l)))
    (declare (ignore _))
    (ok (every (lambda (l) (or (<= (length l) limit)
                               (not (find #\Space l :start (1+ (or (position #\: l) 0))))))
               subject-lines)
        (format nil "lines within ~D where there is a space to cut at: ~S"
                limit (mapcar #'length subject-lines)))
    (ok (notany (lambda (l) (zerop (length (string-trim " " l)))) subject-lines) "no blank line")
    (ok (equal value (header-value (parse-mime wire) "subject")) "unfolds to the value")))

(deftest no-folding-without-a-space-or-with-nil
  (let* ((token (make-string 120 :initial-element #\t))
         (e (make-text-entity "b")))
    (set-header e "X-Token" token)
    (ok (search (format nil "X-Token: ~A~C~C" token #\Return #\Newline) (print-mime e :max-line-length 78))
        "a token longer than the line stays whole")
    (set-header e "X-Words" (format nil "~{~A~^ ~}" (loop repeat 40 collect "abc")))
    (ok (= 1 (count-if (lambda (l) (eql 0 (search "X-Words" l)))
                       (%header-lines (print-mime e :max-line-length nil))))
        "NIL: no folding")
    (ok (notany (lambda (l) (and (plusp (length l)) (char= #\Space (char l 0))))
                (%header-lines (print-mime e)))
        "short fields are not folded at the default 998")))

(deftest print-mime-string-is-octets
  (let* ((subject (format nil "hi ~C~C ~C" (code-char #x41F) (code-char #x2603) (code-char #xE9)))
         (e (make-text-entity (format nil "body ~C~C" (code-char #xE9) (code-char #x2615)))))
    (set-header e "Subject" subject)
    (let ((wire (print-mime e)))
      (ok (every (lambda (c) (< (char-code c) 256)) wire) "one character per octet")
      (ok (equalp (print-mime-octets e) (encoding-protocol:encode wire :encoding :iso-8859-1))
          "print-mime-octets is that string's octets")
      (ok (search (encoding-protocol:encode subject :encoding :utf-8) (print-mime-octets e))
          "header text is UTF-8 on the wire")
      (ok (equal subject (header-value (parse-mime wire) "subject")) "string round trip")
      (ok (equal subject (header-value (parse-mime (print-mime-octets e)) "subject"))
          "octet round trip")
      (ok (equal (format nil "body ~C~C" (code-char #xE9) (code-char #x2615))
                 (%utf8 (mime-content (parse-mime wire)))))
      (ok (string= wire (print-mime (parse-mime wire))) "a fixed point"))))

(deftest form-data-filename-above-latin-1-round-trips
  ;; The #16 repro: filename="a—b" (U+2014).
  (let* ((name (coerce (list #\a (code-char 8212) #\b) 'string))
         (wire (print-mime (make-form-data (list (list "f" :filename name :content "x"))
                                           :boundary "b")))
         (part (first (mime-parts (parse-mime wire)))))
    (ok (equal name (disposition-filename (mime-content-disposition part))))
    (ok (equal "f" (disposition-name (mime-content-disposition part))))
    (ok (string= wire (print-mime (parse-mime wire))) "print ∘ parse ∘ print = print")))

(deftest a-part-holding-the-delimiter-is-refused
  (let* ((forged (%cr-lf "hello" "--b" "Content-Type: text/html" "" "<p>phish</p>"))
         (e (make-multipart-entity (list (make-text-entity forged) (make-text-entity "two"))
                                   :boundary "b")))
    (ok (signals (print-mime e) 'mime-encode-error) "part 0 holds --b on a line")
    (handler-bind ((mime-encode-error (lambda (c) (use-value "c2" c))))
      (let ((parts (mime-parts (parse-mime (print-mime e)))))
        (ok (= 2 (length parts)) "a new boundary keeps two parts")
        (ok (equal forged (%utf8 (mime-content (first parts)))) "and the content")))
    (ok (equal "c2" (media-type-boundary (mime-content-type e))))))

(deftest a-preamble-holding-the-delimiter-is-refused
  (let ((e (make-multipart-entity (list (make-text-entity "one")) :boundary "b")))
    (setf (mime-preamble e) (%cr-lf "pre" "--b" "" "forged"))
    (ok (signals (print-mime e) 'mime-encode-error))))

(deftest default-boundaries-are-unpredictable
  (let ((boundaries (loop repeat 50
                          collect (media-type-boundary
                                   (mime-content-type (make-multipart-entity '()))))))
    (ok (= 50 (length (remove-duplicates boundaries :test #'string=))))
    (ok (every (lambda (b) (>= (length b) 25)) boundaries) (format nil "~S" (first boundaries)))))

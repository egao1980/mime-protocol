(in-package #:mime-protocol/tests)

;;; Adversarial review, round 1, of egao1980/mime-protocol#17 (0.3.2: #12
;;; header injection, #16 octet print). Each test is a break found against
;;; the PR head. Where refusing the input would also be a fix, the test
;;; accepts the refusal.
;;;
;;; From the scratch branch agent/adv-mime-e978 (3a3b9a7). Its consumer
;;; test r1-print-mime-through-a-utf-8-stream-keeps-header-text is not
;;; here: it asks that PRINT-MIME's string survive a UTF-8 encoder, the
;;; opposite of the #16 fix (an octet string). The consumer it models,
;;; mail-backend-smtp, writes the string through ISO-8859-1 instead
;;; (egao1980/mail-protocol#6, tests utf8-wire-test and
;;; smtp-wire-headers-are-ascii-and-read-back).

(defun %r1-octets (&rest items)
  "Octets from ITEMS: an integer is one octet, a string its characters as
ISO-8859-1 octets."
  (coerce (loop for x in items
                append (if (integerp x)
                           (list x)
                           (map 'list #'char-code x)))
          '(vector (unsigned-byte 8))))

(defun %r1-header-block (wire)
  "The header lines of WIRE, up to the first blank line, without CRLF."
  (let ((lines (uiop:split-string wire :separator '(#\Newline))))
    (loop for line in lines
          for l = (string-right-trim '(#\Return) line)
          until (zerop (length l))
          collect l)))

(defun %r1-ftext-name-p (line)
  "Does header LINE start with an RFC 5322 field-name, 1*ftext then \":\"?
(The callers' fields are short, so none is folded.)"
  (let ((colon (position #\: line)))
    (and colon
         (plusp colon)
         (every (lambda (c) (and (<= 33 (char-code c) 126) (char/= c #\:)))
                (subseq line 0 colon)))))

;;; Regressions against 0.3.1 (c7e6d5a passes each of these).

(deftest r1-non-ascii-boundary-survives-print-parse
  ;; Header text is now UTF-8 (C3 A9) in the Content-Type field and in the
  ;; delimiter lines, but PARSE-MIME searches the body for the boundary as
  ;; ISO-8859-1 octets (E9): "multipart body missing boundary".
  (let* ((boundary (format nil "b~C" (code-char #xE9)))
         (e (make-multipart-entity (list (make-text-entity "one") (make-text-entity "two"))
                                   :boundary boundary))
         (wire (handler-case (print-mime e) (mime-encode-error () :refused)))
         (parsed (if (eq wire :refused)
                     :refused
                     (handler-case (parse-mime wire) (error (c) c)))))
    (ok (or (eq parsed :refused)
            (and (typep parsed 'mime-entity) (= 2 (length (mime-parts parsed)))))
        (format nil "boundary ~S: refused, or print → parse keeps 2 parts (got ~A)"
                boundary (if (typep parsed 'mime-entity) (length (mime-parts parsed)) parsed)))))

(deftest r1-parsed-latin-1-boundary-reprints-parseable
  ;; A received message whose boundary is one ISO-8859-1 octet parses, but
  ;; PRINT-MIME re-writes the boundary as UTF-8 and the result no longer
  ;; parses: a parse → print relay loses the message.
  (let* ((raw (%r1-octets "Content-Type: multipart/mixed; boundary=\"b" #xE9 "\"" 13 10 13 10
                          "--b" #xE9 13 10 "Content-Type: text/plain" 13 10 13 10 "one" 13 10
                          "--b" #xE9 "--" 13 10))
         (first-parse (parse-mime raw))
         (wire (handler-case (print-mime first-parse) (mime-encode-error () :refused)))
         (again (if (eq wire :refused)
                    :refused
                    (handler-case (parse-mime wire) (error (c) c)))))
    (ok (= 1 (length (mime-parts first-parse))) "the received message parses")
    (ok (or (eq again :refused)
            (and (typep again 'mime-entity) (= 1 (length (mime-parts again)))))
        (format nil "refused, or parse → print → parse keeps the part (got ~A)"
                (if (typep again 'mime-entity) (length (mime-parts again)) again)))))

(deftest r1-parsed-non-ascii-field-name-is-a-fixed-point
  ;; PARSE-MIME reads a field NAME as ISO-8859-1 (only values get the UTF-8
  ;; reading), PRINT-MIME writes it as UTF-8: each print → parse cycle
  ;; encodes the name again, "x-café" → "x-cafã©" → "x-cafã£â©".
  (let* ((raw (%r1-octets "X-Caf" #xE9 ": v" 13 10 13 10 "body"))
         (one (parse-mime raw))
         (wire (handler-case (print-mime one) (mime-header-error () :refused))))
    (if (eq wire :refused)
        (pass "a non-ftext field name is refused")
        (let* ((two (parse-mime wire))
               (three (parse-mime (print-mime two))))
          (ok (equal (mime-headers one) (mime-headers two))
              (format nil "first cycle: ~S → ~S" (mime-headers one) (mime-headers two)))
          (ok (equal (mime-headers two) (mime-headers three))
              (format nil "second cycle: ~S → ~S" (mime-headers two) (mime-headers three)))))))

;;; Gaps in what the PR says it guarantees (0.3.1 has them too).

(deftest r1-print-mime-refuses-a-name-starting-with-a-blank
  ;; PRINT-MIME's own name check (%PRINTED-NAME-PROBLEM) is for "what would
  ;; break the field structure" reaching the headers via (setf mime-headers)
  ;; or :headers. A name starting with SP or HTAB prints as a continuation
  ;; line: the field disappears into the one before it (RFC 5322 2.2.3; our
  ;; parser and Python's both read Subject "Hi Bcc: eve@example.com").
  (dolist (name (list " bcc" (format nil "~Cbcc" #\Tab) " "))
    (let ((e (make-text-entity "x")))
      (setf (mime-headers e) (list (cons "subject" "Hi") (cons name "eve@example.com")))
      (let ((wire (handler-case (print-mime e) (mime-header-error () :refused))))
        (ok (or (eq wire :refused)
                (equal "Hi" (header-value (parse-mime wire) "subject")))
            (format nil "name ~S: refused, or Subject stays \"Hi\" (got ~S)" name
                    (if (stringp wire) (header-value (parse-mime wire) "subject") wire)))))))

(deftest r1-print-mime-writes-only-field-names
  ;; A name with an inner space or a non-ASCII character also passes
  ;; PRINT-MIME's check. An RFC 5322 receiver ends the header block at that
  ;; line: Python's email parser (3.12, policy.default) reports
  ;; MissingHeaderBodySeparatorDefect and puts X-After, Content-Type and
  ;; Content-Transfer-Encoding in the body, the #12 symptom.
  (dolist (name (list "x bcc" (format nil "x-caf~C" (code-char #xE9))))
    (let ((e (make-text-entity "x")))
      (setf (mime-headers e) (list (cons "subject" "Hi") (cons name "v") (cons "x-after" "w")))
      (let ((wire (handler-case (print-mime e) (mime-header-error () :refused))))
        (ok (or (eq wire :refused)
                (every #'%r1-ftext-name-p (%r1-header-block wire)))
            (format nil "name ~S: refused, or every header line has an ftext name (got ~S)"
                    name (if (stringp wire) (%r1-header-block wire) wire)))))))

(deftest r1-print-mime-writes-nothing-before-a-body-error
  ;; PRINT-MIME's docstring: "Everything is checked before anything is
  ;; written to STREAM." The body is encoded inside EMIT, after the header
  ;; block went out, so a body that cannot be encoded leaves a partial
  ;; entity on STREAM.
  (let ((e (make-text-entity "x"))
        (out (make-string-output-stream)))
    (setf (mime-transfer-encoding e) :bogus)
    (let ((err (handler-case (progn (print-mime e :stream out) nil) (mime-error (c) c))))
      (ok err (format nil "signals: ~A" err))
      (ok (equal "" (get-output-stream-string out)) "nothing written to the stream"))))

(deftest r1-unfoldable-header-line-stays-within-998
  ;; PRINT-MIME's default :max-line-length 998 is "RFC 5322 2.1.1's MUST",
  ;; but a run with no space is left whole, so the line is longer than 998
  ;; and an MTA may refuse to relay it (Exim's smtp transport bounces lines
  ;; over 998 since 4.95). Python's EmailMessage (policy.SMTP, also with
  ;; max_line_length=998) keeps the same Subject within the limit by
  ;; splitting it into encoded-words. A refusal would also do.
  (let ((e (make-text-entity "x")))
    (set-header e "Subject" (make-string 1200 :initial-element #\x))
    (let ((wire (handler-case (print-mime e) (mime-header-error () :refused))))
      (ok (or (eq wire :refused)
              (every (lambda (l) (<= (length l) 998)) (%r1-header-block wire)))
          (format nil "refused, or no header line over 998 octets (longest ~A)"
                  (if (stringp wire)
                      (reduce #'max (mapcar #'length (%r1-header-block wire)))
                      wire))))))

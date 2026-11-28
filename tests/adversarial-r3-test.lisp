(in-package #:mime-protocol/tests)

;;; Adversarial review, round 3, of egao1980/mime-protocol#18 (head 40ad14d,
;;; 0.3.3: binary serdes, RFC 2231 continuations, relay keeps octets). Each
;;; test is a break found against that head. Helpers %R2-OCTETS and
;;; %R2-HEADER-LINES come from adversarial-r2-test.lisp.
;;;
;;; Python 3.13.18 comparisons (email, python-multipart 0.0.x, Werkzeug
;;; 3.x), 2026-11-24.

(defun %r3-header-block (octets)
  "OCTETS up to the blank line that ends the header block."
  (subseq octets 0 (or (search #(13 10 13 10) octets) (length octets))))

(defun %r3-text (octets)
  "OCTETS as text, one character per octet, for a failure message."
  (map 'string #'code-char octets))

;;; The relay keeps octets per field name, not per field.

(deftest r3-duplicate-fields-keep-their-own-octets
  ;; Two X-A fields: the first valid UTF-8 ("café", C3 A9), the second raw
  ;; ISO-8859-1 (E9). RAW-FIELDS records the name "x-a", so PRINT-MIME
  ;; writes both as one octet per character: the UTF-8 field goes out as
  ;; "caf" E9 and is no longer UTF-8. 0.3.2 wrote it back as C3 A9. Received,
  ;; Comments, Resent-*, X-* fields repeat. Python 3.13 keeps both (compat32:
  ;; as they came; policy.default: each as its own unknown-8bit word).
  ;; SET-HEADER on the name replaces the first field and drops the flag, so
  ;; the second, untouched, raw field is rewritten as UTF-8 of its
  ;; ISO-8859-1 reading.
  (let* ((utf-8 (%r2-octets "X-A: caf" #xC3 #xA9 13 10))
         (latin-1 (%r2-octets "X-A: caf" #xE9 13 10))
         (raw (%r2-octets utf-8 latin-1 13 10 "body"))
         (e (parse-mime raw))
         (wire (%r3-header-block (print-mime-octets e))))
    (ok (search utf-8 wire)
        (format nil "the UTF-8 field goes out as UTF-8 (got ~S)" (%r3-text wire)))
    (ok (search (subseq latin-1 0 9) wire)
        "the ISO-8859-1 field goes out as its octets")
    (set-header e "x-a" "new")
    (let ((wire (%r3-header-block (print-mime-octets e))))
      (ok (search (subseq latin-1 0 9) wire)
          (format nil "the second, untouched field keeps its octets after set-header of the first (got ~S)"
                  (%r3-text wire))))))

(deftest r3-relay-keeps-raw-content-type-and-disposition-octets
  ;; Content-Type name= and Content-Disposition filename= in raw windows-1251
  ;; (CF F0 E8 = "При"), as Outlook Express / The Bat! wrote them. PRINT-MIME
  ;; rebuilds both fields from the parsed objects (%ENSURE-CONTENT-TYPE-HEADER
  ;; calls SET-HEADER), so RAW-FIELDS never applies: the relay writes the
  ;; UTF-8 of the ISO-8859-1 reading, C3 8F C3 B0 C3 A8 ("Ïðè"), and adds
  ;; filename*=UTF-8''%C3%8F%C3%B0%C3%A8.txt, which readers prefer. 0.3.1
  ;; kept both octet runs (the Content-Type one wholly). Python 3.13 keeps the
  ;; octets: compat32 as they came, policy.default as
  ;; name*=unknown-8bit''%CF%F0%E8.txt / filename*=unknown-8bit''%CF%F0%E8.txt.
  (let* ((name (%r2-octets #xCF #xF0 #xE8 ".txt"))
         (raw (%r2-octets "Content-Type: text/plain; charset=windows-1251; name=\"" name "\"" 13 10
                          "Content-Disposition: attachment; filename=\"" name "\"" 13 10
                          13 10 "body"))
         (wire (%r3-header-block (print-mime-octets (parse-mime raw))))
         (fields (let ((acc '()))
                   ;; One entry per field, folded lines joined.
                   (dolist (line (%r2-header-lines (%r3-text wire)) (nreverse acc))
                     (if (and acc (plusp (length line)) (member (char line 0) '(#\Space #\Tab)))
                         (setf (first acc) (concatenate 'string (first acc) line))
                         (push line acc)))))
         (fields (mapcar (lambda (field) (map '(vector (unsigned-byte 8)) #'char-code field))
                         fields)))
    (flet ((keeps-p (prefix)
             (let ((field (find-if (lambda (f) (eql 0 (search (%r2-octets prefix) f))) fields)))
               (and field
                    (or (search name field)
                        (search (%r2-octets "%CF%F0%E8.txt") field))))))
      (ok (keeps-p "Content-Type:")
          (format nil "Content-Type keeps the name's octets (got ~S)" (%r3-text wire)))
      (ok (keeps-p "Content-Disposition:")
          "Content-Disposition keeps the filename's octets")
      (ok (not (or (search (%r2-octets #xC3 #x8F #xC3 #xB0 #xC3 #xA8) wire)
                   (search (%r2-octets "%C3%8F%C3%B0%C3%A8") wire)))
          "no field carries the UTF-8 of the ISO-8859-1 reading"))))

(deftest r3-raw-field-survives-moving-headers-to-a-new-entity
  ;; mail-protocol's ADD-ATTACHMENT / ADD-ALTERNATIVE on a parsed message
  ;; (%WRAP-IN-MULTIPART) move the non-Content-* fields to a new multipart
  ;; entity through the exported (SETF MIME-HEADERS). RAW-FIELDS (internal,
  ;; not exported) stays on the old entity, so the raw windows-1251 Subject
  ;; "Привет" goes out as C3 8F C3 B0 ... ("Ïðèâåò"): the r2 relay break
  ;; again, one call later. Checked through mail-protocol 0.3.0 with this
  ;; branch: PARSE-MESSAGE → PRINT-MESSAGE keeps CF F0 E8 E2 E5 F2,
  ;; PARSE-MESSAGE → ADD-ATTACHMENT → PRINT-MESSAGE does not. 0.3.1 kept the
  ;; octets. Python 3.13 parse → add_attachment → as_bytes keeps them too
  ;; (compat32 as they came, policy.default as =?unknown-8bit?b?z/Do4uXy?=).
  (let* ((field (%r2-octets #xCF #xF0 #xE8 #xE2 #xE5 #xF2))
         (old (parse-mime (%r2-octets "Subject: " field 13 10
                                      "Content-Type: text/plain; charset=windows-1251" 13 10
                                      13 10 "body")))
         (new (make-multipart-entity (list old) :boundary "b")))
    (setf (mime-headers new) (remove "content-type" (mime-headers old) :key #'car :test #'string-equal)
          (mime-headers old) (remove "subject" (mime-headers old) :key #'car :test #'string-equal))
    (let ((wire (%r3-header-block (print-mime-octets new))))
      (ok (or (search (%r2-octets "Subject: " field 13 10) wire)
              (equalp field (%r2-b-word-octets (header-value (parse-mime (print-mime-octets new))
                                                             "subject"))))
          (format nil "the moved Subject keeps its octets (got ~S)" (%r3-text wire))))))

;;; RFC 2231 continuations on parse (new in 0.3.3).

(deftest-parametrize r3-form-data-continuations-do-not-override-a-plain-parameter
    ((value)
     ("form-data; name*0=\"ad\"; name*1=\"min\"; name=\"user\"")
     ("form-data; name=\"f\"; filename*0=\"evil.exe\"; filename=\"safe.txt\""))
  ;; A continuation must never override the plain parameter. 0.3.3 let the
  ;; plain one win ("user" / "safe.txt", as python-multipart reads it), but
  ;; Werkzeug joins name*0 / filename*0, so no single reading matches every
  ;; server in a chain. 0.3.6 (#21) refuses the disposition instead.
  (ok (signals (parse-content-disposition value) 'mime-parse-error) value))

;;; Long filenames still cannot be printed.

(deftest-parametrize r3-long-filenames-print-and-read-back
    ((label filename)
     ("1000 ASCII" (make-string 1000 :initial-element #\a))
     ("400 CJK" (make-string 400 :initial-element (code-char #x4E2D))))
  ;; Fix 2 continues only filename*. A plain filename= is one quoted run: an
  ;; ASCII name over ~985 characters, or a non-ASCII one over ~330 CJK
  ;; characters (FORMAT-CONTENT-DISPOSITION writes its raw UTF-8 next to
  ;; filename*), stays a line over 998 and PRINT-MIME refuses the
  ;; attachment, at 78 and at 998. 0.3.1 printed both. Python 3.13
  ;; EmailMessage.add_attachment writes RFC 2231 continuations for both
  ;; (filename*0*=us-ascii''... / filename*0*=utf-8''..., no plain filename
  ;; for the CJK one), every line within 78, and get_filename() reads them
  ;; back.
  (let ((e (make-binary-entity (coerce #(1 2 3) '(vector (unsigned-byte 8))) :filename filename)))
    (dolist (limit '(998 78))
      (let ((wire (handler-case (print-mime e :max-line-length limit)
                    (mime-header-error (c) c))))
        (ok (stringp wire)
            (format nil "~A: prints at :max-line-length ~D (got ~A)" label limit
                    (if (stringp wire) "a message" wire)))
        (when (stringp wire)
          (ok (every (lambda (l) (<= (length l) 998)) (%r2-header-lines wire))
              "no header line over 998 octets")
          (ok (equal filename (disposition-filename (mime-content-disposition (parse-mime wire))))
              "the filename reads back"))))))

;;; Header text: line breaks other than CR and LF.

(deftest-parametrize r3-other-line-breaks-in-a-header-value-are-refused
    ((code)
     (11)
     (12)
     (28)
     (29)
     (30))
  ;; SET-HEADER and PRINT-MIME refuse CR, LF (and NUL) but write VT, FF, FS,
  ;; GS and RS as they are: "Subject: Hi<FF>Bcc: eve@example.com". RFC 5322
  ;; does not allow them in a field body (VCHAR / WSP; obs-utext only), and
  ;; Python treats each as a line break (str.splitlines): 3.13's
  ;; header_store_parse refuses the value ("Header values may not contain
  ;; linefeed or carriage return characters"), and a compat32 parse → print
  ;; of our output stops with "header value appears to contain an embedded
  ;; header: 'Hi\nBcc: eve@example.com'" (CVE-2024-6923's check; before it,
  ;; 3.12.4 / 3.11.9 and older wrote that Bcc line). mail-protocol passes
  ;; Message-ID, Date and ASCII display names to SET-HEADER unencoded.
  ;; MIME-PROTOCOL's own parameter check (%CHECK-HEADER-TEXT) already refuses
  ;; every control but HTAB.
  (let ((result (handler-case
                    (let ((e (make-text-entity "x")))
                      (set-header e "Subject" (format nil "Hi~CBcc: eve@example.com" (code-char code)))
                      (print-mime e))
                  (mime-header-error () :refused))))
    (ok (eq :refused result)
        (format nil "U+~4,'0X in a Subject is refused (got ~S)" code result))))

;;; Pre-existing (0.3.1 and 0.3.2 do the same), reached by the new
;;; continuation join as well.

(deftest-parametrize r3-iso-8859-1-ext-values-are-read
    ((value filename)
     ;; RFC 8187 3.2.2's own example.
     ("attachment; filename*=iso-8859-1'en'%A3%20rates" (format nil "~C rates" (code-char #xA3)))
     ("attachment; filename*0*=iso-8859-1''caf%E9; filename*1*=.txt" (format nil "caf~C.txt" (code-char #xE9))))
  ;; %PARSE-RFC5987 accepts the iso-8859-1 charset, but
  ;; %DECODE-RFC5987-VALUE decodes the octets as UTF-8 whatever it says:
  ;; DISPOSITION-FILENAME signals ENCODING-PROTOCOL:ENCODING-TRANSCODE-ERROR
  ;; (not even a MIME-ERROR, so mail-protocol's and serdes' handlers miss it).
  ;; Python 3.13 reads "£ rates" and "café.txt".
  (let ((got (handler-case (disposition-filename (parse-content-disposition value))
               (error (c) (list (type-of c) (princ-to-string c))))))
    (ok (equal filename got)
        (format nil "~S → ~S (got ~S)" value filename got))))

;;; The new make-text-entity contract.

(deftest r3-an-unknown-charset-signals-mime-encode-error
  ;; The PR: "an unknown charset signals mime-encode-error". %CHARSET-ENCODING
  ;; takes any existing keyword for an encoding, so "default" (or any name
  ;; some package interned) reaches encoding-protocol and signals
  ;; ENCODING-TRANSCODE-ERROR (":DEFAULT is not a valid encoding designator").
  (let ((got (handler-case (progn (make-text-entity "x" :charset "default") :made)
               (mime-encode-error () :mime-encode-error)
               (error (c) (type-of c)))))
    (ok (eq :mime-encode-error got)
        (format nil "charset \"default\" → MIME-ENCODE-ERROR (got ~S)" got))))

(in-package #:mime-protocol/tests)

;;; Adversarial review, round 2, of egao1980/mime-protocol#17 (head a8738c5:
;;; #12 header injection, #16 octet print). Each test is a break found
;;; against that head; where more than one fix would do, the test accepts
;;; each of them.
;;;
;;; Python 3.13.18 comparisons (email.message.EmailMessage), 2026-11-24.

(defun %r2-octets (&rest items)
  "Octets from ITEMS: an integer is one octet, a string its characters as
ISO-8859-1 octets, an octet vector itself."
  (coerce (loop for x in items
                append (etypecase x
                         (integer (list x))
                         (string (map 'list #'char-code x))
                         (vector (coerce x 'list))))
          '(vector (unsigned-byte 8))))

(defun %r2-header-lines (wire)
  "The header lines of WIRE, up to the first blank line, without CRLF."
  (loop for line in (uiop:split-string wire :separator '(#\Newline))
        for l = (string-right-trim '(#\Return) line)
        until (zerop (length l))
        collect l))

(defun %r2-b-word-octets (value)
  "The octets of VALUE when it is one RFC 2047 B encoded-word
=?charset?b?payload?= (any charset, unknown-8bit included), else NIL."
  (let* ((q1 (and (eql 0 (search "=?" value)) (position #\? value :start 2)))
         (q2 (and q1 (position #\? value :start (1+ q1))))
         (close (and q2 (search "?=" value :start2 (1+ q2)))))
    (when (and close
               (= q2 (+ q1 2))
               (char-equal #\b (char value (1+ q1)))
               (= (+ close 2) (length value)))
      (handler-case (encoding-protocol:decode (subseq value (1+ q2) close) :encoding :base64)
        (error () nil)))))

;;; serdes-protocol's octet entry points (regression against 0.3.1).

(deftest r2-serdes-mime-octets-keep-non-ascii-header-text
  ;; serdes-protocol:encode-to-octets UTF-8-encodes the string a text
  ;; format's ENCODE returns (:mime is not a binary format), and that string
  ;; is now octets, so the header text is encoded twice: "café —" comes
  ;; back as "cafÃ© â\u0080\u0094". 0.3.1 gives "café —" back. The
  ;; http-protocol hook was moved to PRINT-MIME-OCTETS; this path was not.
  (let ((subject (format nil "caf~C ~C" (code-char #xE9) (code-char #x2014)))
        (e (make-text-entity "body")))
    (set-header e "Subject" subject)
    (let* ((octets (serdes-protocol:encode-to-octets e :format :mime))
           (back (header-value (serdes-protocol:decode-octets octets :format :mime) "subject")))
      (ok (equal subject back)
          (format nil "encode-to-octets → decode-octets keeps the Subject (got ~S)" back)))))

(deftest r2-serdes-multipart-octets-keep-a-non-ascii-field-name
  ;; The same through :multipart: a form field named "имя" comes back as
  ;; "Ð¸Ð¼Ñ\u008F". 0.3.1 gives "имя" back.
  (let* ((name (coerce (mapcar #'code-char '(#x438 #x43C #x44F)) 'string))
         (table (serdes-protocol:decode-octets
                 (serdes-protocol:encode-to-octets (list (cons name "v")) :format :multipart)
                 :format :multipart))
         (keys (loop for k being the hash-keys of table collect k)))
    (ok (equal (list name) keys)
        (format nil "the field name survives encode-to-octets → decode-octets (got ~S)" keys))))

;;; Lost messages (0.3.1 printed these).

(deftest r2-long-non-ascii-filename-prints-and-reads-back
  ;; 120 CJK characters (a Windows file name may hold 255 UTF-16 units):
  ;; the filename*=UTF-8''%E4%B8%AD... ext-value is one 1097-octet run
  ;; without a space, so PRINT-MIME refuses the attachment at the default 998
  ;; and at mail's 78 (from 109 such characters on): the message cannot be
  ;; sent at all. 0.3.1 printed it. Python's EmailMessage (policy.SMTP and
  ;; policy.default) writes RFC 2231 continuations, filename*0*=utf-8''...;
  ;; filename*1*=..., every line within 78, and get_filename() reads the
  ;; name back.
  (let* ((filename (make-string 120 :initial-element (code-char #x4E2D)))
         (e (make-binary-entity (coerce #(1 2 3) '(vector (unsigned-byte 8)))
                                :filename filename)))
    (dolist (limit '(998 78))
      (let ((wire (handler-case (print-mime e :max-line-length limit)
                    (mime-header-error (c) c))))
        (ok (stringp wire)
            (format nil "prints at :max-line-length ~D (got ~A)" limit
                    (if (stringp wire) "a message" wire)))
        (when (stringp wire)
          (ok (every (lambda (l) (<= (length l) 998)) (%r2-header-lines wire))
              "no header line over 998 octets")
          (let ((back (disposition-filename (mime-content-disposition (parse-mime wire)))))
            (ok (equal filename back)
                (format nil "the filename reads back (got ~S)" back))))))))

;;; A parse → print relay corrupts a field (regression against 0.3.1).

(deftest r2-relay-keeps-the-octets-of-a-non-utf-8-header-field
  ;; A received Subject in raw windows-1251, "Привет" (CF F0 E8 E2 E5 F2), no
  ;; RFC 2047. PARSE-MIME reads it as ISO-8859-1, "Ïðèâåò", and PRINT-MIME
  ;; writes those characters as UTF-8 (C3 8F C3 B0 ...): the relayed field is
  ;; now valid UTF-8 that says "Ïðèâåò", and the original octets are gone
  ;; for any reader. 0.3.1's string, written as ISO-8859-1 (as the PR says
  ;; to), keeps them. Python 3.13 keeps them too: compat32 as they are,
  ;; policy.default as =?unknown-8bit?b?z/Do4uXy?=. Either is accepted.
  (let* ((field (%r2-octets #xCF #xF0 #xE8 #xE2 #xE5 #xF2))
         (raw (%r2-octets "Subject: " field 13 10 13 10 "body"))
         (wire (encoding-protocol:encode (print-mime (parse-mime raw)) :encoding :iso-8859-1))
         (printed (header-value (parse-mime wire) "subject")))
    (ok (or (search field wire)
            (equalp field (%r2-b-word-octets printed)))
        (format nil "the Subject's octets survive parse → print (got ~S)"
                (subseq wire 0 (search #(13 10) wire))))))

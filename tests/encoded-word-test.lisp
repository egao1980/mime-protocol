(in-package #:mime-protocol/tests)

;;; RFC 2047 encoded-words: ENCODE-HEADER-TEXT / ENCODE-ENCODED-WORDS write
;;; them, DECODE-HEADER-TEXT reads them back (mail-protocol#5: a non-ASCII
;;; Subject went out as raw 8-bit). Decoding cases are what Python's email
;;; package writes (policy.SMTP, checked with Python 3.12).

(defun %ascii-only-p (string)
  "Printable ASCII, SP and HTAB only."
  (every (lambda (c) (or (char= c #\Tab) (< 31 (char-code c) 127))) string))

(defun %words (string)
  (remove "" (uiop:split-string string :separator '(#\Space)) :test #'string=))

(deftest-parametrize header-text-round-trips
    ((text)
     ("plain ASCII stays")
     ((format nil "Caf~C ~C menu" (code-char #xE9) (code-char #x2615)))
     ((format nil "Caf~C  ~C~Cdouble blanks and a tab" (code-char #xE9) (code-char #x2615) #\Tab))
     ((format nil "~{~A~^ ~}" (loop for i below 50 collect (format nil "~C~D" (code-char #x41F) i))))
     ((make-string 300 :initial-element (code-char #x2603)))
     ((make-string 200 :initial-element #\x))
     ("looks =?utf-8?q?like?= an encoded word")
     ("=?utf-8?b?SGk=?=")
     ((format nil "esc~Cape" (code-char 27)))
     (" leading and trailing ")
     (""))
  (let ((wire (encode-header-text text)))
    (ok (%ascii-only-p wire) (format nil "ASCII on the wire: ~S" wire))
    (ok (every (lambda (w) (<= (length w) 76)) (%words wire))
        (format nil "no word longer than a line: ~S" (mapcar #'length (%words wire))))
    (ok (equal text (decode-header-text wire)) (format nil "~S → ~S → ~S" text wire (decode-header-text wire)))))

(deftest ascii-header-text-is-unchanged
  (ok (equal "Re: [list] the plan, v2 (draft)" (encode-header-text "Re: [list] the plan, v2 (draft)")))
  (ok (equal "Hi" (decode-header-text "Hi"))))

(deftest only-the-words-that-need-it-are-encoded
  (let ((wire (encode-header-text (format nil "Re: Caf~C ~C at noon" (code-char #xE9) (code-char #x2615)))))
    (ok (eql 0 (search "Re: =?utf-8?b?" wire)) wire)
    (ok (search "?= at noon" wire) wire)
    (ok (= 1 (length (remove-if-not (lambda (w) (search "=?" w)) (%words wire))))
        "the two adjacent words are one run, one encoded-word")))

(deftest encode-header-text-refuses-line-breaks
  (ok (signals (encode-header-text (format nil "Hi~C~CBcc: eve@example.com" #\Return #\Newline)
                                   :name "Subject")
               'mime-header-error))
  (ok (signals (encode-header-text (format nil "Hi~CBcc: x" #\Newline)) 'mime-header-error))
  (ok (equal "Hi"
             (handler-bind ((mime-header-error (lambda (c) (use-value "Hi" c))))
               (encode-header-text (format nil "Hi~Cx" #\Return))))
      "restart use-value"))

(deftest encode-encoded-words-splits-on-characters
  (let* ((text (make-string 100 :initial-element (code-char #x20AC)))  ; 3 octets each
         (wire (encode-encoded-words text)))
    (ok (every (lambda (w) (and (eql 0 (search "=?utf-8?b?" w)) (<= (length w) 75))) (%words wire)))
    (ok (equal text (decode-header-text wire)))
    (ok (equal "" (encode-encoded-words "")))))

(deftest-parametrize decodes-what-python-writes
    ((wire text)
     ("=?utf-8?b?Wm/DqyDDnG7Dr2NvZGU=?=" (format nil "Zo~C ~Cn~Ccode" (code-char #xEB) (code-char #xDC) (code-char #xEF)))
     ("=?utf-8?q?Caf=C3=A9?= word" (format nil "Caf~C word" (code-char #xE9)))
     ("=?utf-8?q?xx?=  =?utf-8?q?yy?=" "xxyy")
     ("=?UTF-8?Q?a_b?=" "a b")
     ("=?utf-8?q?=C3?= =?utf-8?q?=A9?=" (string (code-char #xE9)))
     ("=?iso-8859-1?q?caf=E9?=" (format nil "caf~C" (code-char #xE9)))
     ("=?utf-8*en?b?SGk=?=" "Hi")
     ("a =?utf-8?b?SGk=?= b" "a Hi b")
     ("=?utf-8?b?SGk=?==?utf-8?b?IHRoZXJl?=" "Hi there"))
  (ok (equal text (decode-header-text wire)) (format nil "~S → ~S" wire (decode-header-text wire))))

(deftest-parametrize malformed-encoded-words-are-kept
    ((wire)
     ("=?utf-8?x?abc?=") ("=?bogus-charset?b?SGk=?=") ("=?utf-8?b?SGk=") ("=?utf-8?q?=ZZ?=")
     ("=?base64?b?SGk=?=") ("x=?utf-8?b?SGk=?=") ("=?utf-8?b?/w==?=") ("=??b?SGk=?="))
  (ok (equal wire (decode-header-text wire)) wire))

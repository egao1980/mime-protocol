(in-package #:mime-protocol/tests)

;;; mime-protocol#22: MIME charset names resolve through
;;; encoding-protocol:find-encoding (0.6.0), so Shift_JIS, GB2312, EUC-JP,
;;; Windows-31J and the on-demand CJK charsets decode. Vectors from Python
;;; 3.13 codecs. An unknown charset name is never interned.

(deftest-parametrize encoded-words-in-cjk-charsets-decode
    ((wire text)
     ("=?Shift_JIS?B?k/qWew==?=" (coerce (list (code-char #x65E5) (code-char #x672C)) 'string))
     ("=?shift_jis?q?=93=FA=96=7B?=" (coerce (list (code-char #x65E5) (code-char #x672C)) 'string))
     ("=?windows-31j?B?k/qWew==?=" (coerce (list (code-char #x65E5) (code-char #x672C)) 'string))
     ("=?EUC-JP?B?xvzL3A==?=" (coerce (list (code-char #x65E5) (code-char #x672C)) 'string))
     ("=?gb2312?q?=D6=D0=CE=C4?=" (coerce (list (code-char #x4E2D) (code-char #x6587)) 'string))
     ("=?big5?B?pKSk5Q==?=" (coerce (list (code-char #x4E2D) (code-char #x6587)) 'string))
     ("=?windows-1250?q?=8A?=" (string (code-char #x160))))
  (ok (equal text (decode-header-text wire)) (format nil "~S → ~S" wire (decode-header-text wire))))

(deftest an-unknown-charset-name-is-not-interned
  (let* ((charset (format nil "x-no-such-charset-~36R" (random (expt 36 8))))
         (wire (format nil "=?~A?b?SGk=?=" charset)))
    (ok (equal wire (decode-header-text wire)))
    (ok (signals (make-text-entity "x" :charset charset) 'mime-encode-error))
    (ok (null (find-symbol (string-upcase charset) :keyword)))))

(deftest a-charset-whose-system-is-not-loaded-is-unknown
  ;; *AUTO-LOAD-CHARSETS* NIL: a charset whose system this image has not
  ;; loaded is kept as written, never an ENCODING-UNKNOWN-ENCODING.
  (let ((encoding-protocol:*auto-load-charsets* nil)
        (wire "=?iso-2022-jp?b?GyRCRnxLXBsoQg==?="))
    (if (asdf:component-loaded-p "encoding-protocol/iso-2022-jp")
        (ok (equal (coerce (list (code-char #x65E5) (code-char #x672C)) 'string)
                   (decode-header-text wire)))
        (ok (equal wire (decode-header-text wire))))))

(deftest text-entities-encode-in-cjk-charsets
  (ok (equalp #(#xD6 #xD0 #xCE #xC4)
              (mime-content (make-text-entity (coerce (list (code-char #x4E2D) (code-char #x6587)) 'string)
                                              :charset "gb2312"))))
  (ok (equalp #(#x93 #xFA #x96 #x7B)
              (mime-content (make-text-entity (coerce (list (code-char #x65E5) (code-char #x672C)) 'string)
                                              :charset "Shift_JIS"))))
  (ok (equal "Shift_JIS"
             (media-type-charset
              (mime-content-type
               (make-text-entity "a" :charset "Shift_JIS"))))))

(deftest ext-values-decode-in-any-known-charset
  (ok (equal (format nil "~C~C.txt" (code-char #x65E5) (code-char #x672C))
             (disposition-filename
              (parse-content-disposition "attachment; filename*=shift_jis''%93%FA%96%7B.txt"))))
  (ok (signals (disposition-filename
                (parse-content-disposition "attachment; filename*=x-bogus''abc"))
               'mime-parse-error))
  (ok (signals (disposition-filename
                (parse-content-disposition "attachment; filename*=base64''SGk="))
               'mime-parse-error)))

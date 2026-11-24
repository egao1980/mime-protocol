(defsystem "mime-protocol"
  :version "0.3.3"
  :description "CLOS MIME media-type, disposition, CTE, and multipart for cl-stack (RFC 2045/2046/2047/6266)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "serdes-protocol" "0.2.2") (:version "encoding-protocol" "0.3.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "media-type")
               (:file "disposition")
               (:file "cte")
               (:file "lookup")
               (:file "entity")
               (:file "encoded-word")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "mime-protocol/tests"))))

(defsystem "mime-protocol/tests"
  :depends-on ("mime-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "media-type-test")
               (:file "lookup-test")
               (:file "cte-test")
               (:file "multipart-test")
               (:file "disposition-test")
               (:file "nested-multipart-test")
               (:file "form-data-browser-test")
               (:file "header-injection-test")
               (:file "encoded-word-test")
               (:file "adversarial-r1-test")
               (:file "adversarial-r2-test")
               (:file "relay-continuation-test")
               (:file "adversarial-r3-test"))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "tests failed for ~A" (component-name c)))))

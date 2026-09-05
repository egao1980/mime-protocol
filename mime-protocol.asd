(defsystem "mime-protocol"
  :version "0.1.0"
  :description "CLOS MIME media-type, disposition, CTE, and multipart for cl-stack (RFC 2045/2046/6266)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :properties (:cl-repo (:ci (:sources (("serdes-protocol" :oci)))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "media-type")
               (:file "disposition")
               (:file "cte")
               (:file "lookup")
               (:file "entity")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "mime-protocol/tests"))))

(defsystem "mime-protocol/tests"
  :depends-on ("mime-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "media-type-test")
               (:file "cte-test")
               (:file "multipart-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

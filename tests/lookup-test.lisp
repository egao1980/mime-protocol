(in-package #:mime-protocol/tests)

(deftest lookup-mime-json-html
  (ok (string= "application/json" (lookup-mime "foo.json")))
  (ok (string= "application/json" (lookup-mime "json")))
  (ok (string= "text/html" (lookup-mime "index.html")))
  (ok (string= "text/html" (lookup-mime "index.htm"))))

(deftest lookup-extension-json-html
  (ok (string= "json" (lookup-extension "application/json")))
  (ok (string= "json" (lookup-extension "application/json; charset=utf-8")))
  (ok (string= "json" (lookup-extension (parse-media-type "application/json"))))
  (ok (string= "html" (lookup-extension "text/html")))
  (ok (string= "html" (lookup-extension "text/html; charset=UTF-8")))
  (ok (null (lookup-extension "application/x-unknown")))
  (ok (null (lookup-extension nil))))

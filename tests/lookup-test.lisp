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

(deftest guess-type-tar-gz
  (multiple-value-bind (type enc) (guess-type "archive.tar.gz")
    (ok (string= "application/x-tar" type))
    (ok (string= "gzip" enc)))
  (multiple-value-bind (type enc) (guess-type "archive.tgz")
    (ok (string= "application/x-tar" type))
    (ok (string= "gzip" enc)))
  (multiple-value-bind (type enc) (guess-type "notes.txt")
    (ok (string= "text/plain" type))
    (ok (null enc)))
  (multiple-value-bind (type enc) (guess-type "blob.unknown")
    (ok (null type))
    (ok (null enc)))
  (multiple-value-bind (type enc) (guess-type "only.gz")
    (ok (null type))
    (ok (string= "gzip" enc)))
  (ok (string= "application/x-bzip2" (lookup-mime "x.bz2"))))

(deftest add-type-roundtrip
  (unwind-protect
       (progn
         (ok (string= "application/x-widget" (add-type "application/x-widget" ".wid")))
         (ok (string= "application/x-widget" (lookup-mime "foo.wid")))
         (ok (string= "wid" (lookup-extension "application/x-widget"))))
    (remhash "wid" mime-protocol::*mime-extensions*)
    (remhash "application/x-widget" mime-protocol::*mime-type-extensions*)))

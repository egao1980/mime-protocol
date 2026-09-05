# mime-protocol

CLOS **MIME** media-type, Content-Disposition, Content-Transfer-Encoding, and multipart for [cl-stack](https://github.com/egao1980/cl-stack). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:mime` and `:multipart`.

Not HTTP `Content-Encoding` (gzip/br/zstd) — that stays on `http-protocol`.

OCI **0.1.0** — `ghcr.io/egao1980/cl-systems/mime-protocol:0.1.0`

```lisp
(asdf:load-system "mime-protocol")   ; nick stack-mime; registers :mime / :multipart

(stack-mime:parse-media-type "text/plain; charset=utf-8")
(stack-mime:media-type-match-p "application/json" "application/*")

(let ((e (stack-mime:make-form-data '(("title" . "hi")))))
  (serdes-protocol:encode e :format :multipart))
```

| Format | Wire | Lisp |
|--------|------|------|
| `:mime` | RFC 2045 entity | `mime-entity` |
| `:multipart` | `multipart/form-data` | alist / hash-table of parts |

`lookup-mime` / `mime-type` map filename extensions. `lookup-extension` is the reverse (first table match: `application/json` → `json`, `text/html` → `html`). CTE is `decode-content` / `encode-content` (`:7bit` `:8bit` `:binary` `:base64` `:quoted-printable`).

## License

MIT — see [LICENSE](LICENSE).

# mime-protocol

CLOS **MIME** media-type, Content-Disposition, Content-Transfer-Encoding, and multipart for [cl-stack](https://github.com/egao1980/cl-stack). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:mime` and `:multipart`.

Not HTTP `Content-Encoding` (gzip/br/zstd) — that stays on `http-protocol`.

OCI **0.3.0** — `ghcr.io/egao1980/cl-systems/mime-protocol:0.3.0`

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

## Entities and multipart

`make-text-entity`, `make-binary-entity`, `make-multipart-entity` (any subtype: `mixed`, `alternative`, `related`, `form-data`) nest freely; `print-mime` and `parse-mime` round-trip them, mail layouts like `mixed(alternative(text, html), attachment)` included. Per RFC 2046 §5.1.1 the CRLF before a delimiter belongs to the delimiter, so a part whose content ends in a line break keeps it, and preamble / epilogue come back without it. A delimiter must be followed by `--`, padding or a line break, so `--b` is not found inside `--b2`. A body part may omit its headers (it starts with a blank line).

Headers are stored lowercase (`header-value` and `set-header` are case-insensitive) and printed in canonical case by `canonical-header-name`: `From`, `Subject`, `MIME-Version`, `Message-ID`, `Content-Transfer-Encoding`, `DKIM-Signature`.

## License

MIT — see [LICENSE](LICENSE).

# mime-protocol

CLOS **MIME** media-type, Content-Disposition, Content-Transfer-Encoding, and multipart for [cl-stack](https://github.com/egao1980/cl-stack). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:mime` and `:multipart`.

Not HTTP `Content-Encoding` (gzip/br/zstd) — that stays on `http-protocol`.

OCI **0.3.1** — `ghcr.io/egao1980/cl-systems/mime-protocol:0.3.1`

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

`parse-mime` reads each header field as UTF-8 when its octets are valid UTF-8 (browsers send form-data part headers that way, RFC 7578 §5.1.3; RFC 6532 mail does too), and as ISO-8859-1 otherwise, one character per octet.

## Form-data names

In a `form-data` Content-Disposition, `name` and `filename` follow the browser rule (RFC 7578 §2, §4.2; HTML "multipart/form-data encoding algorithm"): the quoted string ends at the next `"`, a backslash in it is literal (`filename="C:\x\a.svg"`), and `%22` `%0D` `%0A` decode to `"` CR LF. Other `%` sequences are kept. `format-form-data-disposition` and `format-content-disposition` escape them the same way, so a browser, http-protocol's multipart and `make-form-data` all parse back to the same names. `filename*` (RFC 8187) still wins over `filename` when both are present. Every other parameter, and every other disposition and media type, keeps RFC 822 quoted-pairs.

## License

MIT — see [LICENSE](LICENSE).

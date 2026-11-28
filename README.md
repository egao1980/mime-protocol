# mime-protocol

CLOS **MIME** media-type, Content-Disposition, Content-Transfer-Encoding, and multipart for [cl-stack](https://github.com/egao1980/cl-stack). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:mime` and `:multipart`.

Not HTTP `Content-Encoding` (gzip/br/zstd) — that stays on `http-protocol`.

OCI **0.3.4** — `ghcr.io/egao1980/cl-systems/mime-protocol:0.3.4`

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

Both are binary serdes formats: `serdes-protocol:encode` returns `print-mime-octets`, so `encode-to-octets` passes them on unchanged, and their streams are octets by default (`:element-type 'character` gives one character per octet).

`lookup-mime` / `mime-type` map filename extensions. `lookup-extension` is the reverse (first table match: `application/json` → `json`, `text/html` → `html`). CTE is `decode-content` / `encode-content` (`:7bit` `:8bit` `:binary` `:base64` `:quoted-printable`).

## Entities and multipart

`make-text-entity`, `make-binary-entity`, `make-multipart-entity` (any subtype: `mixed`, `alternative`, `related`, `form-data`) nest freely; `print-mime` and `parse-mime` round-trip them, mail layouts like `mixed(alternative(text, html), attachment)` included. Per RFC 2046 §5.1.1 the CRLF before a delimiter belongs to the delimiter, so a part whose content ends in a line break keeps it, and preamble / epilogue come back without it. A delimiter must be followed by `--`, padding or a line break, so `--b` is not found inside `--b2`. A body part may omit its headers (it starts with a blank line).

Headers are stored lowercase (`header-value` and `set-header` are case-insensitive) and printed in canonical case by `canonical-header-name`: `From`, `Subject`, `MIME-Version`, `Message-ID`, `Content-Transfer-Encoding`, `DKIM-Signature`.

`parse-mime` reads each header field as UTF-8 when its octets are valid UTF-8 (browsers send form-data part headers that way, RFC 7578 §5.1.3; RFC 6532 mail does too), and as ISO-8859-1 otherwise, one character per octet.

`print-mime` returns octets, one character per octet: body octets as they are and header text as UTF-8. Write the string to the wire through ISO-8859-1, or call `print-mime-octets`; `parse-mime` reads either back, whatever the header text (#16). The http-protocol `:mime` / `:multipart` data serializers hand http-protocol those octets. A parsed field whose octets were not UTF-8 (a raw windows-1251 Subject) reads as ISO-8859-1 and is printed back as the same octets, so a parse → print relay keeps it; once `set-header` replaces it, it is UTF-8. That raw state belongs to each field's value, not to its name: two fields of one name keep their own octets, and a raw `name=` / `filename=` in Content-Type or Content-Disposition is written back as its octets (no `filename*` is made up for it; a long one goes out as quoted `filename*0="…"` continuations of those octets). `copy-headers` (`from to &key test remove`) copies or moves fields to another entity with their values, so the octets go along (mail-protocol's `add-attachment` on a parsed message). `parse-mime` of a string with a character above U+00FF signals `mime-parse-error`. Header names are looked up case-insensitively.

## Header fields

`set-header` refuses a value holding any C0 control but HTAB (CR, LF, NUL, and VT, FF, FS, GS, RS, which Python's `email` splits lines at), and a name that is not RFC 5322 ftext (printable ASCII without a colon), with `mime-header-error` (a `mime-encode-error`; readers `mime-header-error-name`, `mime-header-error-value`). A line break in a value would end the field, so the rest would be read as further fields (`Bcc:`) or the body: header injection (#12). Python's `EmailMessage` refuses CR and LF in a value too. Restart `use-value` takes another value or name, which is checked again.

`print-mime` checks again what reached the headers some other way (`(setf mime-headers)`, a parsed field) and signals the same condition before it writes anything: a value with a C0 control other than HTAB (a bare CR or an FF survives `parse-headers`), or a name that is not ftext (a name with a blank, a colon or a non-ASCII character would merge into the field before it or end the header block in an RFC 5322 reader). A header line longer than `:max-line-length` octets (default 998, RFC 5322 §2.1.1's limit; mail passes 78) is folded before a single space inside the value, so unfolding gives the value back exactly. A token with no space in it is not cut, and a line that stays over 998 is refused. `nil` never folds or refuses: HTTP form-data has no line limit, and the `:multipart` serdes format and the http-protocol hook pass it.

A multipart part, or the preamble, that holds its own delimiter line (`--boundary` at a line start) would end the part early and let the rest pose as further parts. `print-mime` refuses it with `mime-boundary-error` (a `mime-encode-error`; restart `use-value` takes another boundary). `parse-mime` finds a non-ASCII boundary in the body as UTF-8 or as one octet per character, so a received ISO-8859-1 boundary still parses and re-prints. `make-multipart-entity`'s default boundary is 96 random bits from the OS.

RFC 2047 encoded-words: `encode-header-text` turns the runs of words in unstructured text (Subject) that need it (non-ASCII, control characters, `=?`, longer than a line) into `=?utf-8?b?...?=` words and leaves the rest as it is; it refuses CR, LF and NUL as `set-header` does. `encode-encoded-words` encodes all of a text (a display name). Each word holds whole characters and is at most 64 characters. `decode-header-text` reads B and Q words in any charset encoding-protocol knows, drops blanks between adjacent words, decodes a character split across two words, and keeps a malformed word as written.

## Form-data names

In a `form-data` Content-Disposition, `name` and `filename` follow the browser rule (RFC 7578 §2, §4.2; HTML "multipart/form-data encoding algorithm"): the quoted string ends at the next `"`, a backslash in it is literal (`filename="C:\x\a.svg"`), and `%22` `%0D` `%0A` decode to `"` CR LF. Other `%` sequences are kept. `format-form-data-disposition` and `format-content-disposition` escape them the same way, so a browser, http-protocol's multipart and `make-form-data` all parse back to the same names. RFC 7578 §4.2 says `filename*` MUST NOT be used in form-data, and servers disagree on it: Werkzeug reads `filename*` over `filename` and joins `name*0` / `filename*0` continuations, while python-multipart reads neither. So since 0.3.6 (#21) a form-data disposition with any `name*` / `filename*` parameter signals `mime-parse-error`, except a `filename*` that reads as its `filename` exactly. A repeated `name`, `filename` or `filename*` (Python's parsers read the last one) signals it too. On output a form-data disposition never has a `filename*` or a continuation: a non-ASCII filename goes out as UTF-8 in `filename`, as browsers send it. Outside form-data, a short ASCII value is written plain, and a non-ASCII or long one (over 60 octets) as `filename*=UTF-8''…` in its place, long ones as RFC 2231 continuations (`filename*0*=UTF-8''…; filename*1*=…`, at most 60 characters each, never cutting a character), as Python's `EmailMessage` writes them, so the field folds; `parse-content-disposition` joins continuations (encoded, plain or mixed) back into one parameter. An `iso-8859-1` ext-value is read as ISO-8859-1; an undecodable one signals `mime-parse-error`. A caller's `filename*` ext-value is used as it is only when its language part is a tag, so it cannot add parameters. `make-text-entity` encodes the text in its `:charset` (an unknown one, `"default"` included, or text it cannot encode signals `mime-encode-error`). Every other parameter, and every other disposition and media type, keeps RFC 822 quoted-pairs.

## License

MIT — see [LICENSE](LICENSE).

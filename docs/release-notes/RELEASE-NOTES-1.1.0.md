# cl-chat 1.1.0

**Release date:** 2026-03-31

This release addresses **5 security findings** identified by the
[CLSEC initiative](https://github.com/atgreen/CLSEC).

## Security Fixes

### CLSEC-2026-0116 — Gemini API key in URL (HIGH)

This is inherited from cl-completions and tracked as CLSEC-2026-0001.
Fix will come from an upstream cl-completions update.

### CLSEC-2026-0117 — Error/backtrace exposure (MEDIUM)

Hunchentoot was configured with `*show-lisp-errors-p*` and
`*show-lisp-backtraces-p*` set to T, exposing internal stack traces
in HTTP error responses.

**Fix:** Both are now set to NIL.  Errors are still logged server-side
via log4cl.

### CLSEC-2026-0118 — Stored XSS via unsanitized markdown (HIGH, CVSS 9.0)

User prompts were converted from markdown to HTML via 3bmd and
injected into the page via `innerHTML`/`outerHTML` with no
sanitization.  In the multi-user WebSocket chat room, this enabled
cross-user XSS.

**Fix:** All markdown HTML output is now sanitized through
`sanitize-html:sanitize` before embedding.  Added `sanitize-html`
as a dependency.

### CLSEC-2026-0119 — No TLS on WebSocket/HTTP (MEDIUM)

The WebSocket URL was hardcoded to `ws://localhost:8081/bongo`.

**Fix:** WebSocket URL is now derived from the page's protocol --
uses `wss:` when served over HTTPS, `ws:` otherwise.

### CLSEC-2026-0120 — Session cookie exposed in JavaScript (MEDIUM)

The Hunchentoot session cookie value was embedded directly in a
JavaScript variable and sent over WebSocket messages.

**Fix:** A separate random WebSocket token (32 hex chars via
`ironclad:random-data`) is generated per session.  The actual session
cookie is never exposed to client-side JavaScript.

## Acknowledgments

Security issues identified by the CLSEC (Common Lisp Security
Initiative) automated audit.

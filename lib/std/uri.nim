# (c) 2026 Andreas Rumpf
#
# URIs: parsing, percent-coding, query strings and — the one that is not a
# convenience — path normalization.
#
#   let u = parseUri("http://example.com/a/b?x=1#top")
#   u.hostname == "example.com"
#   u.path     == "/a/b"
#   for (k, v) in decodeQuery(u.query): echo k, " = ", v
#
# ## Why the decoder returns a bool
#
# `%ZZ` is not a URI. Nim's `decodeUrl` raises on it, which means every caller
# either wraps a `try` around a routing decision or gets an exception out of
# what should have been a 400. Both forms are here and the strict one is the
# default shape: `decodeUrl(src, dest)` answers whether `src` *was* a URI, and
# a caller that does not look at the answer has written code that visibly does
# not look at it.
#
# ## Decode, then normalize, in that order
#
# `%2e%2e%2f` is `../`. A server that normalizes the raw target and then
# decodes it has normalized a string that did not yet contain the segments it
# was looking for — which is the entire mechanism behind a decade of path
# traversal bugs, and it is an ordering, not a missing check. `safePath` does
# both, in the order that works, and answers `""` rather than a path that
# climbed out of the root: there is no correct way to serve that request, so
# there is no value to hand back for one.

import std / assertions

type
  Uri* = object
    ## The pieces of a URI, each already percent-*decoded* where decoding is
    ## unambiguous and left raw where it is not: `query` keeps its `%` escapes
    ## because they are what separates a `&` in a value from the `&` between
    ## two pairs, and `decodeQuery` decodes each side after it splits.
    scheme*: string
    username*: string
    password*: string
    hostname*: string
    port*: string
    path*: string
    query*: string
    anchor*: string
    opaque*: bool
      ## The part after the scheme did not start with `//`, so `path` holds
      ## everything and there is no authority: `mailto:x@y`, `urn:...`.
    isIpv6*: bool
      ## `hostname` came from `[...]` and holds the address without them. It
      ## has to be remembered rather than re-derived: an address is put back
      ## in brackets when the URI is spelled out again, and a hostname with
      ## colons in it that is *not* bracketed is a different, invalid URI.

func initUri*(isIpv6 = false): Uri =
  Uri(scheme: "", username: "", password: "", hostname: "", port: "",
      path: "", query: "", anchor: "", opaque: false, isIpv6: isIpv6)

# ------------------------------------------------------- percent-coding ----

const
  Unreserved = {'a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', '~'}
    ## RFC 3986 §2.3. Everything else is encoded, which is deliberately more
    ## than the grammar demands: over-encoding is always legal and always
    ## decodes back, and the sub-delims that are *sometimes* legal are exactly
    ## the ones (`&`, `=`, `+`, `;`) whose meaning depends on which component
    ## they land in.

func hexVal(c: char): int {.inline.} =
  if c >= '0' and c <= '9': ord(c) - ord('0')
  elif c >= 'a' and c <= 'f': ord(c) - ord('a') + 10
  elif c >= 'A' and c <= 'F': ord(c) - ord('A') + 10
  else: -1

func addEncoded*(dest: var string; src: openArray[char]; usePlus = false) =
  ## Append `src` percent-encoded. `usePlus` writes a space as `+`, which is
  ## the `application/x-www-form-urlencoded` spelling and *only* correct
  ## inside a query — a `+` in a path is a literal plus.
  const Hex = "0123456789ABCDEF"
  for i in 0..<src.len:
    let c = src[i]
    if c in Unreserved:
      dest.add c
    elif c == ' ' and usePlus:
      dest.add '+'
    else:
      dest.add '%'
      dest.add Hex[(ord(c) shr 4) and 0xF]
      dest.add Hex[ord(c) and 0xF]

func encodeUrl*(src: openArray[char]; usePlus = false): string =
  ## `src` percent-encoded.
  result = ""
  result.addEncoded(src, usePlus)

func decodeUrl*(src: openArray[char]; dest: var string;
                decodePlus = false): bool =
  ## Percent-decode `src` into `dest`, replacing whatever `dest` held.
  ## `false` — and an empty `dest` — for a `%` that is not followed by two hex
  ## digits, which is not a URI and must not be guessed at.
  ##
  ## `decodePlus` reads `+` as a space. Off by default because it is right for
  ## a form-encoded query and wrong everywhere else, and a decoder that does
  ## it unconditionally turns the filename `a+b.txt` into `a b.txt`.
  ##
  ## A `%00` decodes to a NUL, which is a legal byte here and a catastrophe in
  ## a path: `safePath` is what rejects it, because this proc's job is to say
  ## what the bytes were, not whether they are a good idea.
  dest = ""
  var i = 0
  while i < src.len:
    let c = src[i]
    if c == '%':
      if i + 2 >= src.len:
        dest = ""
        return false
      let hi = hexVal(src[i + 1])
      let lo = hexVal(src[i + 2])
      if hi < 0 or lo < 0:
        dest = ""
        return false
      dest.add char(hi * 16 + lo)
      i += 3
    elif c == '+' and decodePlus:
      dest.add ' '
      inc i
    else:
      dest.add c
      inc i
  result = true

proc decodeUrl*(src: string; decodePlus = false): string {.raises.} =
  ## The convenience form, for a caller that already knows its input is a URI
  ## — a literal, or a string this module produced. Raises `SyntaxError` on a
  ## malformed escape.
  ##
  ## Never reach for this on something a peer sent. A request target is not
  ## known to be a URI; that is what the request is claiming and what the
  ## server has to check.
  result = ""
  if not decodeUrl(toOpenArray(src, 0, src.len - 1), result, decodePlus):
    raise SyntaxError

# ------------------------------------------------------------- parsing ----

func find(s: openArray[char]; c: char; start: Natural = 0): int =
  result = -1
  var i: Natural = start
  while i < s.len:
    if s[i] == c: return i
    inc i

func parseAuthority(u: var Uri; a: openArray[char]) =
  ## `[userinfo@]host[:port]`, where host may be `[v6]`.
  var hostStart: Natural = 0
  let at = a.find('@')
  if at >= 0:
    let colon = toOpenArray(a, 0, at - 1).find(':')
    if colon >= 0:
      for i in 0..<colon: u.username.add a[i]
      for i in colon + 1..<at: u.password.add a[i]
    else:
      for i in 0..<at: u.username.add a[i]
    hostStart = at + 1
  if hostStart < a.len and a[hostStart] == '[':
    # An IPv6 literal. The port's colon is the one *after* the bracket, which
    # is the whole reason the brackets exist.
    let close = a.find(']', hostStart)
    if close >= 0:
      u.isIpv6 = true
      for i in hostStart + 1..<close: u.hostname.add a[i]
      if close + 1 < a.len and a[close + 1] == ':':
        for i in close + 2..<a.len: u.port.add a[i]
      return
  var colon = -1
  var i = a.len - 1
  while i >= hostStart:
    if a[i] == ':':
      colon = i
      break
    dec i
  if colon >= 0:
    for k in hostStart..<colon: u.hostname.add a[k]
    for k in colon + 1..<a.len: u.port.add a[k]
  else:
    for k in hostStart..<a.len: u.hostname.add a[k]

func parseUri*(s: openArray[char]): Uri =
  ## Split `s` into its components. This is a *split*, not a validation: it
  ## answers where the parts are, and whether a hostname is a hostname or a
  ## path is servable is a question for whoever is about to use one.
  ##
  ## Accepts every form a request target can take — `/a/b?q` (origin),
  ## `http://h/a` (absolute), `h:443` (authority, for CONNECT) and `*`.
  result = initUri()
  var i = 0
  let n = s.len

  # The fragment first: `#` binds looser than everything, so a `?` after one
  # is part of the fragment and not a query.
  var stop = n
  let hash = s.find('#')
  if hash >= 0:
    for k in hash + 1..<n: result.anchor.add s[k]
    stop = hash

  let q = toOpenArray(s, 0, stop - 1).find('?')
  var pathEnd = stop
  if q >= 0:
    for k in q + 1..<stop: result.query.add s[k]
    pathEnd = q

  # A scheme is `alpha *( alpha / digit / "+" / "-" / "." ) ":"`, and the
  # first colon only starts one if everything before it fits that. Without
  # the test, `/a:b` has the scheme `/a` and `localhost:8080` is unparseable.
  var colon = -1
  var k = 0
  while k < pathEnd:
    let c = s[k]
    if c == ':':
      colon = k
      break
    let ok = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
             (k > 0 and ((c >= '0' and c <= '9') or c == '+' or c == '-' or c == '.'))
    if not ok: break
    inc k
  if colon > 0:
    for j in 0..<colon: result.scheme.add s[j]
    i = colon + 1

  if i + 1 < pathEnd and s[i] == '/' and s[i + 1] == '/':
    i += 2
    var authEnd = i
    while authEnd < pathEnd and s[authEnd] != '/': inc authEnd
    parseAuthority(result, toOpenArray(s, i, authEnd - 1))
    i = authEnd
  elif colon > 0:
    # `mailto:me@example.com`: a scheme with no authority. Everything left is
    # opaque, and treating it as a path would invite `/`-joining it.
    result.opaque = true

  for j in i..<pathEnd: result.path.add s[j]

func parseUri*(s: string): Uri {.inline.} =
  parseUri(toOpenArray(s, 0, s.len - 1))

func isAbsolute*(u: Uri): bool {.inline.} =
  ## Whether `u` names where it lives — a scheme, and an authority unless it
  ## is opaque.
  u.scheme.len > 0 and (u.opaque or u.hostname.len > 0)

func `$`*(u: Uri): string =
  ## Spell `u` out again. Round-trips what `parseUri` split, brackets included.
  result = ""
  if u.scheme.len > 0:
    result.add u.scheme
    result.add ':'
    if not u.opaque: result.add "//"
  elif u.hostname.len > 0:
    result.add "//"
  if u.username.len > 0:
    result.add u.username
    if u.password.len > 0:
      result.add ':'
      result.add u.password
    result.add '@'
  if u.hostname.len > 0:
    if u.isIpv6:
      result.add '['
      result.add u.hostname
      result.add ']'
    else:
      result.add u.hostname
    if u.port.len > 0:
      result.add ':'
      result.add u.port
  result.add u.path
  if u.query.len > 0:
    result.add '?'
    result.add u.query
  if u.anchor.len > 0:
    result.add '#'
    result.add u.anchor

# --------------------------------------------------------- query strings ---

iterator decodeQuery*(q: openArray[char]; decodePlus = true): (string, string) =
  ## The `key=value` pairs of a query string, each side percent-decoded.
  ##
  ## A pair whose *either* side is a malformed escape is skipped rather than
  ## yielded raw: half-decoded is the one answer that is wrong in a way the
  ## consumer cannot see. A caller that must distinguish "absent" from
  ## "malformed" splits the string itself and uses `decodeUrl` directly.
  ##
  ## `key` with no `=` yields an empty value; `&&` yields nothing. Separators
  ## are `&` and `;`, because a query written by an HTML form of a certain
  ## vintage uses the latter.
  var i: Natural = 0
  while i <= q.len:
    var e: Natural = i
    while e < q.len and q[e] != '&' and q[e] != ';': inc e
    if e > i:
      var eq = e
      var j = i
      while j < e:
        if q[j] == '=':
          eq = j
          break
        inc j
      var key = ""
      var val = ""
      if decodeUrl(toOpenArray(q, i, eq - 1), key, decodePlus):
        let vOk =
          if eq < e: decodeUrl(toOpenArray(q, eq + 1, e - 1), val, decodePlus)
          else: true
        if vOk: yield (key, val)
    i = e + 1

func encodeQuery*(pairs: openArray[(string, string)]; usePlus = true;
                  omitEq = false): string =
  ## The inverse. `omitEq` writes a bare key for an empty value.
  result = ""
  for i in 0..<pairs.len:
    if i > 0: result.add '&'
    let (k, v) = pairs[i]
    result.addEncoded(toOpenArray(k, 0, k.len - 1), usePlus)
    if v.len > 0 or not omitEq:
      result.add '='
      result.addEncoded(toOpenArray(v, 0, v.len - 1), usePlus)

# ---------------------------------------------------------- path safety ---

func normalizedPath*(path: openArray[char]; dest: var string): bool =
  ## RFC 3986 §5.2.4 `remove_dot_segments`, with the one addition that matters
  ## to a server: `false` when the path tried to climb above its own root.
  ##
  ## The RFC discards such a `..` silently, which is right for resolving a
  ## reference and wrong for serving a file — a request for `/../etc/passwd`
  ## is not a request for `/etc/passwd`, it is a request that should not be
  ## answered. Reporting it is what lets a server say 400 instead of quietly
  ## serving something adjacent to what was asked for.
  ##
  ## Operates on a *decoded* path. See this module's header for why that
  ## order is the whole of the defence.
  dest = ""
  let absolute = path.len > 0 and path[0] == '/'
  # Segment starts, as offsets into `path`. Only the boundaries are recorded,
  # so nothing is copied until the answer is known.
  var starts = newSeq[int](0)
  var ends = newSeq[int](0)
  var i = 0
  while i < path.len:
    while i < path.len and path[i] == '/': inc i
    if i >= path.len: break
    let s = i
    while i < path.len and path[i] != '/': inc i
    let isDot = (i - s == 1 and path[s] == '.')
    let isDotDot = (i - s == 2 and path[s] == '.' and path[s + 1] == '.')
    if isDot:
      discard
    elif isDotDot:
      if starts.len > 0:
        discard starts.pop()
        discard ends.pop()
      else:
        # Above the root. For a relative path this is a legal `../`; for an
        # absolute one there is nothing above `/` and the request is bad.
        if absolute: return false
        starts.add s
        ends.add i
    else:
      starts.add s
      ends.add i
  if absolute: dest.add '/'
  for k in 0..<starts.len:
    if k > 0: dest.add '/'
    for j in starts[k]..<ends[k]: dest.add path[j]
  # A path that ended in `/`, `/.` or `/..` names a directory, and the
  # trailing slash is the part of it that says so.
  if path.len > 0 and starts.len > 0 and
     (path[path.len - 1] == '/' or
      (path.len >= 2 and path[path.len - 1] == '.' and path[path.len - 2] == '/') or
      (path.len >= 3 and path[path.len - 1] == '.' and path[path.len - 2] == '.' and
       path[path.len - 3] == '/')):
    dest.add '/'
  result = true

func safePath*(target: openArray[char]; dest: var string): bool =
  ## A request target to a path that is safe to join to a document root, or
  ## `false`. This is the one proc a static file server should be calling.
  ##
  ## Decodes first, then normalizes — the order is the defence, see the module
  ## header — and then refuses three things the normalizer has no opinion
  ## about but a filesystem does:
  ##
  ## * a path that is not absolute, because a relative one resolves against
  ##   whatever the process's working directory happens to be;
  ## * an embedded NUL, which every C API reads as the end of the string and
  ##   so turns `/safe.txt\0/../../etc/passwd` into two different paths
  ##   depending on who is looking;
  ## * a backslash, which is a separator on Windows and a literal filename
  ##   character everywhere else — the same request would then mean two things
  ##   on two hosts, and a cross-platform server has to pick one.
  ##
  ## The query is stripped: it is not part of the path, and a target that
  ## still has one has not been split yet.
  dest = ""
  var stop = target.len
  for i in 0..<target.len:
    if target[i] == '?' or target[i] == '#':
      stop = i
      break
  var decoded = ""
  if not decodeUrl(toOpenArray(target, 0, stop - 1), decoded, false):
    return false
  if decoded.len == 0 or decoded[0] != '/': return false
  for i in 0..<decoded.len:
    if decoded[i] == '\0' or decoded[i] == '\\': return false
  result = normalizedPath(toOpenArray(decoded, 0, decoded.len - 1), dest)
  if not result: dest = ""

# ----------------------------------------------------------- combining ----

func combine*(base, rel: Uri): Uri =
  ## Resolve `rel` against `base`, RFC 3986 §5.3-ish: enough for a redirect's
  ## `Location`, which is what a client actually needs it for.
  if rel.scheme.len > 0:
    result = rel
    var p = ""
    if normalizedPath(toOpenArray(rel.path, 0, rel.path.len - 1), p):
      result.path = p
    return
  result = base
  result.query = rel.query
  result.anchor = rel.anchor
  if rel.hostname.len > 0:
    result.username = rel.username
    result.password = rel.password
    result.hostname = rel.hostname
    result.port = rel.port
    result.isIpv6 = rel.isIpv6
    result.path = rel.path
    return
  if rel.path.len == 0:
    if rel.query.len == 0: result.query = base.query
    return
  var merged = ""
  if rel.path[0] == '/':
    merged = rel.path
  else:
    var cut = -1
    for i in countdown(base.path.len - 1, 0):
      if base.path[i] == '/':
        cut = i
        break
    if cut >= 0:
      for i in 0..cut: merged.add base.path[i]
    else:
      merged.add '/'
    merged.add rel.path
  var p = ""
  if normalizedPath(toOpenArray(merged, 0, merged.len - 1), p):
    result.path = p
  else:
    result.path = merged

func `/`*(base: Uri; rel: string): Uri =
  ## Join one more path segment, with exactly one slash between.
  result = base
  var p = base.path
  let hasSlash = p.len > 0 and p[p.len - 1] == '/'
  let wantsSlash = rel.len > 0 and rel[0] == '/'
  if hasSlash and wantsSlash:
    for i in 1..<rel.len: p.add rel[i]
  elif not hasSlash and not wantsSlash:
    p.add '/'
    p.add rel
  else:
    p.add rel
  result.path = p

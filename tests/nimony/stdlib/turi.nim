# lib/std/uri — parsing, percent-coding, queries, and path safety.

import std / [uri, assertions, syncio]

proc show(s: string) =
  let u = parseUri(s)
  var line = "scheme=" & u.scheme & " user=" & u.username & " host=" & u.hostname &
             " port=" & u.port & " path=" & u.path & " query=" & u.query &
             " anchor=" & u.anchor
  if u.opaque: line.add " opaque"
  if u.isIpv6: line.add " ipv6"
  echo line
  # Everything the splitter split, the speller must put back.
  assert $u == s, "round trip: " & $u & " != " & s

proc testParse =
  show("http://example.com/a/b?x=1&y=2#top")
  show("https://user:pw@example.com:8443/p")
  show("http://[::1]:8080/p")
  show("http://[2001:db8::1]/")
  show("/just/a/path?q=1")            # origin-form, the usual request target
  show("/")
  show("*")                            # asterisk-form, for OPTIONS
  show("mailto:me@example.com")        # opaque: no authority
  show("//example.com/p")              # protocol-relative
  # CONNECT's authority-form. RFC 3986 reads it as a scheme plus an opaque
  # part, which is what comes back — a server that handles CONNECT splits the
  # target itself rather than asking a generic URI parser to guess.
  show("localhost:8080")
  show("http://example.com/p#a?b")     # a '?' inside a fragment is not a query

proc testCoding =
  echo encodeUrl(toOpenArray("a b/c?d", 0, 6))
  echo encodeUrl(toOpenArray("a b", 0, 2), usePlus = true)
  echo encodeUrl(toOpenArray("~-._safe", 0, 7))

  var d = ""
  assert decodeUrl(toOpenArray("a%20b", 0, 4), d) and d == "a b"
  assert decodeUrl(toOpenArray("a+b", 0, 2), d) and d == "a+b",
    "a '+' is a literal plus outside a form-encoded query"
  assert decodeUrl(toOpenArray("a+b", 0, 2), d, decodePlus = true) and d == "a b"
  assert not decodeUrl(toOpenArray("a%2", 0, 2), d), "truncated escape"
  assert not decodeUrl(toOpenArray("a%ZZ", 0, 3), d), "non-hex escape"
  assert not decodeUrl(toOpenArray("%", 0, 0), d)
  assert d.len == 0, "a rejected decode must leave nothing behind"

proc testQuery =
  for (k, v) in decodeQuery(toOpenArray("a=1&b=two+words&c&=empty&d=%2F", 0, 29)):
    echo "  [", k, "] = [", v, "]"
  # A malformed pair is dropped whole; the good ones around it survive.
  for (k, v) in decodeQuery(toOpenArray("ok=1&bad=%ZZ&also=2", 0, 18)):
    echo "  <", k, "> = <", v, ">"
  echo encodeQuery([("a", "1"), ("b", "two words"), ("c", "")])

proc sp(target: string): string =
  var p = ""
  if safePath(toOpenArray(target, 0, target.len - 1), p): p else: "REFUSED"

proc testSafePath =
  echo sp("/a/b/c")
  echo sp("/a/./b")
  echo sp("/a/b/../c")
  echo sp("/a//b")
  echo sp("/a/b/")
  echo sp("/a/b/.")
  echo sp("/index.html?q=1#frag")       # the query is not part of the path
  echo sp("/caf%C3%A9.txt")             # a decoded escape stays a filename
  # Traversal, in the four spellings that matter. `%2e%2e%2f` is the one that
  # gets past a server that normalizes before it decodes.
  echo sp("/../etc/passwd")
  echo sp("/a/../../etc/passwd")
  echo sp("/%2e%2e/etc/passwd")
  echo sp("/a/%2e%2e%2f%2e%2e%2fetc/passwd")
  # Not a path, or not one a filesystem can be handed.
  echo sp("a/b")                        # relative
  echo sp("/a%00/b")                    # embedded NUL
  echo sp("/a\\b")                      # a separator on one platform only
  echo sp("/a%ZZ")                      # malformed escape

proc testCombine =
  let base = parseUri("http://example.com/a/b/c?old#frag")
  echo $combine(base, parseUri("d"))
  echo $combine(base, parseUri("/d"))
  echo $combine(base, parseUri("../d"))
  echo $combine(base, parseUri("?new"))
  echo $combine(base, parseUri("https://other/x"))
  echo $(parseUri("http://h/a") / "b")
  echo $(parseUri("http://h/a/") / "b")
  echo $(parseUri("http://h/a/") / "/b")
  assert isAbsolute(parseUri("http://h/a"))
  assert not isAbsolute(parseUri("/a"))
  assert isAbsolute(parseUri("mailto:x@y"))

testParse()
testCoding()
testQuery()
testSafePath()
testCombine()

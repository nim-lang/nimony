# Header helpers work with the shared module alone, without importing a client.
import std/[http/httpheaders, assertions, syncio]

proc checkHeaders() {.raises.} =
  var h = headers"Accept: application/json\r\nSet-Cookie: a=1\r\n"
  h.add "set-cookie", "b=2"
  h.add "X-Empty", ""
  h.add "X-Padded", " \tvalue\t "

  assert h.get("ACCEPT") == "application/json"
  assert h.get("Set-Cookie") == "a=1"
  assert h.get("missing") == ""
  assert h.get("x-empty") == ""
  assert h.has("X-EMPTY")
  assert not h.has("missing")
  assert h.get("x-padded") == "value"

  var cookies: seq[string] = @[]
  for value in h.values("SET-COOKIE"): cookies.add value
  assert cookies == @["a=1", "b=2"]

  var fields: seq[(string, string)] = @[]
  for name, value in h.pairs: fields.add (name, value)
  assert fields == @[("Accept", "application/json"), ("Set-Cookie", "a=1"),
    ("set-cookie", "b=2"), ("X-Empty", ""), ("X-Padded", "value")]
  assert string(h) == "Accept: application/json\r\nSet-Cookie: a=1\r\n" &
    "set-cookie: b=2\r\nX-Empty: \r\nX-Padded:  \tvalue\t \r\n"

  # Failed insertion leaves an existing block untouched.
  let before = string(h)
  for (name, value) in [("", "value"), ("Bad Name", "value"),
                        ("X-Trace", "a\r\nInjected: yes"), ("X-Trace", "a\n")]:
    var rejected = false
    try:
      h.add name, value
    except ErrorCode as err:
      assert err == BadOperation
      rejected = true
    assert rejected
    assert string(h) == before

  let empty = headers""
  assert not empty.has("x")
  assert empty.get("x") == ""
  var count = 0
  for name, value in empty.pairs: inc count
  for value in empty.values("x"): inc count
  assert count == 0

try:
  checkHeaders()
except ErrorCode as err:
  assert false, $err
echo "shared header helpers passed"

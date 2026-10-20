# Qualified helpers, including the literal plugin, remain available through
# httpclient. Importing both paths refers to the same definitions.
import std/[httpclient, http/httpheaders, assertions, syncio]

proc checkExports() {.raises.} =
  var h = httpclient.headers("X-Trace: one\r\n")
  httpclient.add(h, "x-trace", "two")
  assert httpclient.get(h, "X-TRACE") == "one"
  assert httpclient.has(h, "x-trace")
  var values: seq[string] = @[]
  for value in httpclient.values(h, "X-Trace"): values.add value
  assert values == @["one", "two"]
  var count = 0
  for name, value in httpclient.pairs(h): inc count
  assert count == 2
  assert httpheaders.get(h, "X-Trace") == "one"
  assert get(h, "X-Trace") == "one"

try:
  checkExports()
except ErrorCode as err:
  assert false, $err
echo "httpclient header exports passed"

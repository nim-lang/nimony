# nim-lang/nimony#2940: `TokenBuf`'s custom `=destroy` released only its token
# storage, never `openTags`, `pool` or `tags`, so every retired `HttpMsg` leaked.

import std/[assertions, syncio]
import std/http/httpmsg as msg
import std/http/httpparse as parser

let tags = msg.newHttpTags() # one shared vocabulary for the whole process

proc parseOne() =
  var head = msg.initHttpMsg(tags)
  let wire = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
  assert parser.parseRequestHead(
    toOpenArray(wire, 0, wire.len - 1), head) == wire.len
  assert head.getStr(msg.hHost) == "localhost"

proc emptyOne() =
  var head = msg.initHttpMsg(tags)
  discard head

for i in 0..<10:
  parseOne()
  emptyOne()
let baseline = getOccupiedMem()
for i in 0..<100:
  parseOne()
  emptyOne()
let retained = getOccupiedMem() - baseline
assert retained <= 256, "retired HTTP messages release their owned storage"
echo "httpmsg retains nothing: ", retained <= 256

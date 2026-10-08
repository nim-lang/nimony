# httpcoding's negotiation, without a socket: which coding an
# `Accept-Encoding` asks for, which media types are worth compressing, and
# what a message's `Content-Encoding` says it is in.

import std / [http/httpcoding, http/httpmsg, http/httpparse, assertions,
              syncio]

proc show(ae: string) =
  echo "'", ae, "' -> ", chooseCoding(toOpenArray(ae, 0, ae.len - 1))

show "gzip, deflate, br"
show "deflate"
show "br, deflate;q=0.5, gzip;q=0.4"
show "gzip;q=0, *"
show "gzip;q=0, deflate;q=0"
show "*;q=0"
show "*"
show "identity"
show ""
show " GZIP ; Q=1.0 "
show "x-gzip"
show "gzip;q=0.000, deflate;q=0.001"

for ct in ["text/html", "text/html; charset=utf-8", "TEXT/CSS",
           "application/json", "application/ld+json", "image/svg+xml",
           "text/event-stream", "image/png", "application/octet-stream",
           "application/gzip", ""]:
  echo ct, " compressible=", isCompressible(toOpenArray(ct, 0, ct.len - 1))

let tags = newHttpTags()
for ce in ["gzip", "deflate", "identity", "x-gzip", "br", "gzip, br"]:
  var m = initHttpMsg(tags)
  let head = "HTTP/1.1 200 OK\r\nContent-Encoding: " & ce & "\r\n\r\n"
  assert parseResponseHead(toOpenArray(head, 0, head.len - 1), m) > 0
  echo "content-encoding ", ce, " -> ", codingOf(m)

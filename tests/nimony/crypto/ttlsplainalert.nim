# Once records are protected, an unprotected record -- even a `close_notify`
# alert -- is a protocol failure, not the end of the stream: anyone on the
# path could otherwise forge one and truncate the response unnoticed.
import std/syncio
include std/tls

proc main =
  var e = newTlsEngine(default(TrustStore))
  e.state = hsConnected
  e.encrypted = true
  let forged = [char(ContentAlert), '\3', '\3', '\0', '\2', '\1', '\0']
  var used = 0
  var raised = false
  try:
    e.recordsIn(forged, used)
  except:
    raised = true
  echo "raised ", raised
  echo "peerClosed ", e.isPeerClosed
  echo "protocol failure ", e.failure == tfProtocol

main()

# `headers"..."` is checked while the program is compiled: header injection
# and a missing line end are errors at the literal, not surprises at send time.
import std/httpclient

let good = headers"Accept: application/json\r\nX-Trace: 1\r\n"
let injected = headers"X-Evil: a\nInjected: yes\r\n"
let unterminated = headers"Accept: text/plain"
let badName = headers"Bad Name: 1\r\n"

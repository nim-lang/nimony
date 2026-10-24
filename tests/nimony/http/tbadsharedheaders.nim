# The shared module exposes the same compile-time literal validation.
import std/http/httpheaders

let good = headers"Accept: application/json\r\nX-Trace: 1\r\n"
let injected = headers"X-Evil: a\nInjected: yes\r\n"
let unterminated = headers"Accept: text/plain"
let badName = headers"Bad Name: 1\r\n"

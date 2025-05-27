import std/syncio

type Reader = object
  p, eof: int

template useCpuRegisters(body: untyped) {.untyped.} =
  var p {.inject.} = r.p
  let eof {.inject.} = r.eof
  body
  r.p = p # store back

proc doReader(r: var Reader) =
  useCpuRegisters:
    while p < eof:
      echo p + 1
      inc p
  echo "final p: ", r.p

var r = Reader(p: 0, eof: 5)
doReader(r)

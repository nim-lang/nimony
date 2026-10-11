# A local in an `{.untyped.}` template body whose type is module-qualified.
import std/syncio
import std/syncio as sio

proc show(a, b: sio.FileMode) = echo ord(a), " ", ord(b)

template pick(useWrite: bool; emitter: untyped) {.untyped.} =
  var m: sio.FileMode
  if useWrite: m = fmWrite
  else: m = fmReadWrite
  emitter(fmRead, m)

proc main(useWrite: bool) =
  template local(emitter: untyped) {.untyped.} =
    var m: sio.FileMode = fmAppend
    emitter(m, m)
  pick(useWrite, show)
  local(show)

main(true)
main(false)


import std / syncio
import deps/mlistener

template onChanged(x: var Listener, field: untyped, name: string): untyped =
  echo name, " changed to ", field

var p = Listener(i: 1, s: "a")
p.i = 2
p.s = "b"
p.i = 3


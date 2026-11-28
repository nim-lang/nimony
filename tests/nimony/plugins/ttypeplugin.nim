
import std / [assertions, syncio, tables]
import deps / [m3105, mlistener]

template onChanged(x: var Listener, field: untyped, name: string): untyped =
  echo name, " changed to ", field

var p = Listener(i: 1, s: "a")
p.i = 2
p.s = "b"
p.i = 3

proc checkGenericHooks() =
  var listeners = mkListeners()
  listeners.add Listener(i: 8, s: "second")
  let values = mk()
  var table = initTable[string, string]()
  table["a"] = "b"
  assert listeners.len == 2
  assert listeners[0].i == 7
  assert listeners[0].s == "first"
  assert listeners[1].i == 8
  assert listeners[1].s == "second"
  assert values[0].x == 1
  assert table.getOrDefault("a") == "b"

checkGenericHooks()

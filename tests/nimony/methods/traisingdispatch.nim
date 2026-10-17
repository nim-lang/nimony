# A `.raises` method called through its base type. The vtable slot is called
# through a cast to the method's proc type, which lacked `.raises`: the C
# backend then read `void` where the code is returned (a compile error), and
# the native backend reported a raise that never happened.
import std/syncio

type
  Adapter = ref object of RootObj
  Upper = ref object of Adapter
    calls: int
  Strict = ref object of Adapter

method run(a: Adapter; src: openArray[char]; dest: var string) {.base, raises.} =
  raise BadOperation

method run(a: Upper; src: openArray[char]; dest: var string) {.raises.} =
  for c in src: dest.add(if c in {'a'..'z'}: char(ord(c) - 32) else: c)
  inc a.calls

method run(a: Strict; src: openArray[char]; dest: var string) {.raises.} =
  if src.len > 3: raise FullError
  for c in src: dest.add c

proc use(a: Adapter; s: string) =
  var d = ""
  try:
    a.run(s, d)
    echo "ok: ", d
  except ErrorCode as e:
    echo "raised: ", e

let u = Upper(calls: 0)
use(u, "abc")
use(u, "xyz")
echo u.calls
use(Strict(), "ab")
use(Strict(), "abcd")
use(Adapter(), "x")

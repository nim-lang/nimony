# issue #2686: `p.f` loads and stores through a pointer register. Native-only,
# run by `hastur nativetests`.
import std/syncio
type
  Obj = object
    a, b: uint64
  PObj = ptr Obj

proc getB(o {.register: "rdi".}: PObj): uint64 {.assembler.} =
  var y {.register: "rax".}: uint64
  y = o.b
  result = y

proc setA(o {.register: "rdi".}: PObj; v {.register: "rsi".}: uint64) {.assembler.} =
  o.a = v

var o = Obj(a: 100'u64, b: 7'u64)
setA(addr o, 42'u64)
echo getB(addr o), " ", o.a

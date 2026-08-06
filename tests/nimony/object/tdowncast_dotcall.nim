import std/assertions

# A checked downcast re-semmed through the method-call-syntax rewrite:
# `Sub(p).xs.len` sems the receiver `(ddot (baseobj T -1 p) xs)` once, then
# `.len` fails field lookup and is rebuilt as `len(receiver)`, re-semming
# the already-semmed tree. semBaseobj's typematch only knew the implicit
# (upcast) direction, so the -1 downcast "mismatched" — and the failure
# path left `[type][err]` in dest where callers read one tree, silently
# truncating the operand to a bare type that the C code generator then
# rejected with "expected expression but got: (ptr ...)".

type
  Base = ref object of RootObj
    kind: int
  Sub = ref object of Base
    xs: seq[int]

proc f(b: Base): bool =
  var p = b
  p.kind == 1 and Sub(p).xs.len > 0

proc main =
  let s = Sub(kind: 1, xs: @[1, 2, 3])
  assert f(s)

main()

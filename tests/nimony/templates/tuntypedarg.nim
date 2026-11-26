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

block:
  var r = Reader(p: 0, eof: 5)
  doReader(r)

# bug #1305

proc bar(x: int) = echo 1234

template foo(rune: untyped): untyped {.untyped.} =
  rune(12)

proc bar(x: string) =
  foo(bar)

bar("123")

# `x.each` is `each(x)`: the `untyped` parameter gets `x` unchecked
iterator split(s: string): string = yield "iter"
proc split(s: string): seq[string] = @["proc"]
template each(x: untyped) =
  for y in x: echo y
each("a b".split)
"a b".split.each
template `each=`(x: untyped; v: int) =
  for y in x: echo y, v
"a b".split.each = 1

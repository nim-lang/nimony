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

# constructors over a template parameter get their type at expansion

template tupOf(x: untyped): untyped = (x, x)
template namedOf(x: untyped): untyped = (a: x, b: 1)
template typedOf(x: typed): untyped = (x, x)
template arrOf(x: untyped): untyped = [x, x]
template setOf(x: untyped): untyped = {x, 'b'}
template mixedOf(x: untyped): untyped = ("k", x, [1, 2])

proc constructors =
  var t = tupOf(2)
  let n = namedOf("s")
  let ty = typedOf(2.5)
  let a = arrOf("e")
  let m = mixedOf(3)
  echo t[0], " ", n.a, n.b, " ", ty[1], " ", a[1], " ", card(setOf('a')), " ", m[0], m[1], m[2][1]

constructors()
var topTup = tupOf(@[1, 2])
echo topTup[1].len

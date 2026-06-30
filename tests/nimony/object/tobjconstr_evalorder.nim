import std/[syncio, assertions]

# nim-lang/nimony#1056: object-constructor fields and named call arguments are
# re-emitted in declaration / parameter order, so their value expressions run in
# that order regardless of how they are written. The compiler warns about the
# mismatch (see the PR) but deliberately does NOT change evaluation order. This
# test pins that semantic: evaluation stays in declaration / parameter order.

type Foo = object
  a, b, c: int

var trace = ""
proc mark(s: string; v: int): int =
  trace.add s
  v

# Fields written b, a — but evaluated a, b (declaration order), not "ba":
let f = Foo(b: mark("b", 2), a: mark("a", 1))
assert f.a == 1 and f.b == 2 and f.c == 0
assert trace == "ab"

trace = ""
proc g(x, y, z: int): int = x * 100 + y * 10 + z
# Named args written y, x, z — but evaluated x, y, z (parameter order), not "yxz":
let n = g(y = mark("y", 2), x = mark("x", 1), z = mark("z", 3))
assert n == 123
assert trace == "xyz"

# In declaration / parameter order already (no reordering, no warning):
trace = ""
let f2 = Foo(a: mark("a", 1), b: mark("b", 2), c: mark("c", 3))
assert trace == "abc"
trace = ""
let n2 = g(mark("x", 1), mark("y", 2), mark("z", 3))
assert n2 == 123
assert trace == "xyz"

echo "ok"

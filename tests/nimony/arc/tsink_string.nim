
import std / [syncio, assertions]

proc x(s: sink string) =
  let foo = ensureMove(s)
  echo foo

x("hi")

type
  Foo = object
    buf: string

proc test(f: sink Foo): string =
  result = move(f.buf)

var f = Foo(buf: "1234")
assert test(f) == "1234"

import std/syncio

type Foo = object
  a, b: int

proc foo(): int =
  echo "evaluated first"
  123

proc bar(): int =
  echo "evaluated second"
  456

let x = Foo(
  b: foo(),
  a: bar())

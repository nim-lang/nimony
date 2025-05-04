import std/syncio

bar()

proc bar() =
  foo()

when not defined(nimony):
  type
    Foo = Bar
    Bar = int
  proc foo() = echo "abc"
else:
  type
    Foo = Bar
    Bar = int
  proc foo() = echo "def"

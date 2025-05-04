import std/syncio

bar()

proc bar() =
  foo()

when not defined(nimony):
  type
    Foo = Bar
    Bar = int
  proc foo() = foo2()
  proc foo2() = echo "abc"
else:
  type
    Foo = Bar
    Bar = int
  proc foo() = foo2()
  proc foo2() = echo "def"

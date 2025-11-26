import std/syncio

block:
  type
    Foo = ref object of RootObj
      id: int

  var s = Foo(id: 12)
  echo s.id

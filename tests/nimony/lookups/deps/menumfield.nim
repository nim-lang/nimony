type Foo* = enum
  cmdEnd

proc useFoo() =
  let a1 = cmdEnd
  let a2: Foo = a1
  let b1 = Foo.cmdEnd
  let b2: Foo = b1

type
  FooEnum = enum
    abc

  BarObj = object
    kind: FooEnum

iterator testIter*(): int =
  var f = BarObj(kind: abc)
  f.kind = abc

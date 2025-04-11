import deps/mtypeclassmatch

foo1(123)
foo1(e0)
#foo2(123)
foo2(e0)
foo3(123)
#foo3(e0)
foo4(123)
foo4(e0)

proc union[T: int | EnumA](x: T) =
  if false:
    union(x)

union(123)
union(e0)

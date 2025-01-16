# does not fully compile yet because types in procs are not lifted

proc foo[T](x: T) =
  type Foo = object
    val: T
  var obj = Foo(val: x)

foo(123)
foo("abc")

template fooTempl[T](x: T) =
  type Foo = object
    val: T
  var obj = Foo(val: x)

fooTempl(123)
fooTempl("abc")

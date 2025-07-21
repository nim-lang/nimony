# see issue #1301
template foo[T](_: T) =
  var x: T

foo(undefined)
discard sizeof(undefinedType)

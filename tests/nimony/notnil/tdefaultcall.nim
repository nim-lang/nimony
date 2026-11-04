# `default(T)` is how `system/defaults` spells "the value zeroed storage has",
# and for a pointer it is `T(nil)`. For a NOT-NIL `T` there is no such value,
# and the conversion does not make one -- neither directly nor under an
# `array[I, T]`, whose `default` is a loop of `default(T)`.
type Foo = ref object
  x: int

proc main =
  var a = default(array[8, Foo])
  discard a

main()

type
  GObj[T] = ref object
    data: T

  AObj = ref object
    data: string

proc main =
  var obj = BGObj[string](data: "abc")

  var a = AObj(data: "abc")

  var objB = GObj[int](data: 456)

main()

import std/assertions

type
  Ref = ref object
    id: int

  RefCustom = object
    id: ptr int

proc `=dup`(x: RefCustom): RefCustom =
  result = RefCustom()
  result.id = x.id

proc inc(x: sink Ref) =
  inc x.id

proc inc(x: sink RefCustom) =
  inc x.id[]

proc foo =
  var x = Ref(id: 8)
  inc(x)
  inc(x)

  var id = 777
  var s = RefCustom(id: addr id)
  inc s
  inc s

foo()

proc foo2 =
  var x = Ref(id: 8)
  inc(x)
  assert x.id == 9
  inc(x)
  assert x.id == 10

  var id = 777
  var s = RefCustom(id: addr id)
  inc s
  assert s.id[] == 778
  inc s
  assert s.id[] == 779

foo2()

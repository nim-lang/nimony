# Test: ref field access via var param compiles fine.

type
  Node = ref object
    value: int

proc modify(a: var int) =
  a = 42

proc testRefFieldAccess =
  var n = Node(value: 10)
  modify(n[].value)
  modify(n.value)

testRefFieldAccess()

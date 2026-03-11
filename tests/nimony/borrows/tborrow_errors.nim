# Test: borrow checking errors.

type
  Obj = object
    x: int
    y: int

  Container = object
    items: seq[int]
    name: string

proc take(a: var int, b: int) =
  a = b

# Mutable argument aliases with immutable parameter (Pascal aliasing):
proc testMutImmAlias =
  var a = 5
  take(a, a)

testMutImmAlias()

# Mutable argument aliases with another mutable argument:
proc both(a: var int, b: var int) =
  let tmp = a
  a = b
  b = tmp

proc testMutMutAlias =
  var a = 5
  both(a, a)

testMutMutAlias()

# Mutable field aliases with the whole object passed immutably:
proc takeFieldAndObj(a: var int, b: Obj) =
  a = b.x

proc testFieldVsObj =
  var o = Obj(x: 1, y: 2)
  takeFieldAndObj(o.x, o)

testFieldVsObj()

# Mutating a container while iterating - simulated via var + read:
proc resize(s: var seq[int], val: int) =
  s.add val

proc testMutateWhileBorrowed =
  var s = @[1, 2, 3]
  # Simulate: for x in mitems(s): s.add(x)
  # The `var` param aliases with the direct mention
  resize(s, s[0])

testMutateWhileBorrowed()

# Nested field path overlaps with parent path:
proc modifyItems(c: var Container, name: string) =
  c.name = name

proc testNestedFieldAlias =
  var c = Container(items: @[1], name: "old")
  modifyItems(c, c.name)

testNestedFieldAlias()

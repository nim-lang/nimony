# Test: borrow checking errors.

type
  Obj = object
    x: int
    y: int

  Container = object
    items: seq[int]
    name: string

proc take(a: var string, b: string) =
  a = b

# Mutable argument aliases with immutable parameter (Pascal aliasing). A
# `string` copy still shares the caller's buffer; an `int` copy would not
# (tborrow_copies.nim):
proc testMutImmAlias =
  var a = "5"
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
proc takeFieldAndObj(a: var string, b: Container) =
  a = b.name

proc testFieldVsObj =
  var o = Container(items: @[1], name: "n")
  takeFieldAndObj(o.name, o)

testFieldVsObj()

# Mutating a container while iterating - simulated via var + read:
proc resize(s: var seq[string], val: string) =
  s.add val

proc testMutateWhileBorrowed =
  var s = @["1", "2", "3"]
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

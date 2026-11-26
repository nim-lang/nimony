import std/assertions

type
  TextBox = object
    id: int
    text: string
  NestedBox = object
    inner: TextBox
  Hooked = object
    value: int
  HookedBox = object
    inner: Hooked

proc `=copy`(dest: var Hooked; src: Hooked) =
  dest.value = src.value + 1

proc `=sink`(dest: var Hooked; src: Hooked) =
  dest.value = src.value + 2

proc checkObjects() =
  let src = TextBox(id: 42, text: "a string long enough to require heap storage")
  var dest = TextBox(id: 1, text: "old")
  `=copy`(dest, src)
  assert dest.id == 42
  assert dest.text == src.text
  assert src.text.len > 0

  let nested = NestedBox(inner: src)
  var copied = NestedBox(inner: TextBox(id: 0, text: ""))
  `=copy`(copied, nested)
  assert copied.inner.id == 42
  assert copied.inner.text == nested.inner.text

proc checkArrays() =
  let src = ["a string long enough to require heap storage", "second"]
  var dest = ["old", "contents"]
  `=copy`(dest, src)
  assert dest[0] == src[0]
  assert dest[1] == src[1]
  assert src[0].len > 0

proc checkTuples() =
  let src = (42, "a string long enough to require heap storage")
  var dest = (1, "old")
  `=copy`(dest, src)
  assert dest[0] == 42
  assert dest[1] == src[1]
  assert src[1].len > 0

proc checkCustomHooks() =
  let src = HookedBox(inner: Hooked(value: 10))
  var dest = HookedBox(inner: Hooked(value: 0))
  `=copy`(dest, src)
  assert dest.inner.value == 11
  assert src.inner.value == 10

  var moved = HookedBox(inner: Hooked(value: 20))
  `=sink`(dest, moved)
  `=wasMoved`(moved)
  assert dest.inner.value == 22

checkObjects()
checkArrays()
checkTuples()
checkCustomHooks()

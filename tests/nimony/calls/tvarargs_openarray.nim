import std/assertions
import mvarargs

proc count[T](items: varargs[T]): int = items.len

proc last[T](items: varargs[T]): T = items[items.len - 1]

proc countStrings(items: varargs[string, `$`]): int = items.len

proc withTail(items: varargs[int]; tail: int): int =
  result = tail
  for item in items:
    result += item

proc forward(items: openArray[int]) =
  assert count(items) == 3
  assert last(items) == 3
  assert echoSum(items) == 6
  assert sumWith(100, items) == 106
  assert withTail(items, 100) == 106

proc empty(items: openArray[int]) =
  assert count(items) == 0
  assert echoSum(items) == 0
  assert withTail(items, 100) == 100

proc forwardStrings(items: openArray[string]) =
  assert count(items) == 2
  assert last(items) == "b"
  assert countStrings(items) == 2

proc forwardVarargs(items: varargs[int]): int = withTail(items, echoSum(items))

proc forwardGeneric[T](items: varargs[T]): T = last(items)

proc lenSum(items: varargs[string, `$`]): int =
  result = 0
  for item in items: result += item.len

proc forwardConverted(items: varargs[string, `$`]): int = lenSum(items)

forward([1, 2, 3])
empty([])
forwardStrings(["a", "b"])
assert count(1, 2, 3) == 3
assert last(1.5, 2.5) == 2.5
assert countStrings("a", 42) == 2
assert forwardVarargs(1, 2, 3) == 12
assert forwardVarargs() == 0
assert forwardGeneric("a", "b") == "b"
assert forwardConverted(1, "ab", 3.5) == 6

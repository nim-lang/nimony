import std/syncio

iterator testIterator(): int = echo "iterator testIterator(): int"
proc testIterator(): int = echo "proc testIterator(): int"

iterator testIterator(x: int): int = echo "iterator testIterator(x: int): int"
proc testIterator(x: int): int = echo "proc testIterator(x: int): int"

block:
  var x = testIterator()
  for i in testIterator():
    discard

  var y = testIterator(0)
  for i in testIterator(0):
    discard

iterator testIteratorGenerics(x: int): int = echo "iterator testIteratorGenerics(x: int): int"
proc testIteratorGenerics[T](x: T): T = echo "proc testIteratorGenerics[T](x: T): T"

block:
  var x = testIteratorGenerics(0)
  for i in testIteratorGenerics(0):
    discard

iterator testIteratorGenerics2[T](x: T): T = echo "iterator testIteratorGenerics2[T](x: T): T"
proc testIteratorGenerics2(x: int): int = echo "proc testIteratorGenerics2(x: int): int"

block:
  var x = testIteratorGenerics2(0)
  for i in testIteratorGenerics2(0):
    discard

iterator testIteratorGenerics3[T](x: T): T = echo "iterator testIteratorGenerics3[T](x: T): T"
proc testIteratorGenerics3[T](x: T): T = echo "proc testIteratorGenerics3[T](x: T): T"

block:
  var x = testIteratorGenerics3(0)
  for i in testIteratorGenerics3(0):
    discard

iterator testIteratorGenericsTU(x, y: int): int = echo "iterator testIteratorGenericsTU(x, y: int): int"
proc testIteratorGenericsTU[T, U](x: T; y: U): T = echo "proc testIteratorGenericsTU[T, U](x: T; y: U): T"

block:
  var x = testIteratorGenericsTU(0, 1)
  for i in testIteratorGenericsTU(0, 1):
    discard

iterator testIteratorConv(x: int): int = echo "iterator testIteratorConv(x: int): int"
proc testIteratorConv(x: float): float = echo "proc testIteratorConv(x: float): float"

block:
  var x = testIteratorConv(0)
  for i in testIteratorConv(0):
    discard

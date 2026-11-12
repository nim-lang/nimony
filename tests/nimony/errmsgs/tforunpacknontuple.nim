# Loop variables that unpack need a tuple: a positioned error, not a crash,
# also in a generic body, and no follow-up error when the iterator call
# already failed.

for (a, b) in nosuchIterator(3):
  discard

for x, (y, z) in nosuchIterator(3):
  discard

for (a, b) in [1, 2]:
  discard

for i, (b, c) in [1, 2]:
  discard

for a, b in countup(1, 3):
  discard

proc unpackAll[T](s: seq[T]) =
  for (a, b) in s:
    discard

unpackAll(@[1])

type
  IntIter = concept
    iterator items(x: Self): int

proc unpackConcept[T: IntIter](x: T) =
  for (a, b) in items(x):
    discard

## Regression test: a multi-yield iterator that wraps a body containing a
## nested for-loop must produce DISTINCT C labels per yield expansion.
## Previously `inlineLoopBody` pre-inlined nested for-stmts into the buffer
## used by `transformForStmt`, so a 2-yield outer iter would emit the same
## `forStmtLabel.N` label twice in one C function (gcc: duplicate label).

iterator pair(x: int): int =
  if x > 0:
    yield 1            # yield 1: simple
  else:
    for k in 0 ..< x:
      yield k          # yield 2: inside a nested for-loop in the iter body

iterator pair2(x: int): int =
  if x > 0:
    yield 2
  else:
    for k in 0 ..< x:
      yield k

proc total(): int =
  var acc = 0
  for a in pair(3):
    for b in pair2(a):
      for c in 0 ..< b:
        acc = acc + c
  result = acc

discard total()

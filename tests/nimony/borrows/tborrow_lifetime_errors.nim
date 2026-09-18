# Test: borrow lifetime errors - mutation of borrowed path during iteration.

proc addItem(s: var seq[int]; val: int) =
  s.add val

proc deleteItem(s: var seq[int]; idx: int) =
  s.del idx

# Mutate seq while iterating:
proc testMutateWhileIterating =
  var s = @[1, 2, 3]
  for x in s:
    addItem(s, x)

testMutateWhileIterating()

# Delete from seq while iterating:
proc testDeleteWhileIterating =
  var s = @[1, 2, 3]
  for x in s:
    deleteItem(s, 0)

testDeleteWhileIterating()

# Mutate seq while iterating with a BORROWING iterator. Here the loop variable
# itself is `var T`, so it borrows the container directly — a different source
# of the borrow than the two cases above, and the one the Final IR lowering used
# to state by fabricating a second declaration of the binder. The prover derives
# it from the iterator call now, so this pins that it still does.
proc testMutateWhileBorrowingIter =
  var s = @[1, 2, 3]
  for x in mitems(s):
    addItem(s, x)

testMutateWhileBorrowingIter()

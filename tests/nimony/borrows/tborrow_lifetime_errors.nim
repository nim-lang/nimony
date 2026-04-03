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

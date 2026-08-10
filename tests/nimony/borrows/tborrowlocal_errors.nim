# Borrowing from a global is allowed (see `tborrowglobal.nim`) because a global
# outlives every borrow of it. Borrowing from a LOCAL is NOT: this pins down that
# the relaxation stayed narrow.

type Box = ref object
  v: int

proc bad(): lent Box =
  var localOnly = Box(v: 1)
  return localOnly

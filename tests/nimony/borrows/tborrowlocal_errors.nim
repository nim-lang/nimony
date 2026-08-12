# A borrow must be rooted in the proc's FIRST PARAMETER. Nothing else qualifies —
# not a local, and not a global either: a global's lifetime outlives the borrow,
# but reassigning it under a live borrow still frees the referent, so the root
# rule stays where it is. This pins down the local case.

type Box = ref object
  v: int

proc bad(): lent Box =
  var localOnly = Box(v: 1)
  return localOnly

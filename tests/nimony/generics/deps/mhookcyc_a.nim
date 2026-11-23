import mhookcyc_b {.cyclic.}

proc useHolder*() =
  var h = CycHolder[int](v: 1)

# `owned` is only valid for `ref` and closure types: `seq` is already unique
{.feature: "ownedRefs".}
type
  S = owned seq[int]
var x: S
# a subscript of the rejected `owned` tuple must not crash nimsem
var t: owned (int, int)
discard t[1]

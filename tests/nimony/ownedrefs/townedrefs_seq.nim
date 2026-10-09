# `owned` is only valid for `ref` and closure types: `seq` is already unique
{.feature: "ownedRefs".}
type
  S = owned seq[int]
var x: S

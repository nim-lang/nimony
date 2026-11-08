# `seq`, `string` and `openArray` are not objects for the `object` type class:
# `fields` and `fieldPairs` do not walk their private fields.
proc walkOpen(a: openArray[int]) =
  for v in fields(a): discard

for v in fields(@[1]): discard
for k, v in fieldPairs(@[1]): discard
for v in fields("ab"): discard
for k, v in fieldPairs("ab"): discard

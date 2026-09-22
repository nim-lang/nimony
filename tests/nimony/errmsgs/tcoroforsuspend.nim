# A `for` loop over a `.passive` iterator is a trampoline inside ONE state
# proc, so its body cannot suspend: there is nowhere for the state boundary to
# go. Refused rather than mislowered.

proc step() {.passive.} = discard

iterator count(n: int): int {.passive.} =
  var i = 1
  while i <= n:
    yield i
    inc i

proc consume() {.passive.} =
  for x in count(3):
    step()

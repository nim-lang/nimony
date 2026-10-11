# A bound on a variable can be shifted past xint's range: after a guarded
# early return, `result = by - ay` leaves the vacuous lower bound
# `0 <= result + high(xint)`, and the following `result = result - 1` shifts it
# to `high + 1` — NaN. The shift used to store that NaN fact, and the next
# confluence (`join` on the way out of the proc) asserted on it (#2535). A
# shift that overflows bounds nothing: the fact is dropped, the code compiles,
# and the proc still computes what it says.
import std/syncio

proc yearsBetween(ay, am, by, bm: int): int =
  if by < ay: return 0
  result = by - ay
  if bm < am:
    result = result - 1

proc countdown(ay, by: int): int =
  if by < ay: return 0
  var r = by - ay
  r = r - 1
  result = r

echo yearsBetween(1980, 5, 2026, 9)
echo yearsBetween(1980, 11, 2026, 9)
echo yearsBetween(2030, 1, 2026, 9)
echo countdown(3, 10)

## Exceptions: nimony's heap-exception lowering through the JS backend —
## raise, try/except, and control flow continuing after a catch.
{.feature: "canraise".}
import std/syncio

proc checked(n: int) {.raises: ref Exception.} =
  if n < 0:
    raise (ref Exception)(msg: "negative")
  echo n * 2

try:
  checked(5)
  checked(-1)
except:
  echo "caught negative"
echo "after"

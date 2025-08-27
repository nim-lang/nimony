
import std / [syncio]

proc foo(r: bool) {.raises.} =
  echo "hi"
  if r:
    raise SyntaxError

try:
  foo(true)
except:
  echo "caught"
finally:
  echo "A finally"

try:
  foo(false)
except:
  echo "you are not supposed to see this"
finally:
  echo "B finally"


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


type
  MyObj = object
    value: int

# Custom destructor
proc `=destroy`(x: var MyObj) =
  echo "Destructor called for MyObj(value=", x.value, ")"

proc main() =
  try:
    var o = MyObj(value: 123)
    echo "Object initialized"
  finally:
    echo "Inside finally block"

main()
echo "Program end"

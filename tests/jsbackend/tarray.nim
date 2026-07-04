## JS floats and arrays through the interop layer — the value shapes a state /
## DOM library needs beyond scalars and objects (list children, coordinates).
import std/syncio
import jsffi

# float round-trips as a JS Number (3.5 is exact in binary → stable formatting)
let x = toJs(3.5)
echo "float round-trip: ", toFloat(x)

# build a JS array from Nim, mixing element types
let a = newJsArray()
a.add(toJs("x"))
a.add(toJs(42))
a.add(toJs(true))
echo "len: ", a.len
echo "a[0]: ", $a[0]
echo "a[1] as int: ", toInt(a[1])

# index assignment
a[1] = toJs(99)
echo "a[1] after set: ", toInt(a[1])

# hand the array to real JS: JSON.stringify and Array.prototype.join
let json = global("JSON")
echo "json: ", $json.call("stringify", a)
echo "joined: ", $a.call("join", toJs("-"))

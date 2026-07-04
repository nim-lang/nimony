## JS FFI: marshal Nim values across the linear-memory boundary into real JS
## values, call host JS (console/Math/JSON), read results back, and exercise the
## whole `jsffi` surface (toJs/toStr/toInt/toBool, global/get/set/call, ==, isNil).
import std/syncio
import jsffi

# 1. Nim string -> real JS string -> host console.log (writes to stdout).
let console = global("console")
discard console.call("log", toJs("hello from Nim through a real JS string"))

# 2. Call JS methods (2- and 3-arg), read the numbers back into Nim ints.
let m = global("Math")
echo "Math.max(3, 7)    = " & $toInt(m.call("max", toJs(3), toJs(7)))
echo "Math.max(3, 7, 5) = " & $toInt(m.call("max", toJs(3), toJs(7), toJs(5)))

# 3. Round-trip a string JS -> Nim.
echo "back in Nim: " & toJs("round-trip").toStr

# 4. Build a JS object, set + read fields back, and JSON.stringify it via host.
let o = newJsObject()
o.set("name", toJs("nimony"))
o.set("year", toJs(2026))
echo "o.year via get = " & $toInt(o.get("year"))
echo global("JSON").call("stringify", o).toStr

# 5. bool round-trip, nil for a missing property, and `==` meaning JS `===`.
echo "bool round-trip: " & $toBool(toJs(true)) & " " & $toBool(toJs(false))
echo "missing prop isNil: " & $o.get("nope").isNil
echo "self === self: " & $(m == global("Math"))

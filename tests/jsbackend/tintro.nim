## JS-value introspection (typeof / instanceof / property presence) and a
## variadic method call (`apply`) — the value queries a DOM binding leans on
## (branch on a node's kind, test a property, call a method with N args).
import std/syncio
import jsffi

# typeof across value kinds
echo "typeof num: ", jsTypeof(toJs(42))
echo "typeof str: ", jsTypeof(toJs("hi"))
echo "typeof obj: ", jsTypeof(newJsObject())
echo "typeof arr: ", jsTypeof(newJsArray())

# instanceof against a global constructor
let a = newJsArray()
echo "arr instanceof Array: ", instanceOf(a, "Array")
echo "obj instanceof Array: ", instanceOf(newJsObject(), "Array")

# property presence
let o = newJsObject()
o.set("x", toJs(1))
echo "has x: ", hasProp(o, "x")
echo "has y: ", hasProp(o, "y")

# apply: method call with more args than call0..3 supports
let m = global("Math")
echo "max apply: ", toInt(m.apply("max", [toJs(3), toJs(9), toJs(4), toJs(7), toJs(1)]))

## Exercises `domtokenlist.nim` — a binding GENERATED from dom.idl covering the
## constructs URL didn't: a variadic operation (`add`/`remove` -> openArray +
## applyArgs) and an optional argument (`toggle(token, force)` -> two overloads).
## element.classList is a DOMTokenList, so this runs against jsdom (tclasslist.env.js).
import std/syncio
import jsffi
import dom
import domtokenlist

let doc = document()
let el = doc.createElement("div")
let cl = el.get("classList")          # a DOMTokenList (JsValue)

cl.add(["alpha", "beta"])             # variadic -> applyArgs
echo "length: ", cl.length
echo "contains alpha: ", cl.contains("alpha")
echo "contains gamma: ", cl.contains("gamma")
echo "item0: ", cl.item(0)
echo "item1: ", cl.item(1)

discard cl.toggle("beta")             # optional-arg overload (1 arg): removes beta
echo "value after toggle beta: ", cl.value
echo "toggle force gamma: ", cl.toggle("gamma", true)  # optional-arg overload (2 args)
echo "value: ", cl.value

discard cl.replace("alpha", "ALPHA")
echo "final value: ", cl.value

cl.remove(["gamma"])                  # variadic remove
echo "after remove gamma: ", cl.value

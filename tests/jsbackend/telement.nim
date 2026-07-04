## Exercises `element.nim` — a binding GENERATED from dom.idl by gen/idl2nim.js
## with FLATTENED inheritance and MERGED mixins. A single generated `Element`
## therefore carries members from four sources at once:
##   - own:                setAttribute / getAttribute / id= / tagName
##   - Node (inherited):   appendChild / textContent
##   - EventTarget (inh.): addEventListener / dispatchEvent
##   - ParentNode (mixin): append / querySelector / children / childElementCount
##   - ChildNode (mixin):  remove
##   - NonDocumentTypeChildNode (mixin): previousElementSibling
## element.classList is/needs a real DOM, so this runs against jsdom (telement.env.js).
import std/syncio
import jsffi
import element

var pings = 0
proc onPing(ev: JsValue) {.nimcall.} =
  inc pings
  echo "  handler fired: ", $ev.get("type"), " #", pings

let doc = global("document")

# Build  <div id="root"><span>alpha</span><span>beta</span></div>
let root = doc.call("createElement", toJs("div"))
root.id = "root"                                   # own attr setter
echo "tagName: ", root.tagName                     # own attr (upper-cased by DOM)

let a = doc.call("createElement", toJs("span"))
a.textContent = "alpha"                            # Node attr setter (inherited)
let b = doc.call("createElement", toJs("span"))
b.textContent = "beta"
root.append([a, b])                                # ParentNode variadic (mixin)

echo "childElementCount: ", root.childElementCount             # ParentNode attr
echo "firstElementChild: ", root.firstElementChild.textContent # ParentNode attr -> Node attr
echo "querySelector span: ", root.querySelector("span").textContent   # ParentNode op
echo "b.previousElementSibling: ", b.previousElementSibling.textContent  # NonDocumentTypeChildNode

root.setAttribute("data-x", toJs("42"))            # own op
echo "getAttribute data-x: ", root.getAttribute("data-x")
echo "hasAttribute data-y: ", root.hasAttribute("data-y")

# EventTarget (inherited): Nim proc as a JS event handler, fired twice.
root.addEventListener("ping", toJs(onPing))
discard root.dispatchEvent(newOf("Event", [toJs("ping")]))
discard root.dispatchEvent(newOf("Event", [toJs("ping")]))
echo "pings: ", pings

# ChildNode (mixin): detach the first span.
a.remove()
echo "after remove childElementCount: ", root.childElementCount
echo "firstElementChild now: ", root.firstElementChild.textContent

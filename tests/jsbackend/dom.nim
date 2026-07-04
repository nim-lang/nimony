## A minimal DOM binding slice on top of `jsffi` — the reference shape a
## WebIDL/MDN-driven generator will eventually emit automatically.
##
## Every DOM object is a `JsValue` (a handle into the runtime's value table),
## and each interface is a thin, typed facade over the `jsffi` primitives
## (`global`/`get`/`set`/`call`/`newOf`). Nothing here is JS-backend-specific
## beyond `jsffi`: it is ordinary Nim that marshals to and from real host DOM
## objects, so the same source describes both the hand-written layer and the
## generated one.
import jsffi

type
  ## Distinct names for readability; all are `JsValue` underneath, so any node
  ## flows anywhere a `JsValue` is expected (matching the DOM's loose typing).
  Node* = JsValue
  Element* = JsValue
  Document* = JsValue
  Event* = JsValue

proc document*(): Document =
  ## The global `document`.
  global("document")

# ── creation ─────────────────────────────────────────────────────────────────
proc createElement*(doc: Document; tag: string): Element =
  ## `document.createElement(tag)`.
  doc.call("createElement", toJs(tag))

proc createTextNode*(doc: Document; text: string): Node =
  ## `document.createTextNode(text)`.
  doc.call("createTextNode", toJs(text))

# ── tree ─────────────────────────────────────────────────────────────────────
proc appendChild*(parent: Node; child: Node): Node =
  ## `parent.appendChild(child)` — returns the appended child.
  parent.call("appendChild", child)

proc removeChild*(parent: Node; child: Node): Node =
  ## `parent.removeChild(child)`.
  parent.call("removeChild", child)

proc body*(doc: Document): Element =
  ## `document.body`.
  doc.get("body")

# ── queries ──────────────────────────────────────────────────────────────────
proc getElementById*(doc: Document; id: string): Element =
  ## `document.getElementById(id)`.
  doc.call("getElementById", toJs(id))

proc querySelector*(node: Node; sel: string): Element =
  ## `node.querySelector(sel)`.
  node.call("querySelector", toJs(sel))

proc childCount*(node: Node): int =
  ## `node.childNodes.length`.
  node.get("childNodes").get("length").toInt

# ── attributes & content ─────────────────────────────────────────────────────
proc setAttribute*(el: Element; name, value: string) =
  ## `el.setAttribute(name, value)`.
  discard el.call("setAttribute", toJs(name), toJs(value))

proc getAttribute*(el: Element; name: string): string =
  ## `el.getAttribute(name)`.
  $el.call("getAttribute", toJs(name))

proc textContent*(node: Node): string =
  ## `node.textContent`.
  $node.get("textContent")

proc `textContent=`*(node: Node; text: string) =
  node.set("textContent", toJs(text))

proc innerHTML*(el: Element): string =
  ## `el.innerHTML`.
  $el.get("innerHTML")

proc `innerHTML=`*(el: Element; html: string) =
  el.set("innerHTML", toJs(html))

proc id*(el: Element): string =
  ## `el.id`.
  $el.get("id")

proc `id=`*(el: Element; value: string) =
  el.set("id", toJs(value))

proc tagName*(el: Element): string =
  ## `el.tagName` (upper-case, per the DOM).
  $el.get("tagName")

# ── events ───────────────────────────────────────────────────────────────────
proc addEventListener*(target: Node; event: string; handler: JsProc1) =
  ## `target.addEventListener(event, handler)` — `handler` is a Nim proc that
  ## receives the event as a `JsValue`, wired in through `jsffi`'s callback path.
  discard target.call("addEventListener", toJs(event), toJs(handler))

proc dispatchEvent*(target: Node; event: Event): bool =
  ## `target.dispatchEvent(event)`.
  target.call("dispatchEvent", event).toBool

proc newEvent*(kind: string): Event =
  ## `new Event(kind)`.
  newOf("Event", toJs(kind))

proc target*(ev: Event): Node =
  ## `event.target` — the node the event fired on.
  ev.get("target")

## GENERATED from @webref/idl (dom.idl) by gen/idl2nim.js — do not edit by hand.
## Regenerate: node gen/idl2nim.js dom Element element.nim
##
## A jsffi binding for the WHATWG/W3C `Element` interface. Each member
## marshals through jsffi exactly as the hand-written dom.nim does.
import jsffi

type
  Element* = JsValue
    ## Flattened inheritance (members merged in): Node, EventTarget.
    ## Merged mixins (includes): ParentNode, NonDocumentTypeChildNode, ChildNode, Slottable.

proc namespaceURI*(self: Element): string = $self.get("namespaceURI")

proc prefix*(self: Element): string = $self.get("prefix")

proc localName*(self: Element): string = $self.get("localName")

proc tagName*(self: Element): string = $self.get("tagName")

proc id*(self: Element): string = $self.get("id")
proc `id=`*(self: Element; value: string) = self.set("id", toJs(value))

proc className*(self: Element): string = $self.get("className")
proc `className=`*(self: Element; value: string) = self.set("className", toJs(value))

proc classList*(self: Element): JsValue = self.get("classList")

proc slot*(self: Element): string = $self.get("slot")
proc `slot=`*(self: Element; value: string) = self.set("slot", toJs(value))

proc hasAttributes*(self: Element): bool = self.call("hasAttributes").toBool

proc attributes*(self: Element): JsValue = self.get("attributes")

proc getAttributeNames*(self: Element): JsValue = self.call("getAttributeNames")

proc getAttribute*(self: Element; qualifiedName: string): string = $self.call("getAttribute", toJs(qualifiedName))

proc getAttributeNS*(self: Element; namespace: string; localName: string): string = $self.call("getAttributeNS", toJs(namespace), toJs(localName))

proc setAttribute*(self: Element; qualifiedName: string; value: JsValue) = discard self.call("setAttribute", toJs(qualifiedName), value)

proc setAttributeNS*(self: Element; namespace: string; qualifiedName: string; value: JsValue) = discard self.call("setAttributeNS", toJs(namespace), toJs(qualifiedName), value)

proc removeAttribute*(self: Element; qualifiedName: string) = discard self.call("removeAttribute", toJs(qualifiedName))

proc removeAttributeNS*(self: Element; namespace: string; localName: string) = discard self.call("removeAttributeNS", toJs(namespace), toJs(localName))

proc toggleAttribute*(self: Element; qualifiedName: string): bool = self.call("toggleAttribute", toJs(qualifiedName)).toBool
proc toggleAttribute*(self: Element; qualifiedName: string; force: bool): bool = self.call("toggleAttribute", toJs(qualifiedName), toJs(force)).toBool

proc hasAttribute*(self: Element; qualifiedName: string): bool = self.call("hasAttribute", toJs(qualifiedName)).toBool

proc hasAttributeNS*(self: Element; namespace: string; localName: string): bool = self.call("hasAttributeNS", toJs(namespace), toJs(localName)).toBool

proc getAttributeNode*(self: Element; qualifiedName: string): JsValue = self.call("getAttributeNode", toJs(qualifiedName))

proc getAttributeNodeNS*(self: Element; namespace: string; localName: string): JsValue = self.call("getAttributeNodeNS", toJs(namespace), toJs(localName))

proc setAttributeNode*(self: Element; attr: JsValue): JsValue = self.call("setAttributeNode", attr)

proc setAttributeNodeNS*(self: Element; attr: JsValue): JsValue = self.call("setAttributeNodeNS", attr)

proc removeAttributeNode*(self: Element; attr: JsValue): JsValue = self.call("removeAttributeNode", attr)

proc attachShadow*(self: Element; init: JsValue): JsValue = self.call("attachShadow", init)

proc shadowRoot*(self: Element): JsValue = self.get("shadowRoot")

proc customElementRegistry*(self: Element): JsValue = self.get("customElementRegistry")

proc closest*(self: Element; selectors: string): JsValue = self.call("closest", toJs(selectors))

proc matches*(self: Element; selectors: string): bool = self.call("matches", toJs(selectors)).toBool

proc webkitMatchesSelector*(self: Element; selectors: string): bool = self.call("webkitMatchesSelector", toJs(selectors)).toBool

proc getElementsByTagName*(self: Element; qualifiedName: string): JsValue = self.call("getElementsByTagName", toJs(qualifiedName))

proc getElementsByTagNameNS*(self: Element; namespace: string; localName: string): JsValue = self.call("getElementsByTagNameNS", toJs(namespace), toJs(localName))

proc getElementsByClassName*(self: Element; classNames: string): JsValue = self.call("getElementsByClassName", toJs(classNames))

proc insertAdjacentElement*(self: Element; where: string; element: JsValue): JsValue = self.call("insertAdjacentElement", toJs(where), element)

proc insertAdjacentText*(self: Element; where: string; data: string) = discard self.call("insertAdjacentText", toJs(where), toJs(data))

proc children*(self: Element): JsValue = self.get("children")

proc firstElementChild*(self: Element): JsValue = self.get("firstElementChild")

proc lastElementChild*(self: Element): JsValue = self.get("lastElementChild")

proc childElementCount*(self: Element): int = self.get("childElementCount").toInt

proc prepend*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("prepend", a)

proc append*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("append", a)

proc replaceChildren*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("replaceChildren", a)

proc moveBefore*(self: Element; node: JsValue; child: JsValue) = discard self.call("moveBefore", node, child)

proc querySelector*(self: Element; selectors: string): JsValue = self.call("querySelector", toJs(selectors))

proc querySelectorAll*(self: Element; selectors: string): JsValue = self.call("querySelectorAll", toJs(selectors))

proc previousElementSibling*(self: Element): JsValue = self.get("previousElementSibling")

proc nextElementSibling*(self: Element): JsValue = self.get("nextElementSibling")

proc before*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("before", a)

proc after*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("after", a)

proc replaceWith*(self: Element; nodes: openArray[JsValue]) =
  let a = newJsArray()
  for x in nodes: a.add(x)
  discard self.applyArgs("replaceWith", a)

proc remove*(self: Element) = discard self.call("remove")

proc assignedSlot*(self: Element): JsValue = self.get("assignedSlot")

proc nodeType*(self: Element): int = self.get("nodeType").toInt

proc nodeName*(self: Element): string = $self.get("nodeName")

proc baseURI*(self: Element): string = $self.get("baseURI")

proc isConnected*(self: Element): bool = self.get("isConnected").toBool

proc ownerDocument*(self: Element): JsValue = self.get("ownerDocument")

proc getRootNode*(self: Element): JsValue = self.call("getRootNode")
proc getRootNode*(self: Element; options: JsValue): JsValue = self.call("getRootNode", options)

proc parentNode*(self: Element): JsValue = self.get("parentNode")

proc parentElement*(self: Element): JsValue = self.get("parentElement")

proc hasChildNodes*(self: Element): bool = self.call("hasChildNodes").toBool

proc childNodes*(self: Element): JsValue = self.get("childNodes")

proc firstChild*(self: Element): JsValue = self.get("firstChild")

proc lastChild*(self: Element): JsValue = self.get("lastChild")

proc previousSibling*(self: Element): JsValue = self.get("previousSibling")

proc nextSibling*(self: Element): JsValue = self.get("nextSibling")

proc nodeValue*(self: Element): string = $self.get("nodeValue")
proc `nodeValue=`*(self: Element; value: string) = self.set("nodeValue", toJs(value))

proc textContent*(self: Element): string = $self.get("textContent")
proc `textContent=`*(self: Element; value: string) = self.set("textContent", toJs(value))

proc normalize*(self: Element) = discard self.call("normalize")

proc cloneNode*(self: Element): JsValue = self.call("cloneNode")
proc cloneNode*(self: Element; subtree: bool): JsValue = self.call("cloneNode", toJs(subtree))

proc isEqualNode*(self: Element; otherNode: JsValue): bool = self.call("isEqualNode", otherNode).toBool

proc isSameNode*(self: Element; otherNode: JsValue): bool = self.call("isSameNode", otherNode).toBool

proc compareDocumentPosition*(self: Element; other: JsValue): int = self.call("compareDocumentPosition", other).toInt

proc contains*(self: Element; other: JsValue): bool = self.call("contains", other).toBool

proc lookupPrefix*(self: Element; namespace: string): string = $self.call("lookupPrefix", toJs(namespace))

proc lookupNamespaceURI*(self: Element; prefix: string): string = $self.call("lookupNamespaceURI", toJs(prefix))

proc isDefaultNamespace*(self: Element; namespace: string): bool = self.call("isDefaultNamespace", toJs(namespace)).toBool

proc insertBefore*(self: Element; node: JsValue; child: JsValue): JsValue = self.call("insertBefore", node, child)

proc appendChild*(self: Element; node: JsValue): JsValue = self.call("appendChild", node)

proc replaceChild*(self: Element; node: JsValue; child: JsValue): JsValue = self.call("replaceChild", node, child)

proc removeChild*(self: Element; child: JsValue): JsValue = self.call("removeChild", child)

proc addEventListener*(self: Element; `type`: string; callback: JsValue) = discard self.call("addEventListener", toJs(`type`), callback)
proc addEventListener*(self: Element; `type`: string; callback: JsValue; options: JsValue) = discard self.call("addEventListener", toJs(`type`), callback, options)

proc removeEventListener*(self: Element; `type`: string; callback: JsValue) = discard self.call("removeEventListener", toJs(`type`), callback)
proc removeEventListener*(self: Element; `type`: string; callback: JsValue; options: JsValue) = discard self.call("removeEventListener", toJs(`type`), callback, options)

proc dispatchEvent*(self: Element; event: JsValue): bool = self.call("dispatchEvent", event).toBool

## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member
##   - const member

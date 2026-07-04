## GENERATED from @webref/idl (dom.idl) by gen/idl2nim.js — do not edit by hand.
## Regenerate: node gen/idl2nim.js dom Node domlib.nim Element Document DocumentFragment
##
## jsffi bindings for the WHATWG/W3C Node, Element, Document, DocumentFragment interfaces.
## Each interface is a JsValue alias; interface-typed members are typed by name
## so DOM-tree navigation reads (and, once distinct, checks) like a real DOM API.
import jsffi

type
  Node* = JsValue
  Element* = JsValue
  Document* = JsValue
  DocumentFragment* = JsValue
  Attr* = JsValue
  CDATASection* = JsValue
  Comment* = JsValue
  DOMImplementation* = JsValue
  DOMTokenList* = JsValue
  DocumentType* = JsValue
  Event* = JsValue
  EventListener* = JsValue
  GetRootNodeOptions* = JsValue
  HTMLCollection* = JsValue
  NamedNodeMap* = JsValue
  NodeFilter* = JsValue
  NodeIterator* = JsValue
  NodeList* = JsValue
  ProcessingInstruction* = JsValue
  Range* = JsValue
  ShadowRoot* = JsValue
  ShadowRootInit* = JsValue
  Text* = JsValue
  TreeWalker* = JsValue
  XPathExpression* = JsValue
  XPathNSResolver* = JsValue
  XPathResult* = JsValue

# ── Node ─────────────────────────────────────────────────────────────
#   inherits: EventTarget

proc nodeType*(self: Node): int = self.get("nodeType").toInt

proc nodeName*(self: Node): string = $self.get("nodeName")

proc baseURI*(self: Node): string = $self.get("baseURI")

proc isConnected*(self: Node): bool = self.get("isConnected").toBool

proc ownerDocument*(self: Node): Document = self.get("ownerDocument")

proc getRootNode*(self: Node): Node = self.call("getRootNode")
proc getRootNode*(self: Node; options: GetRootNodeOptions): Node = self.call("getRootNode", options)

proc parentNode*(self: Node): Node = self.get("parentNode")

proc parentElement*(self: Node): Element = self.get("parentElement")

proc hasChildNodes*(self: Node): bool = self.call("hasChildNodes").toBool

proc childNodes*(self: Node): NodeList = self.get("childNodes")

proc firstChild*(self: Node): Node = self.get("firstChild")

proc lastChild*(self: Node): Node = self.get("lastChild")

proc previousSibling*(self: Node): Node = self.get("previousSibling")

proc nextSibling*(self: Node): Node = self.get("nextSibling")

proc nodeValue*(self: Node): string = $self.get("nodeValue")
proc `nodeValue=`*(self: Node; value: string) = self.set("nodeValue", toJs(value))

proc textContent*(self: Node): string = $self.get("textContent")
proc `textContent=`*(self: Node; value: string) = self.set("textContent", toJs(value))

proc normalize*(self: Node) = discard self.call("normalize")

proc cloneNode*(self: Node): Node = self.call("cloneNode")
proc cloneNode*(self: Node; subtree: bool): Node = self.call("cloneNode", toJs(subtree))

proc isEqualNode*(self: Node; otherNode: Node): bool = self.call("isEqualNode", otherNode).toBool

proc isSameNode*(self: Node; otherNode: Node): bool = self.call("isSameNode", otherNode).toBool

proc compareDocumentPosition*(self: Node; other: Node): int = self.call("compareDocumentPosition", other).toInt

proc contains*(self: Node; other: Node): bool = self.call("contains", other).toBool

proc lookupPrefix*(self: Node; namespace: string): string = $self.call("lookupPrefix", toJs(namespace))

proc lookupNamespaceURI*(self: Node; prefix: string): string = $self.call("lookupNamespaceURI", toJs(prefix))

proc isDefaultNamespace*(self: Node; namespace: string): bool = self.call("isDefaultNamespace", toJs(namespace)).toBool

proc insertBefore*(self: Node; node: Node; child: Node): Node = self.call("insertBefore", node, child)

proc appendChild*(self: Node; node: Node): Node = self.call("appendChild", node)

proc replaceChild*(self: Node; node: Node; child: Node): Node = self.call("replaceChild", node, child)

proc removeChild*(self: Node; child: Node): Node = self.call("removeChild", child)

proc addEventListener*(self: Node; `type`: string; callback: EventListener) = discard self.call("addEventListener", toJs(`type`), callback)
proc addEventListener*(self: Node; `type`: string; callback: EventListener; options: JsValue) = discard self.call("addEventListener", toJs(`type`), callback, options)

proc removeEventListener*(self: Node; `type`: string; callback: EventListener) = discard self.call("removeEventListener", toJs(`type`), callback)
proc removeEventListener*(self: Node; `type`: string; callback: EventListener; options: JsValue) = discard self.call("removeEventListener", toJs(`type`), callback, options)

proc dispatchEvent*(self: Node; event: Event): bool = self.call("dispatchEvent", event).toBool

# ── Element ─────────────────────────────────────────────────────────────
#   inherits: Node, EventTarget
#   mixins:   ParentNode, NonDocumentTypeChildNode, ChildNode, Slottable

proc namespaceURI*(self: Element): string = $self.get("namespaceURI")

proc prefix*(self: Element): string = $self.get("prefix")

proc localName*(self: Element): string = $self.get("localName")

proc tagName*(self: Element): string = $self.get("tagName")

proc id*(self: Element): string = $self.get("id")
proc `id=`*(self: Element; value: string) = self.set("id", toJs(value))

proc className*(self: Element): string = $self.get("className")
proc `className=`*(self: Element; value: string) = self.set("className", toJs(value))

proc classList*(self: Element): DOMTokenList = self.get("classList")

proc slot*(self: Element): string = $self.get("slot")
proc `slot=`*(self: Element; value: string) = self.set("slot", toJs(value))

proc hasAttributes*(self: Element): bool = self.call("hasAttributes").toBool

proc attributes*(self: Element): NamedNodeMap = self.get("attributes")

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

proc getAttributeNode*(self: Element; qualifiedName: string): Attr = self.call("getAttributeNode", toJs(qualifiedName))

proc getAttributeNodeNS*(self: Element; namespace: string; localName: string): Attr = self.call("getAttributeNodeNS", toJs(namespace), toJs(localName))

proc setAttributeNode*(self: Element; attr: Attr): Attr = self.call("setAttributeNode", attr)

proc setAttributeNodeNS*(self: Element; attr: Attr): Attr = self.call("setAttributeNodeNS", attr)

proc removeAttributeNode*(self: Element; attr: Attr): Attr = self.call("removeAttributeNode", attr)

proc attachShadow*(self: Element; init: ShadowRootInit): ShadowRoot = self.call("attachShadow", init)

proc shadowRoot*(self: Element): ShadowRoot = self.get("shadowRoot")

proc customElementRegistry*(self: Element): JsValue = self.get("customElementRegistry")

proc closest*(self: Element; selectors: string): Element = self.call("closest", toJs(selectors))

proc matches*(self: Element; selectors: string): bool = self.call("matches", toJs(selectors)).toBool

proc webkitMatchesSelector*(self: Element; selectors: string): bool = self.call("webkitMatchesSelector", toJs(selectors)).toBool

proc getElementsByTagName*(self: Element; qualifiedName: string): HTMLCollection = self.call("getElementsByTagName", toJs(qualifiedName))

proc getElementsByTagNameNS*(self: Element; namespace: string; localName: string): HTMLCollection = self.call("getElementsByTagNameNS", toJs(namespace), toJs(localName))

proc getElementsByClassName*(self: Element; classNames: string): HTMLCollection = self.call("getElementsByClassName", toJs(classNames))

proc insertAdjacentElement*(self: Element; where: string; element: Element): Element = self.call("insertAdjacentElement", toJs(where), element)

proc insertAdjacentText*(self: Element; where: string; data: string) = discard self.call("insertAdjacentText", toJs(where), toJs(data))

proc children*(self: Element): HTMLCollection = self.get("children")

proc firstElementChild*(self: Element): Element = self.get("firstElementChild")

proc lastElementChild*(self: Element): Element = self.get("lastElementChild")

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

proc moveBefore*(self: Element; node: Node; child: Node) = discard self.call("moveBefore", node, child)

proc querySelector*(self: Element; selectors: string): Element = self.call("querySelector", toJs(selectors))

proc querySelectorAll*(self: Element; selectors: string): NodeList = self.call("querySelectorAll", toJs(selectors))

proc previousElementSibling*(self: Element): Element = self.get("previousElementSibling")

proc nextElementSibling*(self: Element): Element = self.get("nextElementSibling")

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

































# ── Document ─────────────────────────────────────────────────────────────
#   inherits: Node, EventTarget
#   mixins:   NonElementParentNode, DocumentOrShadowRoot, ParentNode, XPathEvaluatorBase

proc newDocument*(): Document =
  newOf("Document")

proc implementation*(self: Document): DOMImplementation = self.get("implementation")

proc URL*(self: Document): string = $self.get("URL")

proc documentURI*(self: Document): string = $self.get("documentURI")

proc compatMode*(self: Document): string = $self.get("compatMode")

proc characterSet*(self: Document): string = $self.get("characterSet")

proc charset*(self: Document): string = $self.get("charset")

proc inputEncoding*(self: Document): string = $self.get("inputEncoding")

proc contentType*(self: Document): string = $self.get("contentType")

proc doctype*(self: Document): DocumentType = self.get("doctype")

proc documentElement*(self: Document): Element = self.get("documentElement")




proc createElement*(self: Document; localName: string): Element = self.call("createElement", toJs(localName))
proc createElement*(self: Document; localName: string; options: JsValue): Element = self.call("createElement", toJs(localName), options)

proc createElementNS*(self: Document; namespace: string; qualifiedName: string): Element = self.call("createElementNS", toJs(namespace), toJs(qualifiedName))
proc createElementNS*(self: Document; namespace: string; qualifiedName: string; options: JsValue): Element = self.call("createElementNS", toJs(namespace), toJs(qualifiedName), options)

proc createDocumentFragment*(self: Document): DocumentFragment = self.call("createDocumentFragment")

proc createTextNode*(self: Document; data: string): Text = self.call("createTextNode", toJs(data))

proc createCDATASection*(self: Document; data: string): CDATASection = self.call("createCDATASection", toJs(data))

proc createComment*(self: Document; data: string): Comment = self.call("createComment", toJs(data))

proc createProcessingInstruction*(self: Document; target: string; data: string): ProcessingInstruction = self.call("createProcessingInstruction", toJs(target), toJs(data))

proc importNode*(self: Document; node: Node): Node = self.call("importNode", node)
proc importNode*(self: Document; node: Node; options: JsValue): Node = self.call("importNode", node, options)

proc adoptNode*(self: Document; node: Node): Node = self.call("adoptNode", node)

proc createAttribute*(self: Document; localName: string): Attr = self.call("createAttribute", toJs(localName))

proc createAttributeNS*(self: Document; namespace: string; qualifiedName: string): Attr = self.call("createAttributeNS", toJs(namespace), toJs(qualifiedName))

proc createEvent*(self: Document; `interface`: string): Event = self.call("createEvent", toJs(`interface`))

proc createRange*(self: Document): Range = self.call("createRange")

proc createNodeIterator*(self: Document; root: Node): NodeIterator = self.call("createNodeIterator", root)
proc createNodeIterator*(self: Document; root: Node; whatToShow: int): NodeIterator = self.call("createNodeIterator", root, toJs(whatToShow))
proc createNodeIterator*(self: Document; root: Node; whatToShow: int; filter: NodeFilter): NodeIterator = self.call("createNodeIterator", root, toJs(whatToShow), filter)

proc createTreeWalker*(self: Document; root: Node): TreeWalker = self.call("createTreeWalker", root)
proc createTreeWalker*(self: Document; root: Node; whatToShow: int): TreeWalker = self.call("createTreeWalker", root, toJs(whatToShow))
proc createTreeWalker*(self: Document; root: Node; whatToShow: int; filter: NodeFilter): TreeWalker = self.call("createTreeWalker", root, toJs(whatToShow), filter)

proc getElementById*(self: Document; elementId: string): Element = self.call("getElementById", toJs(elementId))









proc createExpression*(self: Document; expression: string): XPathExpression = self.call("createExpression", toJs(expression))
proc createExpression*(self: Document; expression: string; resolver: XPathNSResolver): XPathExpression = self.call("createExpression", toJs(expression), resolver)

proc createNSResolver*(self: Document; nodeResolver: Node): Node = self.call("createNSResolver", nodeResolver)

proc evaluate*(self: Document; expression: string; contextNode: Node): XPathResult = self.call("evaluate", toJs(expression), contextNode)
proc evaluate*(self: Document; expression: string; contextNode: Node; resolver: XPathNSResolver): XPathResult = self.call("evaluate", toJs(expression), contextNode, resolver)

































# ── DocumentFragment ─────────────────────────────────────────────────────────────
#   inherits: Node, EventTarget
#   mixins:   NonElementParentNode, ParentNode

proc newDocumentFragment*(): DocumentFragment =
  newOf("DocumentFragment")









































## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Node const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Element const member
##   - Document.evaluate arity 4 (>3 fixed args)
##   - Document.evaluate arity 5 (>3 fixed args)
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - Document const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member
##   - DocumentFragment const member

## GENERATED from @webref/idl (dom.idl) by gen/idl2nim.js — do not edit by hand.
## Regenerate: node gen/idl2nim.js dom DOMTokenList domtokenlist.nim
##
## jsffi bindings for the WHATWG/W3C DOMTokenList interface.
## Each interface is a JsValue alias; interface-typed members are typed by name
## so DOM-tree navigation reads (and, once distinct, checks) like a real DOM API.
import jsffi

type
  DOMTokenList* = JsValue

# ── DOMTokenList ─────────────────────────────────────────────────────────────

proc length*(self: DOMTokenList): int = self.get("length").toInt

proc item*(self: DOMTokenList; index: int): string = $self.call("item", toJs(index))

proc contains*(self: DOMTokenList; token: string): bool = self.call("contains", toJs(token)).toBool

proc add*(self: DOMTokenList; tokens: openArray[string]) =
  let a = newJsArray()
  for x in tokens: a.add(toJs(x))
  discard self.applyArgs("add", a)

proc remove*(self: DOMTokenList; tokens: openArray[string]) =
  let a = newJsArray()
  for x in tokens: a.add(toJs(x))
  discard self.applyArgs("remove", a)

proc toggle*(self: DOMTokenList; token: string): bool = self.call("toggle", toJs(token)).toBool
proc toggle*(self: DOMTokenList; token: string; force: bool): bool = self.call("toggle", toJs(token), toJs(force)).toBool

proc replace*(self: DOMTokenList; token: string; newToken: string): bool = self.call("replace", toJs(token), toJs(newToken)).toBool

proc supports*(self: DOMTokenList; token: string): bool = self.call("supports", toJs(token)).toBool

proc value*(self: DOMTokenList): string = $self.get("value")
proc `value=`*(self: DOMTokenList; value: string) = self.set("value", toJs(value))

## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):
##   - DOMTokenList iterable member

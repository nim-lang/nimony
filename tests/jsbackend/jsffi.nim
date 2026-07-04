## Minimal JS-value interop for the Leng JS backend (prototype).
##
## The JS backend lays native Nim data out as byte offsets into one linear
## `ArrayBuffer`; a genuine JS value (string, object, function, DOM node) can't
## live there. `JsValue` is an opaque **handle** into a runtime-side value table
## (`runtime.js`), the generalisation of the proc-pointer `_fns` table the
## backend already uses. This module wraps the `importc` seam — an `importc`
## proc with no body lowers to a plain call of the named runtime function — into
## an ergonomic surface: marshal Nim `string`/`int`/`bool` to and from JS, look
## up globals, read/set properties, call methods, construct with `new`, and pass
## Nim procs as JS callbacks.
##
## **Ownership.** `JsValue` is GC-integrated: it owns its table slot. `=destroy`
## releases the slot, and copying a handle (`=copy`/`=dup`) allocates a *new*
## slot referring to the same JS value, so every copy is independently owned and
## there is no double free. Transient values (a method result you don't keep, a
## member-name handle) are reclaimed automatically at scope exit — no manual
## release. The FFI seam itself is kept on plain `int32` handles so no owning
## `JsValue` ever crosses into an `importc` (which would silently duplicate it).
##
## This is the foundation a DOM binding sits on: `document`, `Element`, event
## handlers, etc. are all `JsValue`s reached through `global`/`get`/`call`.

type
  JsValue* = object
    ## Opaque, owning handle to a JS value held in the runtime's side table.
    ## The wrapper carries the ARC hooks; only the raw `h` int crosses the FFI.
    h: int32

# ── raw seam (int32 handles → runtime.js functions; NO owning JsValue here) ───
proc rawRelease(h: int32) {.importc: "_jsRelease".}
proc rawDup(h: int32): int32 {.importc: "_jsvDup".}
proc rawStrictEq(a, b: int32): bool {.importc: "_jsStrictEq".}
proc rawLive(): int {.importc: "_jsvLive".}

proc rawStrToJs(p: cstring; n: int): int32 {.importc: "_strToJs".}
proc rawStrLen(h: int32): int {.importc: "_jsStrLen".}
proc rawStrInto(h: int32; dst: cstring) {.importc: "_jsStrInto".}

proc rawNumToJs(x: int): int32 {.importc: "_numToJs".}
proc rawToNum(h: int32): int {.importc: "_jsToNum".}
proc rawFloatToJs(x: float): int32 {.importc: "_numToJs".}   # a Nim float is already a JS Number
proc rawToFloat(h: int32): float {.importc: "_jsToNum".}
proc rawBoolToJs(b: bool): int32 {.importc: "_boolToJs".}
proc rawToBool(h: int32): bool {.importc: "_jsToBool".}

proc rawNewArray(): int32 {.importc: "_jsNewArray".}
proc rawArrLen(h: int32): int {.importc: "_jsArrLen".}
proc rawArrPush(h, v: int32) {.importc: "_jsArrPush".}
proc rawArrGet(h: int32; i: int): int32 {.importc: "_jsArrGet".}
proc rawArrSet(h: int32; i: int; v: int32) {.importc: "_jsArrSet".}

proc rawTypeof(h: int32): int32 {.importc: "_jsTypeof".}
proc rawHasProp(obj, name: int32): bool {.importc: "_jsHasProp".}
proc rawInstanceOf(v, ctor: int32): bool {.importc: "_jsInstanceOf".}
proc rawApply(obj, name, args: int32): int32 {.importc: "_jsApply".}

proc rawGlobalH(nameH: int32): int32 {.importc: "_jsGlobalH".}
proc rawGetProp(obj, name: int32): int32 {.importc: "_jsGetProp".}
proc rawSetProp(obj, name, val: int32) {.importc: "_jsSetProp".}
proc rawCall0(obj, name: int32): int32 {.importc: "_jsCall0".}
proc rawCall1(obj, name, a: int32): int32 {.importc: "_jsCall1".}
proc rawCall2(obj, name, a, b: int32): int32 {.importc: "_jsCall2".}
proc rawCall3(obj, name, a, b, c: int32): int32 {.importc: "_jsCall3".}
proc rawNewObject(): int32 {.importc: "_jsNewObject".}
proc rawCtor0(ctor: int32): int32 {.importc: "_jsCtor0".}
proc rawCtor1(ctor, a: int32): int32 {.importc: "_jsCtor1".}

# ── ARC hooks: JsValue owns its slot ─────────────────────────────────────────
proc `=destroy`(x: JsValue) =
  rawRelease(x.h)                      # runtime guards handle 0 (nil) → no-op
proc `=dup`(x: JsValue): JsValue {.nodestroy.} =
  JsValue(h: rawDup(x.h))              # a copy is a new slot to the same value
proc `=copy`(dest: var JsValue; src: JsValue) {.nodestroy.} =
  if dest.h != src.h:
    rawRelease(dest.h)
    dest = JsValue(h: rawDup(src.h))
# =wasMoved/=sink use the compiler defaults: a moved-from JsValue becomes 0 (nil),
# whose =destroy is a no-op — exactly right.

const undefined* = JsValue(h: 0)

proc `==`*(a, b: JsValue): bool {.inline.} = rawStrictEq(a.h, b.h)
  ## JS `===`: two distinct handles to the same value compare equal.
proc isNil*(v: JsValue): bool {.inline.} = v.h == 0
  ## True for the nil handle — both JS `undefined` and `null` marshal to it.

proc liveHandles*(): int {.inline.} = rawLive()
  ## Number of live (un-freed) slots in the runtime value table — for leak tests.

# ── marshalling ──────────────────────────────────────────────────────────────
proc toJs*(x: int): JsValue {.inline.} = JsValue(h: rawNumToJs(x))
  ## A Nim `int` as a JS Number.
proc toInt*(v: JsValue): int {.inline.} = rawToNum(v.h)
  ## A JS Number as a Nim `int` (unchecked).
proc toJs*(x: float): JsValue {.inline.} = JsValue(h: rawFloatToJs(x))
  ## A Nim `float` as a JS Number.
proc toFloat*(v: JsValue): float {.inline.} = rawToFloat(v.h)
  ## A JS Number as a Nim `float`.
proc toJs*(b: bool): JsValue {.inline.} = JsValue(h: rawBoolToJs(b))
proc toBool*(v: JsValue): bool {.inline.} = rawToBool(v.h)

proc toJs*(s: string): JsValue =
  ## A Nim `string` as a real JS string (UTF-8 decoded).
  var t = s
  JsValue(h: rawStrToJs(toCString(t), t.len))

proc toStr*(v: JsValue): string =
  ## A JS value stringified (`String(v)`) and copied into a Nim `string`.
  let n = rawStrLen(v.h)
  if n <= 0: return ""
  result = newString(n)
  rawStrInto(v.h, toCString(result))

proc `$`*(v: JsValue): string {.inline.} = toStr(v)

proc newJsObject*(): JsValue {.inline.} = JsValue(h: rawNewObject())
  ## An empty JS `{}` object.

# ── JS arrays ────────────────────────────────────────────────────────────────
proc newJsArray*(): JsValue {.inline.} = JsValue(h: rawNewArray())
  ## An empty JS `[]` array.
proc len*(arr: JsValue): int {.inline.} = rawArrLen(arr.h)
  ## `arr.length`.
proc add*(arr: JsValue; val: JsValue) {.inline.} = rawArrPush(arr.h, val.h)
  ## `arr.push(val)`. The array takes its own reference to the value, so `val`'s
  ## handle may be released afterwards without affecting the array.
proc `[]`*(arr: JsValue; i: int): JsValue {.inline.} = JsValue(h: rawArrGet(arr.h, i))
  ## `arr[i]` — returns a fresh owning handle to the element.
proc `[]=`*(arr: JsValue; i: int; val: JsValue) {.inline.} = rawArrSet(arr.h, i, val.h)
  ## `arr[i] = val`.

# ── Nim procs as JS callbacks (event handlers) ───────────────────────────────
type
  JsProc0* = proc() {.nimcall.}
  JsProc1* = proc(ev: JsValue) {.nimcall.}
    ## A one-argument callback — the shape of a DOM event handler. Its `JsValue`
    ## argument is only valid for the duration of the call (the runtime releases
    ## the handle when the callback returns), matching the DOM event contract.

proc rawFnToJs0(p: JsProc0): int32 {.importc: "_fnToJs0".}
proc rawFnToJs1(p: JsProc1): int32 {.importc: "_fnToJs1".}
proc toJs*(p: JsProc0): JsValue {.inline.} = JsValue(h: rawFnToJs0(p))
  ## A Nim proc as a JS function taking no arguments.
proc toJs*(p: JsProc1): JsValue {.inline.} = JsValue(h: rawFnToJs1(p))
  ## A Nim proc as a JS function taking one argument (marshalled to a handle).

# ── globals, properties, methods, construction ───────────────────────────────
proc global*(name: string): JsValue =
  ## `globalThis[name]` — e.g. `global("document")`, `global("Math")`.
  let n = toJs(name)
  JsValue(h: rawGlobalH(n.h))

proc get*(obj: JsValue; name: string): JsValue =
  ## `obj[name]`.
  let n = toJs(name)
  JsValue(h: rawGetProp(obj.h, n.h))

proc set*(obj: JsValue; name: string; val: JsValue) =
  ## `obj[name] = val`.
  let n = toJs(name)
  rawSetProp(obj.h, n.h, val.h)

proc call*(obj: JsValue; name: string): JsValue =
  ## `obj.name()`.
  let n = toJs(name)
  JsValue(h: rawCall0(obj.h, n.h))

proc call*(obj: JsValue; name: string; a: JsValue): JsValue =
  ## `obj.name(a)`.
  let n = toJs(name)
  JsValue(h: rawCall1(obj.h, n.h, a.h))

proc call*(obj: JsValue; name: string; a, b: JsValue): JsValue =
  ## `obj.name(a, b)`.
  let n = toJs(name)
  JsValue(h: rawCall2(obj.h, n.h, a.h, b.h))

proc call*(obj: JsValue; name: string; a, b, c: JsValue): JsValue =
  ## `obj.name(a, b, c)`.
  let n = toJs(name)
  JsValue(h: rawCall3(obj.h, n.h, a.h, b.h, c.h))

proc newOf*(ctorName: string): JsValue =
  ## `new globalThis[ctorName]()` — e.g. `newOf("EventTarget")`.
  let c = global(ctorName)
  JsValue(h: rawCtor0(c.h))

proc newOf*(ctorName: string; a: JsValue): JsValue =
  ## `new globalThis[ctorName](a)` — e.g. `newOf("Event", toJs("click"))`.
  let c = global(ctorName)
  JsValue(h: rawCtor1(c.h, a.h))

# ── introspection ────────────────────────────────────────────────────────────
proc jsTypeof*(v: JsValue): string =
  ## The JS `typeof v` string ("number", "string", "object", "function", …).
  ## (Named `jsTypeof`, not `typeof`, to avoid Nim's `typeof` builtin.)
  toStr(JsValue(h: rawTypeof(v.h)))

proc hasProp*(obj: JsValue; name: string): bool =
  ## `name in obj`.
  let n = toJs(name)
  rawHasProp(obj.h, n.h)

proc instanceOf*(v: JsValue; ctorName: string): bool =
  ## `v instanceof globalThis[ctorName]` — e.g. `n.instanceOf("Array")`.
  let c = global(ctorName)
  rawInstanceOf(v.h, c.h)

proc apply*(obj: JsValue; name: string; args: openArray[JsValue]): JsValue =
  ## `obj.name(...args)` — a method call with any number of arguments (the
  ## variadic counterpart of `call`; the args are marshalled through a JS array).
  let arr = newJsArray()
  for x in args:
    arr.add(x)
  let n = toJs(name)
  JsValue(h: rawApply(obj.h, n.h, arr.h))

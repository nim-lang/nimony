## Minimal JS-value interop for the Leng JS backend (prototype).
##
## The JS backend lays native Nim data out as byte offsets into one linear
## `ArrayBuffer`; a genuine JS value (string, object, function, DOM node) can't
## live there. `JsValue` is an opaque integer HANDLE into a runtime-side value
## table (`runtime.js`), the generalisation of the proc-pointer `_fns` table the
## backend already uses. This module wraps the `importc` seam — an `importc`
## proc with no body lowers to a plain call of the named runtime function — into
## an ergonomic surface: marshal Nim `string`/`int`/`bool` to and from JS,
## look up globals, and read/set properties and call methods on JS objects.
##
## This is the foundation a DOM binding sits on: `document`, `Element`, event
## handlers, etc. are all `JsValue`s reached through `global`/`get`/`call`.
##
## Lifetime: handles are NOT yet GC-integrated — a value you keep should be
## `release`d when done (the wrappers release the transient member-name handles
## they create themselves). Slot 0 is `undefined`/`null`, mirroring nil = 0.

type
  JsValue* = distinct int32
    ## Opaque handle to a JS value held in the runtime's side table.

const undefined* = JsValue(0)

# ── raw runtime seam (importc → runtime.js functions) ────────────────────────
proc release*(v: JsValue) {.importc: "_jsRelease".}
  ## Free the handle's slot in the runtime table.

proc jsStrictEq(a, b: JsValue): bool {.importc: "_jsStrictEq".}

proc `==`*(a, b: JsValue): bool {.inline.} = jsStrictEq(a, b)
  ## JS `===`: two distinct handles to the same value compare equal (this is
  ## *not* handle-integer equality).
proc isNil*(v: JsValue): bool {.inline.} = int32(v) == 0
  ## True for the nil handle — both JS `undefined` and `null` marshal to it.

proc strToJs(p: cstring; n: int): JsValue {.importc: "_strToJs".}
proc jsStrLen(v: JsValue): int {.importc: "_jsStrLen".}
proc jsStrInto(v: JsValue; dst: cstring) {.importc: "_jsStrInto".}

proc toJs*(x: int): JsValue {.importc: "_numToJs".}
  ## A Nim `int` as a JS Number.
proc toInt*(v: JsValue): int {.importc: "_jsToNum".}
  ## A JS Number as a Nim `int`.
proc toJs*(b: bool): JsValue {.importc: "_boolToJs".}
proc toBool*(v: JsValue): bool {.importc: "_jsToBool".}

proc jsGlobalH(name: JsValue): JsValue {.importc: "_jsGlobalH".}
proc jsGetProp(obj, name: JsValue): JsValue {.importc: "_jsGetProp".}
proc jsSetProp(obj, name, val: JsValue) {.importc: "_jsSetProp".}
proc jsCall0(obj, name: JsValue): JsValue {.importc: "_jsCall0".}
proc jsCall1(obj, name, a: JsValue): JsValue {.importc: "_jsCall1".}
proc jsCall2(obj, name, a, b: JsValue): JsValue {.importc: "_jsCall2".}
proc jsCall3(obj, name, a, b, c: JsValue): JsValue {.importc: "_jsCall3".}

proc newJsObject*(): JsValue {.importc: "_jsNewObject".}
  ## An empty JS `{}` object.

# ── Nim procs as JS callbacks (event handlers) ───────────────────────────────
type
  JsProc0* = proc() {.nimcall.}
  JsProc1* = proc(ev: JsValue) {.nimcall.}
    ## A one-argument callback — the shape of a DOM event handler. Its `JsValue`
    ## argument is only valid for the duration of the call (the runtime releases
    ## the handle when the callback returns), matching the DOM event contract.

proc toJs*(p: JsProc0): JsValue {.importc: "_fnToJs0".}
  ## A Nim proc as a JS function taking no arguments.
proc toJs*(p: JsProc1): JsValue {.importc: "_fnToJs1".}
  ## A Nim proc as a JS function taking one argument (marshalled to a handle).

# ── construction (`new Ctor(...)`) ───────────────────────────────────────────
proc jsCtor0(ctor: JsValue): JsValue {.importc: "_jsCtor0".}
proc jsCtor1(ctor, a: JsValue): JsValue {.importc: "_jsCtor1".}

# ── string marshalling ───────────────────────────────────────────────────────
proc toJs*(s: string): JsValue =
  ## A Nim `string` as a real JS string (UTF-8 decoded).
  var t = s
  result = strToJs(toCString(t), t.len)

proc toStr*(v: JsValue): string =
  ## A JS value stringified (`String(v)`) and copied into a Nim `string`.
  let n = jsStrLen(v)
  if n <= 0: return ""
  result = newString(n)
  jsStrInto(v, toCString(result))

proc `$`*(v: JsValue): string {.inline.} = toStr(v)

# ── globals, properties, methods (member name keyed by JS string) ────────────
proc global*(name: string): JsValue =
  ## `globalThis[name]` — e.g. `global("document")`, `global("Math")`.
  let n = toJs(name); result = jsGlobalH(n); n.release

proc get*(obj: JsValue; name: string): JsValue =
  ## `obj[name]`.
  let n = toJs(name); result = jsGetProp(obj, n); n.release

proc set*(obj: JsValue; name: string; val: JsValue) =
  ## `obj[name] = val`.
  let n = toJs(name); jsSetProp(obj, n, val); n.release

proc call*(obj: JsValue; name: string): JsValue =
  ## `obj.name()`.
  let n = toJs(name); result = jsCall0(obj, n); n.release

proc call*(obj: JsValue; name: string; a: JsValue): JsValue =
  ## `obj.name(a)`.
  let n = toJs(name); result = jsCall1(obj, n, a); n.release

proc call*(obj: JsValue; name: string; a, b: JsValue): JsValue =
  ## `obj.name(a, b)`.
  let n = toJs(name); result = jsCall2(obj, n, a, b); n.release

proc call*(obj: JsValue; name: string; a, b, c: JsValue): JsValue =
  ## `obj.name(a, b, c)`.
  let n = toJs(name); result = jsCall3(obj, n, a, b, c); n.release

proc newOf*(ctorName: string): JsValue =
  ## `new globalThis[ctorName]()` — e.g. `newOf("EventTarget")`.
  let c = global(ctorName); result = jsCtor0(c); c.release

proc newOf*(ctorName: string; a: JsValue): JsValue =
  ## `new globalThis[ctorName](a)` — e.g. `newOf("Event", toJs("click"))`.
  let c = global(ctorName); result = jsCtor1(c, a); c.release

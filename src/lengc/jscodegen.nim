#
#
#           Leng Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## JavaScript backend for Leng.
##
## A third codegen over the same Leng IR consumed by the C backend
## (`codegen.nim`) and the LLVM backend (`llvmcodegen.nim`). Rather than assemble
## JS by string concatenation, it **builds a NIF tree of JS constructs** (`jsnif`,
## a dedicated `JsTag` vocabulary) and a tiny `jsnif -> JS` printer emits the
## text — Araq's direction on PR #2043: *"produce the JS as a NIF-tree with a
## dedicated JS enum … then a tiny jsnif -> js emitter. This way the code is not
## full of stupid string operations we have to keep correct all the time, plus
## you can easily run a peephole optimizer later on the nifjs."* So this file's
## job is purely *translation* Leng -> jsnif; parenthesization, indentation and
## every other syntactic detail live in `jsnif.emit`.
##
## **Memory model (Araq's Typed-Array direction).** Nim-native data —
## `object`/`array`/`string` and pointers to them — lives as bytes in one linear
## `ArrayBuffer` (`mem`), so `cast` and low-level libs work uniformly:
##   - an aggregate is `allocFixed`'d and accessed by typed load/store at the byte
##     offsets `jslayout` computes (`mem.setX(p+off, v)` / `mem.getX(p+off)`);
##   - a pointer is a single INTEGER byte offset. `addr o.f` is `p+off`, `addr
##     a[i]` is `p+i*stride`, `deref p` is a typed load at `p`, `==` is integer
##     `===`, nil is offset 0. `(addr (deref p))`/`(deref (addr loc))` cancel;
##   - an address-taken scalar local is spilled to a buffer slot (C's stack
##     model), so it too has a real address — found by the `scanForAddr` pre-pass.
##
## The fat-pointer `[base, key]` form (the classic Nim JS backend's `etyBaseIndex`)
## is retained only as a fallback for undeclared/opaque types and is reserved for
## the future `importjs`/`jsstring` interop layer; native data never uses it.
##
## Constructs outside the supported subset emit a `jUndef` marker (a valid
## `undefined` expression), so generation always completes and gaps stay visible.

import std / [assertions, syncio, strutils, formatfloat, sets, tables]

import ".." / lib / nifcoreparse   # re-exports nifcore (Cursor, beginRead, intVal, ...)
import ".." / lib / nifcdecl        # stmtKind/exprKind/substructureKind + Leng enums + decl extractors
import mangler
import noptions
import nifmodules                   # MainModule + load
import jslayout                     # C-ABI layout: typeLayout/objectFields/accessOf (buffer model)
import jsnif                        # the JS-as-NIF model + emitter (the only place JS text is produced)
from ".." / lib / vfs import vfsExists, vfsRead, vfsWrite
from std / os import extractFilename

type
  JSGenFlag* = enum
    gfMainModule

  JSGen = object
    m: MainModule
    js: JsBuilder           ## the jsnif tree under construction
    flags: set[JSGenFlag]
    todos: int
    boxed: HashSet[SymId]   ## locals whose address is taken (see `scanForAddr`)
    localTypes: Table[SymId, Cursor]  ## var/param -> declared type (for buffer-model access)

proc initJSGen(m: sink MainModule; flags: set[JSGenFlag]): JSGen =
  JSGen(m: m, js: initJsBuilder(), flags: flags, boxed: initHashSet[SymId](),
        localTypes: initTable[SymId, Cursor]())

# ── symbol naming ─────────────────────────────────────────────────────────────

proc name(g: var JSGen; symId: SymId): string =
  ## The JS identifier for a symbol. `importc`/`exportc` symbols use their
  ## external (C) name — resolved cross-module via the lazily-loaded foreign
  ## declarations, exactly as the C backend's `mangleSym` does — so a call into
  ## another module's `importc` proc/global lands on the runtime name (e.g.
  ## `stdout`, `fwrite`) rather than a mangled stub. Everything else mangles.
  let d = g.m.getDeclOrNil(symId)
  if d != nil and d.extern != StrId(0):
    result = g.m.pool.strings[d.extern]
  else:
    result = mangleToC(g.m.pool.syms[symId])

proc isImportc(g: var JSGen; symId: SymId): bool =
  ## True for `importc`/`importcpp` symbols: they name external entities, so the
  ## JS backend references them but emits no definition (a runtime provides them).
  let d = g.m.getDeclOrNil(symId)
  result = d != nil and d.isImport

proc isProc(g: var JSGen; symId: SymId): bool =
  ## True when `symId` names a proc (as opposed to a var/param/field). Drives the
  ## function-table model: a proc *called directly* is a plain JS call; a proc used
  ## as a *value* becomes a table index, and a call through such an index (a proc
  ## variable / closure field) is dispatched via `_fns[idx]`.
  let d = g.m.getDeclOrNil(symId)
  result = d != nil and d.kind == ProcY

proc fieldName(g: JSGen; symId: SymId): string {.inline.} =
  ## Object-field key: always the mangled field name (matching the C backend),
  ## never an extern name — `name`'s foreign-decl lookup is for top-level symbols.
  mangleToC(g.m.pool.syms[symId])

# ── expression builder ────────────────────────────────────────────────────────

proc gx(g: var JSGen; n: var Cursor)

template memMeth(g: var JSGen; meth: string; args: untyped) =
  ## `mem.<meth>(<args>)` — the runtime accessor calls (`mem.i64n(p)`,
  ## `mem.setI64(p, v)`, `mem.copy(d, s, n)`). `args` appends the argument nodes.
  g.js.tree jCall:
    g.js.member meth: g.js.name "mem"
    args

template jbin(g: var JSGen; op: string; a, b: untyped) =
  ## `(a op b)` — the emitter supplies the parentheses.
  g.js.tree jBin:
    g.js.addOp op
    a
    b

proc jallocVar(g: var JSGen; nm: string; sz: int64) =
  ## `let <nm> = allocFixed(<sz>);`
  g.js.tree jVar:
    g.js.name nm
    g.js.tree jCall:
      g.js.name "allocFixed"
      g.js.num sz

proc todo(g: var JSGen; what: string; n: var Cursor) =
  ## Placeholder for an unsupported node — a valid `undefined` expression, so the
  ## output always *parses* even where an expression is required. The gap stays
  ## visible and is counted in `g.todos`.
  g.js.undef("TODO:" & what)
  inc g.todos
  skip n

proc binTyped(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op TYPE lhs rhs)` -> `(lhs opr rhs)`; the type operand is irrelevant in JS.
  g.js.tree jBin:
    g.js.addOp opr
    n.into:
      skip n            # result type
      g.gx n
      g.gx n
      while n.hasMore: skip n

proc intDiv(g: var JSGen; n: var Cursor) =
  ## Nim integer `div` truncates toward zero; JS `/` is float division.
  g.js.tree jCall:
    g.js.member "trunc": g.js.name "Math"
    g.js.tree jBin:
      g.js.addOp "/"
      n.into:
        skip n
        g.gx n
        g.gx n
        while n.hasMore: skip n

proc binPlain(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op lhs rhs)` (comparisons / logic): no type operand.
  g.js.tree jBin:
    g.js.addOp opr
    n.into:
      g.gx n
      g.gx n
      while n.hasMore: skip n

proc unTyped(g: var JSGen; n: var Cursor; opr: string) =
  g.js.tree jUn:
    g.js.addOp opr
    n.into:
      skip n            # type
      g.gx n
      while n.hasMore: skip n

proc unPlain(g: var JSGen; n: var Cursor; opr: string) =
  g.js.tree jUn:
    g.js.addOp opr
    n.into:
      g.gx n
      while n.hasMore: skip n

proc genCall(g: var JSGen; n: var Cursor) =
  n.into:
    if n.kind == Symbol and g.isProc(n.symId):
      # direct call: the callee names a proc -> a plain JS call `name(args)`.
      g.js.tree jCall:
        g.js.name g.name(n.symId); inc n
        while n.hasMore: g.gx n
    else:
      # indirect call: the callee is a function-table index (a proc variable or a
      # closure's fn field) -> `_fns[idx](args)`. JS can't call an integer, so the
      # index is resolved through the runtime table.
      g.js.tree jCall:
        g.js.tree jIndex:
          g.js.name "_fns"
          g.gx n          # the fn-pointer index (advances past the callee)
        while n.hasMore: g.gx n

# ── buffer model (Typed-Array linear memory, Araq's M2 direction) ────────────
# A Nim-native object is bytes in linear memory: construction is `allocFixed` +
# typed stores at the field byte-offsets `jslayout` computes, field access is a
# typed load. This path is taken only when the object type has a resolvable
# layout (a declared `object`); JS-interop objects and as-yet-undeclared types
# stay on the legacy JS-object / fat-pointer mapping.

proc accessors(ak: AccessKind): (string, string) =
  ## `(load, store)` method names on the `mem` runtime for a scalar access kind.
  case ak
  of akI8:  ("i8", "setI8")
  of akI16: ("i16", "setI16")
  of akI32: ("i32", "setI32")
  of akI64: ("i64n", "setI64")
  of akU8:  ("u8At", "setU8")
  of akU16: ("u16", "setU16")
  of akU32: ("u32", "setU32")
  of akU64: ("u64n", "setU64")
  of akF32: ("f32", "setF32")
  of akF64: ("f64", "setF64")
  of akPtr: ("i64n", "setI64")     ## a pointer is a pointer-size (64-bit) integer
  of akAggregate: ("", "")         ## no scalar accessor (sub-object / array)

proc isBufferObjectType(g: var JSGen; typ: Cursor): bool =
  ## True when `typ` resolves to a declared `object` — the trigger for laying it
  ## out in linear memory rather than as a native JS object.
  var t = typ
  if t.typeKind == ObjectT: return true
  # Only resolve locally-declared symbols: `getDeclOrNil` asserts on an
  # unresolvable suffixed symbol (foreign-load path), and cross-module object
  # types stay on the legacy mapping for this increment.
  if t.typeKind == NoType and t.kind == Symbol and g.m.hasResolvableDecl(t.symId):
    let def = g.m.getDeclOrNil(t.symId)
    if def != nil:
      var p = def.pos
      return asTypeDecl(p).body.typeKind == ObjectT
  false

proc resolveType(g: var JSGen; typ: Cursor): Cursor =
  ## Resolve a named type to its declared body (locally-declared only, to avoid
  ## `getDeclOrNil`'s foreign-load assert); otherwise return the type unchanged.
  result = typ
  if typ.typeKind == NoType and typ.kind == Symbol and g.m.hasResolvableDecl(typ.symId):
    let def = g.m.getDeclOrNil(typ.symId)
    if def != nil:
      var p = def.pos
      result = asTypeDecl(p).body

proc arrayElemInfo(g: var JSGen; arrTyp: Cursor): (bool, int64, AccessKind) =
  ## `(isArray, elementStride, elementAccessKind)` for an array/flexarray type. The
  ## stride is the element size rounded up to its alignment (C array layout). A
  ## flexarray `(flexarray elem)` has the same element-then-optional-tail shape as
  ## `(array elem size)`, so the same walk covers both.
  let t = g.resolveType(arrTyp)
  if t.typeKind notin {ArrayT, FlexarrayT}: return (false, 0'i64, akAggregate)
  var n = t
  var stride = 0'i64
  var ak = akAggregate
  n.into:
    let elem = n
    let lay = typeLayout(g.m, elem)
    stride = (if lay.align <= 1: lay.size else: (lay.size + lay.align - 1) and not (lay.align - 1))
    ak = accessOf(g.m, elem)
    while n.hasMore: skip n
  (true, stride, ak)

proc isBufferArray(g: var JSGen; typ: Cursor): bool =
  g.arrayElemInfo(typ)[0]

proc isBufferAggregate(g: var JSGen; typ: Cursor): bool =
  ## A Nim-native aggregate laid out in linear memory: a declared object or an
  ## array. (Unions/flexarrays fall here too once needed.)
  g.isBufferObjectType(typ) or g.isBufferArray(typ)

proc exprType(g: var JSGen; n: Cursor): (bool, Cursor) =
  ## Static type of an lvalue expression, resolved structurally. Returns a type
  ## Cursor `jslayout` can consume (a named type or a type node), or `(false, n)`
  ## when it can't be determined. This is what lets buffer access follow through
  ## *nested* pointer/field chains — e.g. `(dot (deref (dot s more)) fullLen)`,
  ## the heap-string path where the dot base is itself a `(deref (dot …))`:
  ##   * a Symbol local           -> its declared type;
  ##   * `(deref p)` / `(pat p i)`-> the pointee of `p`'s (ptr) type;
  ##   * `(dot base field)`       -> the field's type in `base`'s object;
  ##   * `(at arr i)`             -> the array element type;
  ##   * `(cast/conv T x)`        -> the target type `T`.
  ## Guards every object/array lookup behind `isBufferObjectType`/`resolveType`
  ## (which check `hasResolvableDecl`), so an unresolvable foreign symbol never
  ## trips `getDeclOrNil`'s assert.
  result = (false, n)
  var e = n
  case e.exprKind
  of NoExpr:
    if e.kind == Symbol and g.localTypes.hasKey(e.symId):
      result = (true, g.localTypes[e.symId])
  of DerefC, PatC:
    e.into:
      let (ok, t) = g.exprType(e)
      if ok and t.typeKind in {PtrT, AptrT}:
        var inner = t
        inner.into:
          result = (true, inner)
          while inner.hasMore: skip inner
      while e.hasMore: skip e
  of DotC:
    e.into:
      let (ok, bt) = g.exprType(e)
      if ok and g.isBufferObjectType(bt):
        skip e                       # advance to the field symbol
        let fsym = e.symId
        for f in objectFields(g.m, bt):
          if f.sym == fsym: result = (true, f.typ)
      while e.hasMore: skip e
  of AtC:
    e.into:
      let (ok, at) = g.exprType(e)
      if ok:
        let t = g.resolveType(at)
        if t.typeKind == ArrayT:
          var el = t
          el.into:
            result = (true, el)
            while el.hasMore: skip el
      while e.hasMore: skip e
  of CastC, ConvC:
    e.into:
      result = (true, e)             # the explicit target type
      while e.hasMore: skip e
  else: discard

proc dotObjType(g: var JSGen; base: Cursor): (bool, Cursor) =
  ## Resolve the object type of a `(dot BASE field)` base for buffer routing. Any
  ## base whose static type (`exprType`) is a declared object routes to the buffer
  ## — a Symbol object value, `(deref sym)` through a pointer, or a deeper chain
  ## like `(deref (dot s more))`. The caller still renders the base offset by
  ## building the base expression (a pointer-to-aggregate deref yields the
  ## offset). Returns `(false, base)` for anything else (legacy fat-pointer).
  let (ok, t) = g.exprType(base)
  if ok and g.isBufferObjectType(t):
    (true, t)
  else:
    (false, base)

proc pointeeAk(g: var JSGen; operand: Cursor): AccessKind =
  ## Access kind of the pointee for a `(deref p)` / pointer store. The pointer's
  ## static type (`exprType`) gives it: a real `(ptr T)` yields `accessOf(T)`; a
  ## value loosely typed as its pointee yields `accessOf` of that type.
  result = akI64
  let (ok, t) = g.exprType(operand)
  if ok:
    if t.typeKind in {PtrT, AptrT}:
      var inner = t
      inner.into:
        result = accessOf(g.m, inner)
        while inner.hasMore: skip inner
    else:
      result = accessOf(g.m, t)

proc pointeeInfo(g: var JSGen; operand: Cursor): (AccessKind, int64) =
  ## `(accessKind, byteSize)` of the pointee for a pointer operand — used for
  ## `(pat p i)` element arithmetic (`p + i*size`) and its load/store width.
  result = (akU8, 1'i64)     # default: byte pointer (cstring-like)
  let (ok, t) = g.exprType(operand)
  if ok:
    if t.typeKind in {PtrT, AptrT}:
      var inner = t
      inner.into:
        result = (accessOf(g.m, inner), typeLayout(g.m, inner).size)
        while inner.hasMore: skip inner
    else:
      result = (accessOf(g.m, t), typeLayout(g.m, t).size)

# `dest + off` where `dest` is a plain identifier name (a constructed aggregate's
# base). Always emits the `+ off` — the emitter parenthesises it.
proc destPlus(g: var JSGen; dest: string; off: int64) =
  g.jbin("+", g.js.name dest, g.js.num off)

proc constrExtraBytes(g: var JSGen; n: Cursor): int64 =
  ## Extra buffer bytes an `(oconstr …)` needs beyond its fixed layout: the
  ## payload of a flexarray field initialised by a string literal — a static
  ## long-string's `data`. Zero for ordinary objects.
  result = 0
  var c = n
  c.into:
    skip c   # type operand
    while c.hasMore:
      if c.substructureKind == KvU:
        var kv = c
        kv.into:
          inc kv         # field symbol
          if kv.kind == StrLit:
            result += int64(g.m.pool.strings[strId(kv)].len)
          while kv.hasMore: skip kv
      skip c

proc aconstrBytes(g: var JSGen; n: Cursor; arrTyp: Cursor): int64 =
  ## Total bytes an `(aconstr T e0 e1 …)` occupies — element count times stride.
  ## Used to size a flexarray/unsized-array allocation, whose fixed layout is 0.
  let (_, stride, _) = g.arrayElemInfo(arrTyp)
  var count = 0'i64
  var c = n
  c.into:
    skip c            # element/array type operand
    while c.hasMore:
      inc count
      skip c
  count * stride

proc constructObjectInto(g: var JSGen; dest: string; n: var Cursor) =
  ## Build the stores for an `(oconstr Type (kv f v) …)` into the storage at
  ## `dest` (already allocated): one `mem.setX(dest + off, v)` per field, or a
  ## `mem.copy` for a nested aggregate field.
  n.into:
    let ty = n            # object type (independent cursor copy)
    skip n                # advance past the type operand
    let fields = objectFields(g.m, ty)
    while n.hasMore:
      if n.substructureKind == KvU:
        n.into:
          let fsym = n.symId; inc n
          var off = 0'i64
          var ftyp = n            # placeholder; overwritten when the field is found
          for f in fields:
            if f.sym == fsym:
              off = f.offset; ftyp = f.typ
          let ak = accessOf(g.m, ftyp)
          if n.kind == StrLit:
            # a static string payload — the `data` flexarray of a long-string
            # literal. Write its bytes into the tail the caller over-allocated
            # (see `constrExtraBytes`); the runtime `mem.writeStr` copies them in.
            let s = g.m.pool.strings[strId(n)]
            g.js.tree jExprStmt:
              g.memMeth("writeStr"):
                g.destPlus(dest, off)
                g.js.str s
            inc n
          elif ak == akAggregate:
            let fsize = typeLayout(g.m, ftyp).size
            g.js.tree jExprStmt:
              g.memMeth("copy"):
                g.destPlus(dest, off)
                g.gx n
                g.js.num fsize
          else:
            let (_, st) = accessors(ak)
            g.js.tree jExprStmt:
              g.memMeth(st):
                g.destPlus(dest, off)
                g.gx n              # field value
          while n.hasMore: skip n
      else:
        skip n

proc constructArrayInto(g: var JSGen; dest: string; n: var Cursor; arrTyp: Cursor) =
  ## Build the stores for an `(aconstr T e0 e1 …)` into `dest`: one
  ## `mem.setX(dest + i*stride, ei)` per element (or `mem.copy` for aggregates).
  let (_, stride, ak) = g.arrayElemInfo(arrTyp)
  n.into:
    skip n                # array type operand
    var i = 0'i64
    while n.hasMore:
      if ak == akAggregate:
        g.js.tree jExprStmt:
          g.memMeth("copy"):
            g.destPlus(dest, i * stride)
            g.gx n
            g.js.num stride
      else:
        let (_, st) = accessors(ak)
        g.js.tree jExprStmt:
          g.memMeth(st):
            g.destPlus(dest, i * stride)
            g.gx n
      inc i

# ── addr / deref / eq over the byte-pointer model ─────────────────────────────

proc genAddrOf(g: var JSGen; n: var Cursor) =
  ## `(addr LOC)` -> an integer BYTE OFFSET into the buffer. A field's address is
  ## `base + off`, an element's is `base + i*stride`, an address-taken scalar
  ## local's is its own slot offset. `(addr (deref p))` cancels to `p`. Fat
  ## pointers `[base, key]` are the legacy fallback for undeclared types.
  case n.exprKind
  of DerefC:
    n.into:
      g.gx n                        # addr(deref p) = p
      while n.hasMore: skip n
  of DotC:
    n.into:
      let (isBuf, oty) = g.dotObjType(n)
      if isBuf:
        g.js.tree jBin:
          g.js.addOp "+"
          g.gx n                    # base offset (advances past it)
          let fsym = n.symId; inc n
          var off = 0'i64
          for f in objectFields(g.m, oty):
            if f.sym == fsym: off = f.offset
          g.js.num off
      else:
        g.js.tree jArray:           # fat pointer [base, "field"]
          g.gx n
          let fsym = n.symId; inc n
          g.js.str g.fieldName(fsym)
      while n.hasMore: skip n
  of AtC:
    n.into:
      var aty = n
      var isArr = false
      if n.kind == Symbol and g.localTypes.hasKey(n.symId):
        aty = g.localTypes[n.symId]; isArr = g.isBufferArray(aty)
      if isArr:
        let (_, stride, _) = g.arrayElemInfo(aty)
        g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
      else:
        g.js.tree jArray:           # fat pointer [base, idx]
          g.gx n
          g.gx n
      while n.hasMore: skip n
  of PatC:
    n.into:
      let (_, stride) = g.pointeeInfo(n)
      g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
      while n.hasMore: skip n
  of NoExpr:
    if n.kind == Symbol:
      g.js.name g.name(n.symId); inc n     # address-taken scalar: its slot offset
    else:
      g.todo("addr-of-location", n)
  else:
    g.todo("addr-of-location", n)

proc genDeref(g: var JSGen; n: var Cursor) =
  ## `(deref p)` -> a typed load at byte offset `p`. `(deref (addr LOC))` cancels
  ## to reading LOC directly; a pointer-to-aggregate deref is the offset itself.
  if n.exprKind == AddrC:
    n.into:
      g.gx n
      while n.hasMore: skip n
  else:
    let ak = g.pointeeAk(n)
    if ak == akAggregate:
      g.gx n                       # the offset itself
    else:
      let (ld, _) = accessors(ak)
      g.memMeth(ld): g.gx n

proc genEq(g: var JSGen; n: var Cursor; negate: bool) =
  ## `(eq a b)` / `(neq a b)`. Pointers are integer byte offsets, so a plain
  ## strict comparison is correct for addresses too.
  g.js.tree jBin:
    g.js.addOp (if negate: "!==" else: "===")
    n.into:
      g.gx n
      g.gx n
      while n.hasMore: skip n

# ── the main expression translator ────────────────────────────────────────────

proc gx(g: var JSGen; n: var Cursor) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:   g.js.num intVal(n); inc n
    of UIntLit:  g.js.unum uintVal(n); inc n
    of FloatLit: g.js.raw $floatVal(n); inc n
    of CharLit:  g.js.num int64(ord(n.charLit)); inc n
    of StrLit:   g.js.str g.m.pool.strings[strId(n)]; inc n
    of Symbol:
      let nm = g.name(n.symId)
      if g.isProc(n.symId):
        # a proc used as a VALUE (stored in a closure / proc variable): its stable
        # function-table index, `_fnid(fn)`. A *called* proc never reaches here —
        # `genCall` emits the direct call itself.
        g.js.tree jCall:
          g.js.name "_fnid"
          g.js.name nm
        inc n
      else:
        # an address-taken SCALAR local lives in a buffer slot (so it has a real
        # byte address); read it with a typed load. An aggregate local already holds
        # its offset, and ordinary locals stay JS registers — both read by name.
        let ak = (if n.symId in g.boxed and g.localTypes.hasKey(n.symId): accessOf(g.m, g.localTypes[n.symId])
                  elif n.symId in g.boxed: akI64
                  else: akAggregate)   # sentinel meaning "emit name"
        if n.symId in g.boxed and ak != akAggregate:
          let (ld, _) = accessors(ak)
          g.memMeth(ld): g.js.name nm
        else:
          g.js.name nm
        inc n
    of DotToken:
      g.js.raw "undefined"; inc n
    else:
      g.todo("expr:" & $n.kind, n)
  of TrueC: g.js.raw "true"; skip n
  of FalseC: g.js.raw "false"; skip n
  of NilC: g.js.num 0; skip n   # a nil pointer is byte offset 0 (reserved as null)
  of InfC: g.js.raw "Infinity"; skip n
  of NegInfC: g.js.raw "(-Infinity)"; skip n
  of NanC: g.js.raw "NaN"; skip n
  of OvfC:
    # The overflow flag (see `KeepovfS`). JS numbers are doubles with no 64-bit
    # overflow trap, so nothing sets it — a checked op never reports overflow.
    g.js.raw "false"; skip n
  of CallC: genCall g, n
  of AddC: binTyped g, n, "+"
  of SubC: binTyped g, n, "-"
  of MulC: binTyped g, n, "*"
  of DivC: intDiv g, n
  of ModC: binTyped g, n, "%"
  of ShlC: binTyped g, n, "<<"
  of ShrC: binTyped g, n, ">>"
  of BitandC: binTyped g, n, "&"
  of BitorC: binTyped g, n, "|"
  of BitxorC: binTyped g, n, "^"
  of BitnotC: unTyped g, n, "~"
  of NegC: unTyped g, n, "-"
  of AndC: binPlain g, n, "&&"
  of OrC: binPlain g, n, "||"
  of NotC: unPlain g, n, "!"
  of EqC: genEq g, n, false
  of NeqC: genEq g, n, true
  of LeC: binPlain g, n, "<="
  of LtC: binPlain g, n, "<"
  of CastC, ConvC:
    # JS is untyped: a conversion/cast is the inner value (skip the type).
    n.into:
      skip n
      g.gx n
      while n.hasMore: skip n
  of ParC:
    n.into:
      g.gx n
      while n.hasMore: skip n
  of SufC:
    # literal-with-suffix: emit the value, drop the suffix annotation.
    n.into:
      g.gx n
      while n.hasMore: skip n
  of OconstrC:
    # `(oconstr Type (kv f v) …)`. A Nim-native object materialises into a fresh
    # buffer allocation (an IIFE returning the offset), so an inline object value
    # is a pointer just like a stored one. Undeclared types fall back to a JS
    # object literal `{field: value, …}`.
    var oty = n
    var isBuf = false
    block:
      var probe = n
      probe.into:
        oty = probe
        isBuf = g.isBufferObjectType(oty)
        while probe.hasMore: skip probe
    if isBuf:
      let sz = typeLayout(g.m, oty).size + g.constrExtraBytes(n)  # + static string data
      g.js.tree jCall:                       # (() => { … })()
        g.js.tree jArrow:
          g.js.tree jParams: discard
          g.js.tree jBlock:
            g.jallocVar("_o", sz)
            g.constructObjectInto("_o", n)
            g.js.tree jRet: g.js.name "_o"
    else:
      n.into:
        skip n            # object type
        g.js.tree jObject:
          while n.hasMore:
            if n.substructureKind == KvU:
              g.js.tree jKv:
                n.into:
                  g.js.kvKey g.fieldName(n.symId); inc n   # field name as key
                  g.gx n                                   # value
                  while n.hasMore: skip n
            else:
              skip n
  of AconstrC:
    # `(aconstr Type e0 e1 …)`. A Nim-native array materialises into a fresh
    # buffer allocation (IIFE -> offset); otherwise a JS array literal.
    var aty = n
    var isArr = false
    block:
      var probe = n
      probe.into:
        aty = probe
        isArr = g.isBufferArray(aty)
        while probe.hasMore: skip probe
    if isArr:
      let sz = typeLayout(g.m, aty).size
      g.js.tree jCall:
        g.js.tree jArrow:
          g.js.tree jParams: discard
          g.js.tree jBlock:
            g.jallocVar("_a", sz)
            g.constructArrayInto("_a", n, aty)
            g.js.tree jRet: g.js.name "_a"
    else:
      n.into:
        skip n            # element/array type
        g.js.tree jArray:
          while n.hasMore:
            g.gx n
  of AddrC:
    n.into:
      g.genAddrOf n
      while n.hasMore: skip n
  of DerefC:
    n.into:
      g.genDeref n
      while n.hasMore: skip n
  of DotC:
    # `(dot obj field …)`. For a Nim-native object (a local of declared object
    # type, or through a pointer) this is a typed load at the field's byte offset;
    # otherwise the legacy `obj.field` mapping (JS-interop / undeclared types).
    n.into:
      let (useBuffer, oty) = g.dotObjType(n)
      if useBuffer:
        # peek the field ahead (without consuming the base) to choose load vs
        # bare-offset before committing the outer node shape.
        var probe = n
        skip probe
        let pfsym = probe.symId
        var off = 0'i64
        var ftyp = probe
        var found = false
        for f in objectFields(g.m, oty):
          if f.sym == pfsym: off = f.offset; ftyp = f.typ; found = true
        let ak = (if found: accessOf(g.m, ftyp) else: akAggregate)
        if ak == akAggregate:
          g.js.tree jBin:
            g.js.addOp "+"
            g.gx n                    # base (advances to field)
            inc n                     # skip field symbol
            g.js.num off
        else:
          let (ld, _) = accessors(ak)
          g.memMeth(ld):
            g.js.tree jBin:
              g.js.addOp "+"
              g.gx n
              inc n
              g.js.num off
      else:
        g.js.tree jMember:
          g.gx n                                  # object
          g.js.addOp g.fieldName(n.symId); inc n  # field ident
      while n.hasMore: skip n        # inheritance depth / access token
  of AtC:
    # `(at arr i)` on a Nim-native array -> typed load at `base + i*stride`;
    # otherwise (undeclared element type) the legacy `arr[i]`.
    n.into:
      var isArr = false
      var aty = n
      if n.kind == Symbol and g.localTypes.hasKey(n.symId):
        aty = g.localTypes[n.symId]
        isArr = g.isBufferArray(aty)
      if isArr:
        let (_, stride, ak) = g.arrayElemInfo(aty)
        if ak == akAggregate:
          g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
        else:
          let (ld, _) = accessors(ak)
          g.memMeth(ld):
            g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
      else:
        g.js.tree jIndex:
          g.gx n
          g.gx n
      while n.hasMore: skip n
  of PatC:
    # `(pat p i)` pointer indexing -> typed load at `p + i*stride` (byte pointer).
    n.into:
      let (ak, stride) = g.pointeeInfo(n)
      if ak == akAggregate:
        g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
      else:
        let (ld, _) = accessors(ak)
        g.memMeth(ld):
          g.jbin("+", g.gx n, g.jbin("*", g.gx n, g.js.num stride))
      while n.hasMore: skip n
  of SizeofC:
    # `(sizeof T)` -> the byte size the layout pass computes.
    n.into:
      g.js.num typeLayout(g.m, n).size
      while n.hasMore: skip n
  else:
    g.todo("expr:" & $n.exprKind, n)

# ── statements ───────────────────────────────────────────────────────────────

proc gs(g: var JSGen; n: var Cursor)

proc genBlock(g: var JSGen; n: var Cursor) =
  ## emit a `(stmts ...)` body as a `jBlock` (the emitter braces + indents it).
  g.js.tree jBlock:
    if n.stmtKind in {StmtsS, ScopeS}:
      n.loopInto:
        g.gs n
    else:
      g.gs n

proc genVar(g: var JSGen; n: var Cursor) =
  var d = takeVarDecl(n)
  if g.isImportc(d.name.symId): return   # external global: provided by the runtime
  g.localTypes[d.name.symId] = d.typ     # remember the type for buffer-model access
  let nm = g.name(d.name.symId)
  if g.isBufferAggregate(d.typ):
    # Nim-native aggregate (object or array): storage in linear memory. Allocate
    # its bytes, then construct the (a)constr into it in place, or copy another
    # aggregate value. Its address is simply its offset (no boxing needed).
    let lay = typeLayout(g.m, d.typ)
    var allocSz = lay.size
    if d.value.kind != DotToken and d.value.exprKind == OconstrC:
      allocSz += g.constrExtraBytes(d.value)   # room for a static string's data
    elif d.value.kind != DotToken and d.value.exprKind == AconstrC and allocSz == 0:
      allocSz = g.aconstrBytes(d.value, d.typ)  # flexarray/unsized: size from elems
    g.jallocVar(nm, allocSz)
    if d.value.kind != DotToken:
      var v = d.value
      if v.exprKind == OconstrC:
        g.constructObjectInto(nm, v)
      elif v.exprKind == AconstrC:
        g.constructArrayInto(nm, v, d.typ)
      else:
        g.js.tree jExprStmt:
          g.memMeth("copy"):
            g.js.name nm
            g.gx v
            g.js.num lay.size
    return
  if d.name.symId in g.boxed:
    # address-taken scalar local: spill to a buffer slot so it has a real byte
    # address (its variable holds the offset). Reads/writes go through mem.
    let lay = typeLayout(g.m, d.typ)
    g.jallocVar(nm, lay.size)
    if d.value.kind != DotToken:
      let (_, st) = accessors(accessOf(g.m, d.typ))
      var v = d.value
      g.js.tree jExprStmt:
        g.memMeth(st):
          g.js.name nm
          g.gx v
  else:
    g.js.tree jVar:
      g.js.name nm
      if d.value.kind != DotToken:
        var v = d.value
        g.gx v
      else:
        g.js.none

proc genIfBranches(g: var JSGen; n: var Cursor) =
  ## Build the `(elif …)*(else …)?` list at `n` into a nested `jIf` chain. Each
  ## `elif` becomes a `jIf` whose else-slot is the rest of the list; a trailing
  ## `else` becomes a `jBlock` in that slot; nothing becomes `jNone`.
  if not n.hasMore: return
  case n.substructureKind
  of ElifU:
    g.js.tree jIf:
      n.into:
        g.gx n            # condition
        g.genBlock n      # then-body
        while n.hasMore: skip n
      if n.hasMore:
        g.genIfBranches n # else = the remaining branches (another jIf or a block)
      else:
        g.js.none
  of ElseU:
    n.into:
      g.genBlock n        # the else-block occupies the parent's else-slot
      while n.hasMore: skip n
  else:
    g.todo("if-branch", n)

proc genIf(g: var JSGen; n: var Cursor) =
  n.into:
    g.genIfBranches n

proc genWhile(g: var JSGen; n: var Cursor) =
  g.js.tree jWhile:
    n.into:
      g.gx n              # condition
      g.genBlock n        # body
      while n.hasMore: skip n

proc genCase(g: var JSGen; n: var Cursor) =
  g.js.tree jSwitch:
    n.into:
      g.gx n              # selector
      while n.hasMore:
        case n.substructureKind
        of OfU:
          g.js.tree jCase:
            n.into:
              g.js.tree jLabels:
                if n.substructureKind == RangesU:
                  n.loopInto: g.gx n
                else:
                  g.gx n
              g.genBlock n
              while n.hasMore: skip n
        of ElseU:
          g.js.tree jDefault:
            n.into:
              g.genBlock n
              while n.hasMore: skip n
        else:
          g.todo("case-branch", n)

proc genLvalueStore(g: var JSGen; lval: Cursor; val: Cursor) =
  ## Store the value expression at `val` into the lvalue `lval`, dispatching on
  ## the byte-pointer model: `(deref p)` -> typed store at `p`; a buffer object
  ## field or array element -> typed store at its offset; an address-taken scalar
  ## local -> store at its slot; anything else -> a plain `lhs = value`.
  var n = lval
  case n.exprKind
  of DerefC:
    n.into:
      if n.exprKind == AddrC:
        n.into:
          g.genLvalueStore(n, val)         # (deref (addr loc)) = v  ->  store loc
          while n.hasMore: skip n
      else:
        let (ak, sz) = g.pointeeInfo(n)
        if ak == akAggregate:
          # storing a whole aggregate through a pointer (`(deref p) = obj`, e.g. a
          # `new`'d heap cell): copy the object's bytes, not a scalar store.
          g.js.tree jExprStmt:
            g.memMeth("copy"):
              g.gx n                        # dest pointer
              (var v = val; g.gx v)         # source offset
              g.js.num sz
        else:
          let (_, st) = accessors(ak)
          g.js.tree jExprStmt:
            g.memMeth(st):
              g.gx n                        # p (the pointer)
              (var v = val; g.gx v)
      while n.hasMore: skip n
  of DotC:
    var isBuf = false
    var oty = n
    block:
      var probe = n
      probe.into:
        (isBuf, oty) = g.dotObjType(probe)
        while probe.hasMore: skip probe
    if isBuf:
      var nn = n
      nn.into:
        # peek field for off/ak/size before rendering the base
        var probe = nn
        skip probe
        let fsym = probe.symId
        var off = 0'i64
        var ak = akAggregate
        var fsize = 0'i64
        for f in objectFields(g.m, oty):
          if f.sym == fsym:
            off = f.offset; ak = accessOf(g.m, f.typ); fsize = typeLayout(g.m, f.typ).size
        if ak == akAggregate:
          g.js.tree jExprStmt:
            g.memMeth("copy"):
              g.jbin("+", g.gx nn, g.js.num off)
              (var v = val; g.gx v)
              g.js.num fsize
        else:
          let (_, st) = accessors(ak)
          g.js.tree jExprStmt:
            g.memMeth(st):
              g.jbin("+", g.gx nn, g.js.num off)
              (var v = val; g.gx v)
        while nn.hasMore: skip nn
    else:
      g.js.tree jAsgn:
        (var nn = n; g.gx nn)
        (var v = val; g.gx v)
  of AtC:
    var isArr = false
    var aty = n
    block:
      var probe = n
      probe.into:
        if probe.kind == Symbol and g.localTypes.hasKey(probe.symId):
          aty = g.localTypes[probe.symId]; isArr = g.isBufferArray(aty)
        while probe.hasMore: skip probe
    if isArr:
      let (_, stride, ak) = g.arrayElemInfo(aty)
      var nn = n
      nn.into:
        if ak == akAggregate:
          g.js.tree jExprStmt:
            g.memMeth("copy"):
              g.jbin("+", g.gx nn, g.jbin("*", g.gx nn, g.js.num stride))
              (var v = val; g.gx v)
              g.js.num stride
        else:
          let (_, st) = accessors(ak)
          g.js.tree jExprStmt:
            g.memMeth(st):
              g.jbin("+", g.gx nn, g.jbin("*", g.gx nn, g.js.num stride))
              (var v = val; g.gx v)
        while nn.hasMore: skip nn
    else:
      g.js.tree jAsgn:
        (var nn = n; g.gx nn)
        (var v = val; g.gx v)
  of PatC:
    # `(pat p i) = v` — a pointer-indexed store (a seq/openarray element write):
    # `mem.setX(p + i*stride, v)`, with stride the pointee size.
    var nn = n
    nn.into:
      let (ak, stride) = g.pointeeInfo(nn)
      if ak == akAggregate:
        g.js.tree jExprStmt:
          g.memMeth("copy"):
            g.jbin("+", g.gx nn, g.jbin("*", g.gx nn, g.js.num stride))
            (var v = val; g.gx v)
            g.js.num stride
      else:
        let (_, st) = accessors(ak)
        g.js.tree jExprStmt:
          g.memMeth(st):
            g.jbin("+", g.gx nn, g.jbin("*", g.gx nn, g.js.num stride))
            (var v = val; g.gx v)
      while nn.hasMore: skip nn
  of NoExpr:
    if n.kind == Symbol and n.symId in g.boxed:
      let ak = (if g.localTypes.hasKey(n.symId): accessOf(g.m, g.localTypes[n.symId]) else: akI64)
      if ak != akAggregate:
        let (_, st) = accessors(ak)
        g.js.tree jExprStmt:
          g.memMeth(st):
            g.js.name g.name(n.symId)
            (var v = val; g.gx v)
      else:
        g.js.tree jAsgn:
          (var nn = n; g.gx nn)
          (var v = val; g.gx v)
    else:
      g.js.tree jAsgn:
        (var nn = n; g.gx nn)
        (var v = val; g.gx v)
  else:
    g.js.tree jAsgn:
      (var nn = n; g.gx nn)
      (var v = val; g.gx v)

proc gs(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken: inc n
    else: g.todo("stmt:" & $n.kind, n)
  of StmtsS:
    n.loopInto: g.gs n
  of ScopeS:
    g.genBlock n
  of VarS, GvarS, TvarS, ConstS:
    g.genVar n
  of AsgnS:
    # `(asgn lvalue value)` — dispatched by `genLvalueStore`.
    n.into:
      let lval = n
      skip n
      let val = n
      g.genLvalueStore(lval, val)
      while n.hasMore: skip n
  of StoreS:
    # `(store value lvalue)` — operands reversed relative to `asgn`.
    n.into:
      let val = n
      skip n
      let lval = n
      g.genLvalueStore(lval, val)
      while n.hasMore: skip n
  of KeepovfS:
    # `(keepovf (op TYPE lhs rhs) dest)` computes and stores into `dest`. JS has no
    # 64-bit overflow trap, so this is just the store.
    n.into:
      let val = n
      skip n
      let lval = n
      g.genLvalueStore(lval, val)
      while n.hasMore: skip n
  of CallS:
    g.js.tree jExprStmt:
      g.genCall n
  of RetS:
    g.js.tree jRet:
      n.into:
        if n.kind != DotToken:
          g.gx n
        else:
          g.js.none; inc n
        while n.hasMore: skip n
  of DiscardS:
    n.into:
      g.js.tree jExprStmt:
        g.gx n
      while n.hasMore: skip n
  of IfS: g.genIf n
  of WhileS: g.genWhile n
  of CaseS: g.genCase n
  of BreakS:
    g.js.leaf jBreak
    skip n
  of LabS:
    # A goto-target label. Vestigial under structured control flow (no matching
    # `jmp`); JS has no `goto`, so emit nothing. A real `jmp` still surfaces as
    # an unsupported node, making any genuine goto use visible.
    skip n
  of RaiseS:
    g.js.tree jThrow:
      n.into:
        if n.kind != DotToken:
          g.gx n
        else:
          g.js.none; inc n
        while n.hasMore: skip n
  else:
    g.todo("stmt:" & $n.stmtKind, n)

# ── declarations / module ────────────────────────────────────────────────────

proc genProc(g: var JSGen; n: var Cursor) =
  var prc = takeProcDecl(n)
  if g.isImportc(prc.name.symId): return   # external proc: provided by the runtime
  g.js.tree jFunc:
    g.js.name g.name(prc.name.symId)
    g.js.tree jParams:
      if prc.params.kind != DotToken:
        var p = prc.params
        p.loopInto:
          var d = takeParamDecl(p)
          g.localTypes[d.name.symId] = d.typ   # remember param types for buffer access
          g.js.name g.name(d.name.symId)
    g.js.tree jBlock:
      # Prologue: a value parameter whose address is taken must be spilled to a
      # buffer slot so a pointer to it is a real byte offset. Re-bind the name to
      # its slot at entry and store the incoming value; all uses inside the body
      # then go through `mem`. A pointer parameter already arrives as an offset.
      if prc.params.kind != DotToken:
        var p = prc.params
        p.loopInto:
          var d = takeParamDecl(p)
          # Only SCALAR value params need spilling; an aggregate param already
          # arrives as a byte offset (addressable), so it is left as-is.
          if d.name.symId in g.boxed and accessOf(g.m, d.typ) != akAggregate:
            let nm = g.name(d.name.symId)
            let sz = typeLayout(g.m, d.typ).size
            let (_, st) = accessors(accessOf(g.m, d.typ))
            g.js.tree jVar:                        # let nm_v = nm;
              g.js.name (nm & "_v"); g.js.name nm
            g.js.tree jAsgn:                       # nm = allocFixed(sz);
              g.js.name nm
              g.js.tree jCall:
                g.js.name "allocFixed"; g.js.num sz
            g.js.tree jExprStmt:                   # mem.setX(nm, nm_v);
              g.memMeth(st): g.js.name nm; g.js.name (nm & "_v")
      var body = prc.body
      if body.stmtKind in {StmtsS, ScopeS}:
        body.loopInto: g.gs body
      else:
        g.gs body

proc genToplevel(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of ProcS: g.genProc n
  of VarS, GvarS, TvarS, ConstS: g.genVar n
  of TypeS: skip n               # no types in JS
  of EmitS:
    # raw passthrough: string literals are emitted verbatim (JS injection).
    n.loopInto:
      if n.kind == StrLit:
        g.js.raw g.m.pool.strings[strId(n)]; inc n
      else:
        g.js.tree jExprStmt:
          g.gx n
  of StmtsS:
    n.loopInto: g.genToplevel n
  of CallS, AsgnS, StoreS, IfS, WhileS, CaseS, DiscardS, ScopeS:
    g.gs n
  else:
    g.todo("toplevel:" & $n.stmtKind, n)

proc scanForAddr(g: var JSGen; n: var Cursor) =
  ## Pre-pass over the whole module: record every local that is the direct
  ## operand of `(addr <symbol>)`. Symbols are module-unique, so a single set
  ## drives boxing for both the declaration and every use.
  if n.kind == TagLit:
    if n.exprKind == AddrC:
      n.into:
        if n.hasMore and n.kind == Symbol:
          g.boxed.incl n.symId
        while n.hasMore: g.scanForAddr n
    else:
      n.loopInto: g.scanForAddr n
  else:
    inc n

proc generateJSCode*(s: var State; inp, outp: string; flags: set[JSGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var g = initJSGen(m, flags)
  var probe = beginRead(g.m.src)
  g.scanForAddr probe
  # Build the whole module as one jsnif `(module …)` tree, then print it.
  var n = beginRead(g.m.src)
  g.js.tree jModule:
    if n.stmtKind == StmtsS:
      n.loopInto: g.genToplevel n
    else:
      g.todo("root", n)
  var output = "// generated by lengc (js backend) from " & extractFilename(inp) & "\n"
  output.add "\"use strict\";\n"
  var opt = peephole(g.js)         # fold `x+0`/`x*1`/constants on the jsnif tree
  output.add emit(opt)
  if g.todos > 0:
    stdout.writeLine "[lengc js] " & inp & ": " & $g.todos & " unsupported node(s) emitted as /*TODO*/"
  if vfsExists(outp) and vfsRead(outp) == output:
    discard "unchanged"
  else:
    vfsWrite outp, output

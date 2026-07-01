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
## (`codegen.nim`) and the LLVM backend (`llvmcodegen.nim`). JavaScript is
## dynamically typed and garbage collected, so this pass drops all type
## generation and maps Leng constructs directly to JS:
##   - `(add T a b)` -> `(a + b)`        (the leading type operand is skipped)
##   - `(asgn x e)`  -> `x = e;`
##   - `(if (elif c (stmts ...)))` -> `if (c) { ... }`
##   - `(try ...)` / `(raise e)` -> native `try`/`throw`
##
## **Memory model (Araq's Typed-Array direction, PR #2043).** Nim-native data —
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
## Constructs outside the supported subset emit a `/*TODO:<tag>*/` marker (a valid
## `undefined` expression), so generation always completes and gaps stay visible.

import std / [assertions, syncio, strutils, formatfloat, sets, tables]

import ".." / lib / nifcoreparse   # re-exports nifcore (Cursor, beginRead, intVal, ...)
import ".." / lib / nifcdecl        # stmtKind/exprKind/substructureKind + Leng enums + decl extractors
import mangler
import noptions
import nifmodules                   # MainModule + load
import jslayout                     # C-ABI layout: typeLayout/objectFields/accessOf (buffer model)
from ".." / lib / vfs import vfsExists, vfsRead, vfsWrite
from std / os import extractFilename

type
  JSGenFlag* = enum
    gfMainModule

  JSGen = object
    m: MainModule
    code: string
    indent: int
    flags: set[JSGenFlag]
    todos: int
    boxed: HashSet[SymId]   ## locals whose address is taken (see `scanForAddr`)
    usesPtrEq: bool         ## whether the module compares fat pointers (needs the helper)
    localTypes: Table[SymId, Cursor]  ## var/param -> declared type (for buffer-model dot access)

proc initJSGen(m: sink MainModule; flags: set[JSGenFlag]): JSGen =
  JSGen(m: m, code: "", indent: 0, flags: flags, boxed: initHashSet[SymId](),
        localTypes: initTable[SymId, Cursor]())

const ptrEqHelper =
  "function nimPtrEq(a, b) { return a[0] === b[0] && a[1] === b[1]; }\n"

# ── low-level emit helpers ───────────────────────────────────────────────────

proc wr(g: var JSGen; s: string) {.inline.} = g.code.add s

proc nl(g: var JSGen) =
  g.code.add "\n"
  for _ in 0 ..< g.indent: g.code.add "  "

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

proc fieldName(g: JSGen; symId: SymId): string {.inline.} =
  ## Object-field key: always the mangled field name (matching the C backend),
  ## never an extern name — `name`'s foreign-decl lookup is for top-level symbols.
  mangleToC(g.m.pool.syms[symId])

proc jsString(s: string): string =
  ## minimal JS string-literal escaping.
  result = "\""
  for ch in s:
    case ch
    of '\\': result.add "\\\\"
    of '"': result.add "\\\""
    of '\n': result.add "\\n"
    of '\r': result.add "\\r"
    of '\t': result.add "\\t"
    else: result.add ch
  result.add "\""

proc todo(g: var JSGen; what: string; n: var Cursor) =
  ## Placeholder for an unsupported node. It is a valid JS expression
  ## (`undefined` with a tagging comment), so the output always *parses* even
  ## where an expression is required — e.g. as a call argument — and a single
  ## unsupported node never breaks the surrounding (possibly never-called)
  ## function. The gap stays visible and is counted in `g.todos`.
  g.wr "undefined/*TODO:" & what & "*/"
  inc g.todos
  skip n

# ── expressions ──────────────────────────────────────────────────────────────

proc gx(g: var JSGen; n: var Cursor)

proc binTyped(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op TYPE lhs rhs)` -> `(lhs <opr> rhs)`; the type operand is irrelevant in JS.
  n.into:
    skip n            # result type
    g.wr "("
    gx g, n
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc intDiv(g: var JSGen; n: var Cursor) =
  ## Nim integer `div` truncates toward zero; JS `/` is float division.
  n.into:
    skip n            # result type
    g.wr "Math.trunc("
    gx g, n
    g.wr " / "
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc binPlain(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op lhs rhs)` (comparisons / logic): no type operand.
  n.into:
    g.wr "("
    gx g, n
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc unTyped(g: var JSGen; n: var Cursor; opr: string) =
  n.into:
    skip n            # type
    g.wr "("
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc unPlain(g: var JSGen; n: var Cursor; opr: string) =
  n.into:
    g.wr "("
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc genCall(g: var JSGen; n: var Cursor) =
  n.into:
    gx g, n           # callee
    g.wr "("
    var i = 0
    while n.hasMore:
      if i > 0: g.wr ", "
      gx g, n
      inc i
    g.wr ")"

proc captureExpr(g: var JSGen; n: var Cursor): string =
  ## Generate the expression at `n` into a string instead of the main buffer,
  ## advancing the cursor. Used where an emitted operand must be referenced more
  ## than once (a fat-pointer deref `p[0][p[1]]`, pointer-equality components).
  let start = g.code.len
  gx g, n
  result = g.code[start ..< g.code.len]
  g.code.setLen start

# genAddrOf / genDeref / genEq are defined below, after the buffer-layout helpers
# they depend on (arrayElemInfo, isBufferObjectType, pointeeAk), just before `gx`.

# ── buffer model (Typed-Array linear memory, Araq's M2 direction) ────────────
# A Nim-native object is bytes in linear memory: construction is `allocFixed` +
# typed stores at the field byte-offsets `jslayout` computes, field access is a
# typed load. This path is taken only when the object type has a resolvable
# layout (a declared `object`); JS-interop objects and as-yet-undeclared types
# stay on the legacy JS-object / fat-pointer mapping below.

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

proc constructObjectInto(g: var JSGen; dest: string; n: var Cursor) =
  ## Emit the stores that build an `(oconstr Type (kv f v) …)` into the storage at
  ## `dest` (already allocated by the caller): one `mem.setX(dest + off, v)` per
  ## field at its computed offset.
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
          g.nl()
          if ak == akAggregate:
            # a nested aggregate field: copy the value's bytes in.
            g.wr "mem.copy(" & dest & " + " & $off & ", "
            g.gx n
            g.wr ", " & $typeLayout(g.m, ftyp).size & ");"
          else:
            let (_, st) = accessors(ak)
            g.wr "mem." & st & "(" & dest & " + " & $off & ", "
            g.gx n              # field value
            g.wr ");"
          while n.hasMore: skip n
      else:
        skip n

proc dotObjType(g: var JSGen; base: Cursor): (bool, Cursor) =
  ## Resolve the object type of a `(dot BASE field)` base for buffer routing.
  ## Two IR shapes carry a resolvable object type:
  ##   * a Symbol local of object type          — its value IS the object's offset;
  ##   * `(deref sym)` where `sym : ptr Object`  — `sym`'s value is the offset
  ##     (deref of a pointer-to-aggregate is the offset itself).
  ## In both cases the caller still renders the base offset with `captureExpr`
  ## (which yields the operand for the deref shape), so only the type differs.
  ## Returns `(false, base)` for anything else (legacy JS-object / fat-pointer).
  result = (false, base)
  var b = base
  if b.kind == Symbol and g.localTypes.hasKey(b.symId):
    let t = g.localTypes[b.symId]
    if g.isBufferObjectType(t): result = (true, t)
  elif b.exprKind == DerefC:
    b.into:
      if b.kind == Symbol and g.localTypes.hasKey(b.symId):
        let t = g.localTypes[b.symId]
        if t.typeKind in {PtrT, AptrT}:
          var inner = t
          inner.into:
            if g.isBufferObjectType(inner): result = (true, inner)
            while inner.hasMore: skip inner
      while b.hasMore: skip b

proc bufferFieldOf(g: var JSGen; dotNode: Cursor): (bool, Cursor) =
  ## Non-consuming peek: is `dotNode` a `(dot base field …)` whose base resolves to
  ## a declared object type (by value or through a pointer)? Returns
  ## `(true, objectType)`. Used to route a `(dot …)` assignment target to a typed
  ## store instead of `lhs = rhs`.
  result = (false, dotNode)
  if dotNode.exprKind != DotC: return
  var probe = dotNode
  probe.into:
    result = g.dotObjType(probe)
    while probe.hasMore: skip probe

proc takeBufferField(g: var JSGen; n: var Cursor; oty: Cursor):
    tuple[base: string, off: int64, ak: AccessKind, fsize: int64] =
  ## Consume a `(dot base field …)` lvalue at `n`, returning the base offset
  ## expression and the field's offset / access kind / size.
  result = (base: "", off: 0'i64, ak: akAggregate, fsize: 0'i64)
  n.into:
    result.base = g.captureExpr n
    let fsym = n.symId; inc n
    for f in objectFields(g.m, oty):
      if f.sym == fsym:
        result.off = f.offset
        result.ak = accessOf(g.m, f.typ)
        result.fsize = typeLayout(g.m, f.typ).size
    while n.hasMore: skip n

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
  ## `(isArray, elementStride, elementAccessKind)` for an array type. The stride
  ## is the element size rounded up to its alignment (C array layout).
  let t = g.resolveType(arrTyp)
  if t.typeKind != ArrayT: return (false, 0'i64, akAggregate)
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

template withPointee(g: var JSGen; operand: Cursor; pointee, body: untyped) =
  ## Run `body` with `pointee` bound to the statically-known pointee type of a
  ## pointer-valued operand, when it can be determined:
  ##   * a Symbol local typed `(ptr T)`/`(aptr T)`   -> T;
  ##   * a Symbol loosely typed as its pointee `T`    -> T;
  ##   * a `(cast (ptr T) x)` / `(conv (ptr T) x)`    -> T (the explicit cast type,
  ##     the shape system.nim uses for `deref(cast (ptr u8) (addr …))`).
  ## `body` is skipped when the pointee is not statically knowable (caller keeps
  ## its default).
  block:
    var op = operand
    if op.kind == Symbol and g.localTypes.hasKey(op.symId):
      let t = g.localTypes[op.symId]
      if t.typeKind in {PtrT, AptrT}:
        var pointee = t
        pointee.into:
          body
          while pointee.hasMore: skip pointee
      else:
        var pointee = t
        body
    elif op.exprKind in {CastC, ConvC}:
      op.into:
        let ty = op
        if ty.typeKind in {PtrT, AptrT}:
          var pointee = ty
          pointee.into:
            body
            while pointee.hasMore: skip pointee
        while op.hasMore: skip op

proc pointeeAk(g: var JSGen; operand: Cursor): AccessKind =
  ## Access kind of the pointee for a `(deref p)` / pointer store — see `withPointee`.
  result = akI64
  g.withPointee(operand, pointee):
    result = accessOf(g.m, pointee)

proc pointeeInfo(g: var JSGen; operand: Cursor): (AccessKind, int64) =
  ## `(accessKind, byteSize)` of the pointee for a pointer operand — used for
  ## `(pat p i)` element arithmetic (`p + i*size`) and its load/store width.
  result = (akU8, 1'i64)     # default: byte pointer (cstring-like)
  g.withPointee(operand, pointee):
    result = (accessOf(g.m, pointee), typeLayout(g.m, pointee).size)

proc genAddrOf(g: var JSGen; n: var Cursor) =
  ## `(addr LOC)` -> an integer BYTE OFFSET into the buffer (a Nim-native
  ## pointer). A field's address is `base + off`, an element's is `base + i*stride`,
  ## an address-taken scalar local's is its own slot offset. `(addr (deref p))`
  ## cancels to `p`. Fat pointers `[base,key]` are reserved for the future importjs
  ## interop layer (the legacy fallback branch, hit only for undeclared types).
  case n.exprKind
  of DerefC:
    n.into:
      gx g, n                       # addr(deref p) = p
      while n.hasMore: skip n
  of DotC:
    n.into:
      let (isBuf, oty) = g.dotObjType(n)
      let base = g.captureExpr n
      let fsym = n.symId; inc n
      if isBuf:
        var off = 0'i64
        for f in objectFields(g.m, oty):
          if f.sym == fsym: off = f.offset
        g.wr "(" & base & " + " & $off & ")"
      else:
        g.wr "[" & base & ", " & jsString(g.fieldName(fsym)) & "]"
      while n.hasMore: skip n
  of AtC:
    n.into:
      var aty = n
      var isArr = false
      if n.kind == Symbol and g.localTypes.hasKey(n.symId):
        aty = g.localTypes[n.symId]; isArr = g.isBufferArray(aty)
      let base = g.captureExpr n
      let idx = g.captureExpr n
      if isArr:
        let (_, stride, _) = g.arrayElemInfo(aty)
        g.wr "(" & base & " + (" & idx & ") * " & $stride & ")"
      else:
        g.wr "[" & base & ", " & idx & "]"
      while n.hasMore: skip n
  of PatC:
    # `addr (pat p i)` -> pointer arithmetic `p + i*stride` (stride = pointee size).
    n.into:
      let (_, stride) = g.pointeeInfo(n)
      let base = g.captureExpr n
      let idx = g.captureExpr n
      g.wr "(" & base & " + (" & idx & ") * " & $stride & ")"
      while n.hasMore: skip n
  of NoExpr:
    if n.kind == Symbol:
      # an address-taken scalar local: its slot's byte offset IS the variable.
      g.wr g.name(n.symId); inc n
    else:
      g.todo("addr-of-location", n)
  else:
    g.todo("addr-of-location", n)

proc genDeref(g: var JSGen; n: var Cursor) =
  ## `(deref p)` -> a typed load at byte offset `p`, the pointee width taken from
  ## `p`'s type. `(deref (addr LOC))` cancels to reading LOC directly.
  if n.exprKind == AddrC:
    n.into:
      gx g, n
      while n.hasMore: skip n
  else:
    let ak = g.pointeeAk(n)
    let p = g.captureExpr n
    if ak == akAggregate:
      g.wr p                       # deref of a pointer-to-aggregate: the offset itself
    else:
      let (ld, _) = accessors(ak)
      g.wr "mem." & ld & "(" & p & ")"

proc genEq(g: var JSGen; n: var Cursor; negate: bool) =
  ## `(eq a b)` / `(neq a b)`. Pointers are now integer byte offsets, so a plain
  ## strict comparison is correct for addresses too (no fat-pointer component
  ## compare needed).
  n.into:
    let a = g.captureExpr n
    let b = g.captureExpr n
    g.wr "(" & a & (if negate: " !== " else: " === ") & b & ")"
    while n.hasMore: skip n

proc constructArrayInto(g: var JSGen; dest: string; n: var Cursor; arrTyp: Cursor) =
  ## Emit the stores that build an `(aconstr T e0 e1 …)` into the storage at
  ## `dest`: one `mem.setX(dest + i*stride, ei)` per element.
  let (_, stride, ak) = g.arrayElemInfo(arrTyp)
  n.into:
    skip n                # array type operand
    var i = 0'i64
    while n.hasMore:
      g.nl()
      if ak == akAggregate:
        g.wr "mem.copy(" & dest & " + " & $(i * stride) & ", "
        g.gx n
        g.wr ", " & $stride & ");"
      else:
        let (_, st) = accessors(ak)
        g.wr "mem." & st & "(" & dest & " + " & $(i * stride) & ", "
        g.gx n
        g.wr ");"
      inc i

proc gx(g: var JSGen; n: var Cursor) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      g.wr $intVal(n); inc n
    of UIntLit:
      g.wr $uintVal(n); inc n
    of FloatLit:
      g.wr $floatVal(n); inc n
    of CharLit:
      g.wr $ord(n.charLit); inc n
    of StrLit:
      g.wr jsString(g.m.pool.strings[strId(n)]); inc n
    of Symbol:
      # an address-taken SCALAR local lives in a buffer slot (so it has a real
      # byte address); read it with a typed load. An aggregate local already holds
      # its offset, and ordinary locals stay JS registers — both read by name.
      let nm = g.name(n.symId)
      let ak = (if n.symId in g.boxed and g.localTypes.hasKey(n.symId): accessOf(g.m, g.localTypes[n.symId])
                elif n.symId in g.boxed: akI64
                else: akAggregate)   # sentinel meaning "emit name"
      if n.symId in g.boxed and ak != akAggregate:
        let (ld, _) = accessors(ak)
        g.wr "mem." & ld & "(" & nm & ")"
      else:
        g.wr nm
      inc n
    of DotToken:
      g.wr "undefined"; inc n
    else:
      g.todo("expr:" & $n.kind, n)
  of TrueC: g.wr "true"; skip n
  of FalseC: g.wr "false"; skip n
  of NilC: g.wr "0"; skip n   # a nil pointer is byte offset 0 (reserved as null)
  of InfC: g.wr "Infinity"; skip n
  of NegInfC: g.wr "(-Infinity)"; skip n
  of NanC: g.wr "NaN"; skip n
  of OvfC:
    # The overflow flag (see `KeepovfS`). JS numbers are doubles with no 64-bit
    # overflow trap, so nothing sets it — a checked op never reports overflow.
    g.wr "false"; skip n
  of CallC: genCall g, n
  of AddC: binTyped g, n, " + "
  of SubC: binTyped g, n, " - "
  of MulC: binTyped g, n, " * "
  of DivC: intDiv g, n
  of ModC: binTyped g, n, " % "
  of ShlC: binTyped g, n, " << "
  of ShrC: binTyped g, n, " >> "
  of BitandC: binTyped g, n, " & "
  of BitorC: binTyped g, n, " | "
  of BitxorC: binTyped g, n, " ^ "
  of BitnotC: unTyped g, n, "~"
  of NegC: unTyped g, n, "-"
  of AndC: binPlain g, n, " && "
  of OrC: binPlain g, n, " || "
  of NotC: unPlain g, n, "!"
  of EqC: genEq g, n, false
  of NeqC: genEq g, n, true
  of LeC: binPlain g, n, " <= "
  of LtC: binPlain g, n, " < "
  of CastC, ConvC:
    # JS is untyped: a conversion/cast is the inner value (skip the type).
    n.into:
      skip n
      gx g, n
      while n.hasMore: skip n
  of ParC:
    n.into:
      g.wr "("
      gx g, n
      g.wr ")"
      while n.hasMore: skip n
  of SufC:
    # literal-with-suffix: emit the value, drop the suffix annotation.
    n.into:
      gx g, n
      while n.hasMore: skip n
  of OconstrC:
    # `(oconstr Type (kv f v) …)`. For a Nim-native object this materialises into
    # a fresh buffer allocation (an IIFE returning the offset), so an inline
    # object value is a pointer just like a stored one. Undeclared types fall back
    # to the legacy JS object literal `{field: value, …}`.
    var oty = n
    var isBuf = false
    block:
      var probe = n
      probe.into:
        oty = probe
        isBuf = g.isBufferObjectType(oty)
        while probe.hasMore: skip probe
    if isBuf:
      let sz = typeLayout(g.m, oty).size
      g.wr "(() => { const _o = allocFixed(" & $sz & ");"
      g.constructObjectInto("_o", n)
      g.wr " return _o; })()"
    else:
      n.into:
        skip n            # object type
        g.wr "{"
        var i = 0
        while n.hasMore:
          if n.substructureKind == KvU:
            if i > 0: g.wr ", "
            n.into:
              g.wr g.fieldName(n.symId); inc n   # field name (Symbol) as key
              g.wr ": "
              g.gx n                         # value
              while n.hasMore: skip n        # optional inheritance depth
            inc i
          else:
            skip n
        g.wr "}"
  of AconstrC:
    # `(aconstr Type e0 e1 …)`. A Nim-native array materialises into a fresh
    # buffer allocation (IIFE -> offset); otherwise a legacy JS array literal.
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
      g.wr "(() => { const _a = allocFixed(" & $sz & ");"
      g.constructArrayInto("_a", n, aty)
      g.wr " return _a; })()"
    else:
      n.into:
        skip n            # element/array type
        g.wr "["
        var i = 0
        while n.hasMore:
          if i > 0: g.wr ", "
          g.gx n
          inc i
        g.wr "]"
  of AddrC:
    # `(addr LOC)` -> a fat pointer `[base, key]` (see `genAddrOf`).
    n.into:
      g.genAddrOf n
      while n.hasMore: skip n
  of DerefC:
    # `(deref p)` -> `p[0][p[1]]` (see `genDeref`).
    n.into:
      g.genDeref n
      while n.hasMore: skip n
  of DotC:
    # `(dot obj field …)`. For a Nim-native object (a local of declared object
    # type) this is a typed load at the field's byte offset in linear memory;
    # otherwise the legacy `obj.field` mapping (JS-interop / undeclared types).
    n.into:
      let (useBuffer, oty) = g.dotObjType(n)
      if useBuffer:
        let base = g.captureExpr n         # object base offset (advances past it)
        let fsym = n.symId; inc n
        var off = 0'i64
        var ftyp = n
        var found = false
        for f in objectFields(g.m, oty):
          if f.sym == fsym: off = f.offset; ftyp = f.typ; found = true
        let ak = (if found: accessOf(g.m, ftyp) else: akAggregate)
        if ak == akAggregate:
          g.wr "(" & base & " + " & $off & ")"   # sub-object: a pointer, not a load
        else:
          let (ld, _) = accessors(ak)
          g.wr "mem." & ld & "(" & base & " + " & $off & ")"
      else:
        g.gx n            # object
        g.wr "."
        g.wr g.fieldName(n.symId); inc n   # field name (Symbol)
      while n.hasMore: skip n        # inheritance depth / access token
  of AtC:
    # `(at arr i)` on a Nim-native array -> typed load at `base + i*stride` in
    # linear memory; otherwise (undeclared element type) the legacy `arr[i]`.
    n.into:
      var isArr = false
      var aty = n
      if n.kind == Symbol and g.localTypes.hasKey(n.symId):
        aty = g.localTypes[n.symId]
        isArr = g.isBufferArray(aty)
      if isArr:
        let (_, stride, ak) = g.arrayElemInfo(aty)
        let base = g.captureExpr n
        let idx = g.captureExpr n
        if ak == akAggregate:
          g.wr "(" & base & " + (" & idx & ") * " & $stride & ")"
        else:
          let (ld, _) = accessors(ak)
          g.wr "mem." & ld & "(" & base & " + (" & idx & ") * " & $stride & ")"
      else:
        g.gx n; g.wr "["; g.gx n; g.wr "]"
      while n.hasMore: skip n
  of PatC:
    # `(pat p i)` pointer indexing -> typed load at `p + i*stride` (byte pointer).
    n.into:
      let (ak, stride) = g.pointeeInfo(n)
      let base = g.captureExpr n
      let idx = g.captureExpr n
      if ak == akAggregate:
        g.wr "(" & base & " + (" & idx & ") * " & $stride & ")"
      else:
        let (ld, _) = accessors(ak)
        g.wr "mem." & ld & "(" & base & " + (" & idx & ") * " & $stride & ")"
      while n.hasMore: skip n
  of SizeofC:
    # `(sizeof T)` -> the byte size the layout pass computes.
    n.into:
      g.wr $typeLayout(g.m, n).size
      while n.hasMore: skip n
  else:
    g.todo("expr:" & $n.exprKind, n)

# ── statements ───────────────────────────────────────────────────────────────

proc gs(g: var JSGen; n: var Cursor)

proc genBlock(g: var JSGen; n: var Cursor) =
  ## emit a `(stmts ...)` body as a brace-delimited JS block.
  g.wr "{"
  inc g.indent
  if n.stmtKind in {StmtsS, ScopeS}:
    n.loopInto:
      g.gs n
  else:
    g.gs n
  dec g.indent
  g.nl()
  g.wr "}"

proc genVar(g: var JSGen; n: var Cursor) =
  var d = takeVarDecl(n)
  if g.isImportc(d.name.symId): return   # external global: provided by the runtime
  g.localTypes[d.name.symId] = d.typ     # remember the type for buffer-model access
  g.nl()
  let nm = g.name(d.name.symId)
  if g.isBufferAggregate(d.typ):
    # Nim-native aggregate (object or array): storage in linear memory. Allocate
    # its bytes, then construct the (a)constr into it in place, or copy another
    # aggregate value. Its address is simply its offset (no boxing needed).
    let lay = typeLayout(g.m, d.typ)
    g.wr "let " & nm & " = allocFixed(" & $lay.size & ");"
    if d.value.kind != DotToken:
      var v = d.value
      if v.exprKind == OconstrC:
        g.constructObjectInto(nm, v)
      elif v.exprKind == AconstrC:
        g.constructArrayInto(nm, v, d.typ)
      else:
        g.nl(); g.wr "mem.copy(" & nm & ", "
        g.gx v
        g.wr ", " & $lay.size & ");"
    return
  if d.name.symId in g.boxed:
    # address-taken scalar local: spill to a buffer slot so it has a real byte
    # address (its variable holds the offset). Reads/writes go through mem.
    let lay = typeLayout(g.m, d.typ)
    g.wr "let " & nm & " = allocFixed(" & $lay.size & ");"
    if d.value.kind != DotToken:
      let (_, st) = accessors(accessOf(g.m, d.typ))
      g.nl(); g.wr "mem." & st & "(" & nm & ", "
      var v = d.value
      g.gx v
      g.wr ");"
  else:
    g.wr "let " & nm
    if d.value.kind != DotToken:
      g.wr " = "
      var v = d.value
      g.gx v
    g.wr ";"

proc genIf(g: var JSGen; n: var Cursor) =
  var first = true
  n.loopInto:
    case n.substructureKind
    of ElifU:
      g.nl()
      g.wr (if first: "if (" else: "else if (")
      n.into:
        g.gx n          # condition
        g.wr ") "
        g.genBlock n    # body
        while n.hasMore: skip n
      first = false
    of ElseU:
      g.nl()
      g.wr "else "
      n.into:
        g.genBlock n
        while n.hasMore: skip n
    else:
      g.todo("if-branch", n)

proc genWhile(g: var JSGen; n: var Cursor) =
  n.into:
    g.nl()
    g.wr "while ("
    g.gx n              # condition
    g.wr ") "
    g.genBlock n        # body
    while n.hasMore: skip n

proc genCase(g: var JSGen; n: var Cursor) =
  n.into:
    g.nl()
    g.wr "switch ("
    g.gx n              # selector
    g.wr ") {"
    inc g.indent
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          # (of (ranges v1 v2 ...) body)
          if n.substructureKind == RangesU:
            n.loopInto:
              g.nl(); g.wr "case "; g.gx n; g.wr ":"
          else:
            g.nl(); g.wr "case "; g.gx n; g.wr ":"
          inc g.indent
          g.nl(); g.genBlock n
          g.nl(); g.wr "break;"
          dec g.indent
          while n.hasMore: skip n
      of ElseU:
        n.into:
          g.nl(); g.wr "default:"
          inc g.indent
          g.nl(); g.genBlock n
          g.nl(); g.wr "break;"
          dec g.indent
          while n.hasMore: skip n
      else:
        g.todo("case-branch", n)
    dec g.indent
    g.nl(); g.wr "}"

proc genLvalueStore(g: var JSGen; lval: Cursor; value: string) =
  ## Store the pre-rendered `value` into the lvalue, dispatching on the unified
  ## byte-pointer model: `(deref p)` -> typed store at offset `p`; a buffer object
  ## field or array element -> typed store at its offset; an address-taken scalar
  ## local -> store at its slot; anything else -> a plain `lhs = value`.
  var n = lval
  case n.exprKind
  of DerefC:
    n.into:
      if n.exprKind == AddrC:
        n.into:
          g.genLvalueStore(n, value)      # (deref (addr loc)) = v  ->  store loc
          while n.hasMore: skip n
      else:
        let ak = g.pointeeAk(n)
        let p = g.captureExpr n
        let (_, st) = accessors(ak)
        g.wr "mem." & st & "(" & p & ", " & value & ");"
      while n.hasMore: skip n
  of DotC:
    let (isBuf, oty) = g.bufferFieldOf(n)
    if isBuf:
      var nn = n
      let fld = g.takeBufferField(nn, oty)
      if fld.ak == akAggregate:
        g.wr "mem.copy(" & fld.base & " + " & $fld.off & ", " & value & ", " & $fld.fsize & ");"
      else:
        let (_, st) = accessors(fld.ak)
        g.wr "mem." & st & "(" & fld.base & " + " & $fld.off & ", " & value & ");"
    else:
      var nn = n
      g.gx nn; g.wr " = " & value & ";"
  of AtC:
    var isArr = false
    var aty = n
    var probe = n
    probe.into:
      if probe.kind == Symbol and g.localTypes.hasKey(probe.symId):
        aty = g.localTypes[probe.symId]; isArr = g.isBufferArray(aty)
      while probe.hasMore: skip probe
    if isArr:
      let (_, stride, ak) = g.arrayElemInfo(aty)
      var nn = n
      nn.into:
        let base = g.captureExpr nn
        let idx = g.captureExpr nn
        if ak == akAggregate:
          g.wr "mem.copy(" & base & " + (" & idx & ") * " & $stride & ", " & value & ", " & $stride & ");"
        else:
          let (_, st) = accessors(ak)
          g.wr "mem." & st & "(" & base & " + (" & idx & ") * " & $stride & ", " & value & ");"
        while nn.hasMore: skip nn
    else:
      var nn = n
      g.gx nn; g.wr " = " & value & ";"
  of NoExpr:
    let ak = (if n.kind == Symbol and n.symId in g.boxed and g.localTypes.hasKey(n.symId):
                accessOf(g.m, g.localTypes[n.symId]) else: akAggregate)
    if n.kind == Symbol and n.symId in g.boxed and ak != akAggregate:
      let (_, st) = accessors(ak)
      g.wr "mem." & st & "(" & g.name(n.symId) & ", " & value & ");"
    else:
      var nn = n
      g.gx nn; g.wr " = " & value & ";"
  else:
    var nn = n
    g.gx nn; g.wr " = " & value & ";"

proc gs(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken: inc n
    else: g.todo("stmt:" & $n.kind, n)
  of StmtsS:
    n.loopInto: g.gs n
  of ScopeS:
    g.nl(); g.genBlock n
  of VarS, GvarS, TvarS, ConstS:
    g.genVar n
  of AsgnS:
    # `(asgn lvalue value)` — dispatched by `genLvalueStore` (typed store for a
    # deref / buffer field / array element / spilled scalar; plain `=` otherwise).
    n.into:
      let lval = n
      skip n
      let value = g.captureExpr n
      g.nl()
      g.genLvalueStore(lval, value)
      while n.hasMore: skip n
  of StoreS:
    # `(store value lvalue)` — operands reversed relative to `asgn`.
    n.into:
      let value = g.captureExpr n
      let lval = n
      g.nl()
      g.genLvalueStore(lval, value)
      while n.hasMore: skip n
  of KeepovfS:
    # `(keepovf (op TYPE lhs rhs) dest)` computes the arithmetic and stores it
    # into `dest`. JS has no 64-bit overflow trap, so this is just the store.
    n.into:
      let arith = g.captureExpr n
      let lval = n
      g.nl()
      g.genLvalueStore(lval, arith)
      while n.hasMore: skip n
  of CallS:
    g.nl()
    g.genCall n
    g.wr ";"
  of RetS:
    g.nl()
    n.into:
      if n.kind != DotToken:
        g.wr "return "
        g.gx n
        g.wr ";"
      else:
        g.wr "return;"; inc n
      while n.hasMore: skip n
  of DiscardS:
    n.into:
      g.nl()
      g.gx n
      g.wr ";"
      while n.hasMore: skip n
  of IfS: g.genIf n
  of WhileS: g.genWhile n
  of CaseS: g.genCase n
  of BreakS:
    g.nl(); g.wr "break;"
    skip n
  of LabS:
    # A goto-target label. Vestigial under structured control flow (no matching
    # `jmp`); JS has no `goto`, so emit nothing. A real `jmp` still surfaces as
    # an unsupported node, making any genuine goto use visible.
    skip n
  of RaiseS:
    g.nl()
    n.into:
      if n.kind != DotToken:
        g.wr "throw "
        g.gx n
        g.wr ";"
      else:
        g.wr "throw new Error();"; inc n
      while n.hasMore: skip n
  else:
    g.nl(); g.todo("stmt:" & $n.stmtKind, n)

# ── declarations / module ────────────────────────────────────────────────────

proc genProc(g: var JSGen; n: var Cursor) =
  var prc = takeProcDecl(n)
  if g.isImportc(prc.name.symId): return   # external proc: provided by the runtime
  g.nl()
  g.wr "function " & g.name(prc.name.symId) & "("
  if prc.params.kind != DotToken:
    var p = prc.params
    var i = 0
    p.loopInto:
      var d = takeParamDecl(p)
      g.localTypes[d.name.symId] = d.typ   # remember param types for buffer access
      if i > 0: g.wr ", "
      g.wr g.name(d.name.symId)
      inc i
  g.wr ") {"
  inc g.indent
  # Prologue: a value parameter whose address is taken must be spilled to a
  # buffer slot so a pointer to it is a real byte offset. Re-bind the name to its
  # slot at entry and store the incoming value; all uses inside the body then go
  # through `mem` (see `gx`). A pointer parameter already arrives as an offset.
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
        g.nl(); g.wr "const " & nm & "_v = " & nm & ";"
        g.nl(); g.wr nm & " = allocFixed(" & $sz & ");"
        g.nl(); g.wr "mem." & st & "(" & nm & ", " & nm & "_v);"
  var body = prc.body
  if body.stmtKind in {StmtsS, ScopeS}:
    body.loopInto: g.gs body
  else:
    g.gs body
  dec g.indent
  g.nl()
  g.wr "}"
  g.nl()

proc genToplevel(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of ProcS: g.genProc n
  of VarS, GvarS, TvarS, ConstS: g.genVar n
  of TypeS: skip n               # no types in JS
  of EmitS:
    # raw passthrough: string literals are emitted verbatim (JS injection).
    g.nl()
    n.loopInto:
      if n.kind == StrLit:
        g.wr g.m.pool.strings[strId(n)]; inc n
      else:
        g.gx n
  of StmtsS:
    n.loopInto: g.genToplevel n
  of CallS, AsgnS, StoreS, IfS, WhileS, CaseS, DiscardS, ScopeS:
    g.gs n
  else:
    g.nl(); g.todo("toplevel:" & $n.stmtKind, n)

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
  # The body is generated into `g.code`; the header and any runtime helpers are
  # prepended afterwards, since whether the module needs `nimPtrEq` is only known
  # once generation has run.
  var n = beginRead(g.m.src)
  if n.stmtKind == StmtsS:
    n.loopInto: g.genToplevel n
  else:
    g.todo("root", n)
  g.code.add "\n"
  var output = "// generated by lengc (js backend) from " & extractFilename(inp) & "\n"
  output.add "\"use strict\";\n"
  if g.usesPtrEq: output.add ptrEqHelper
  output.add g.code
  if g.todos > 0:
    stdout.writeLine "[lengc js] " & inp & ": " & $g.todos & " unsupported node(s) emitted as /*TODO*/"
  if vfsExists(outp) and vfsRead(outp) == output:
    discard "unchanged"
  else:
    vfsWrite outp, output

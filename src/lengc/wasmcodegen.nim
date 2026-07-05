#
#
#           Leng Compiler — WebAssembly backend
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## WebAssembly backend for Leng — a fourth codegen over the same Leng IR the C
## (`codegen`), LLVM (`llvmcodegen`) and JavaScript (`jscodegen`) backends consume.
##
## **It is additive, not a rewrite.** WASM linear memory *is* the linear-memory
## model the JS backend (PR #2043) already built — one byte-addressable buffer, a
## pointer is an integer offset — so this backend reuses `jslayout` (the C-ABI
## byte-offset/`AccessKind` computer) **verbatim**; only the emitted instruction
## differs (`i32.load` here vs. `HEAP32[p>>2]` in `jscodegen`). Where JS builds a
## `jsnif` text tree, this builds a `WasmModule` (`wasmenc`) and serializes it to
## `.wasm` bytecode — the same split `llvmcodegen`/`llvmserializer` use.
##
## Scope of this first slice (the rest reuses the same `jslayout` core):
##   - scalar `i32`/`i64`/`f32`/`f64` params, locals, arithmetic (WASM ints wrap
##     natively — the 32-bit-wrapping fix `jscodegen` needed is *free* here),
##     comparisons, bit ops, int/float conversions;
##   - structured control flow (`if`/`while`/`break`) mapped to WASM
##     `block`/`loop`/`if` with relative `br` depths — the `.c.nif` is already
##     structured (no raw `goto`), so no relooper is needed;
##   - `call`/recursion; linear-memory field/array/deref loads & stores at
##     `jslayout` byte offsets (proving the layout core is shared).
##
## Entry model: a WASM module is instantiated by a host that calls its exports, so
## the backend emits a function per `exportc` proc (and its same-module callees)
## and exports it — it does *not* emit the C `main(argc,argv,envp)` shim, whose
## job (argv wiring, cross-module init) is a C-runtime convention. Module-level
## globals, `string`/`seq` heap data and `echo` (which need a start function and
## the ported allocator) are the next increment, on this same `jslayout` core.

import std / [tables, sets, syncio]

import ".." / lib / nifcoreparse   # re-exports nifcore (Cursor, into, skip, intVal, ...)
import ".." / lib / nifcdecl        # stmtKind/exprKind/substructureKind + decl readers
import noptions
import nifmodules                   # MainModule + load + scope stack
import typenav                      # getNominalType — the shared Leng type navigator
import jslayout                     # C-ABI layout: typeLayout/objectFields/accessOf — REUSED
import wasmenc                      # the WASM binary model + encoder

type
  WasmGenFlag* = enum
    gfMainModule

  ProcInfo = object
    sym: SymId
    extern: string          ## exportc name ("" if none)
    isImport: bool
    name, params, returnType, body: Cursor
    typeIdx: uint32
    funcIdx: uint32
    hasResult: bool
    resultVT: ValType

  WasmGen = object
    m: MainModule
    md: WasmModule
    flags: set[WasmGenFlag]
    todos: int
    procs: seq[ProcInfo]
    procBySym: Table[SymId, int]     ## symId -> index into `procs`
    # per-function state:
    body: seq[byte]
    locals: seq[ValType]             ## additional locals (indices numParams..)
    localIdx: Table[SymId, uint32]
    localVT: Table[SymId, ValType]
    numParams: uint32
    curResultVT: ValType
    curHasResult: bool
    nframes: int                     ## current control-frame nesting (for `br` depth)
    breakBlocks: seq[int]            ## `nframes` value at each enclosing loop's break-block
    labelBlocks: Table[SymId, int]   ## goto label -> the `nframes` level of its wrapping block

# ── type mapping ──────────────────────────────────────────────────────────────

proc vt(ak: AccessKind): ValType =
  ## The WASM value type a scalar access kind lives in. Sub-32-bit ints ride in an
  ## `i32`; pointers are `i32` (the `--bits:32` target); explicit 64-bit is `i64`.
  case ak
  of akI8, akI16, akI32, akU8, akU16, akU32, akPtr: vI32
  of akI64, akU64: vI64
  of akF32: vF32
  of akF64: vF64
  of akAggregate: vI32          ## an aggregate is handled by its address (an i32)

proc isVoidType(t: Cursor): bool =
  t.kind == DotToken or t.typeKind == VoidT

proc exprAk(g: var WasmGen; n: Cursor): AccessKind =
  ## The access kind (hence WASM type / signedness) of an expression, via the
  ## shared `typenav`. Locals we typed at declaration are answered from that map.
  if n.kind == Symbol and g.localVT.hasKey(n.symId):
    case g.localVT[n.symId]
    of vI32: return akI32
    of vI64: return akI64
    of vF32: return akF32
    of vF64: return akF64
  let t = getNominalType(g.m, n)
  if t.kind == TagLit and t.typeKind == NoType: akI32   # typenav (err) sentinel
  else: accessOf(g.m, t)

# ── forward decls ─────────────────────────────────────────────────────────────

proc genExpr(g: var WasmGen; n: var Cursor; want: ValType)
proc genStmt(g: var WasmGen; n: var Cursor)
proc genStmtList(g: var WasmGen; n: var Cursor)
proc labelSym(n: Cursor): SymId

proc todo(g: var WasmGen; what: string) =
  inc g.todos
  when defined(wasmDebugTodo): stderr.writeLine "  TODO: " & what
  # A gap must still leave a stack-valid stream: emit `unreachable` (0x00), which
  # is stack-polymorphic, so validation passes and the untranslated path traps.
  g.body.op 0x00'u8

# ── expressions ───────────────────────────────────────────────────────────────

proc constLit(g: var WasmGen; n: Cursor; want: ValType) =
  ## Emit a literal coerced to the wanted WASM type.
  case want
  of vF64:
    if n.kind == FloatLit: g.body.f64Const floatVal(n)
    elif n.kind == IntLit: g.body.f64Const float64(intVal(n))
    else: g.body.f64Const 0.0
  of vF32:
    if n.kind == FloatLit: g.body.f32Const float32(floatVal(n))
    elif n.kind == IntLit: g.body.f32Const float32(intVal(n))
    else: g.body.f32Const 0.0
  of vI64:
    if n.kind == IntLit: g.body.i64Const intVal(n)
    elif n.kind == UIntLit: g.body.i64Const cast[int64](uintVal(n))
    elif n.kind == CharLit: g.body.i64Const int64(ord(n.charLit))
    else: g.body.i64Const 0
  of vI32:
    if n.kind == IntLit: g.body.i32Const int32(intVal(n))
    elif n.kind == UIntLit: g.body.i32Const cast[int32](uint32(uintVal(n)))
    elif n.kind == CharLit: g.body.i32Const int32(ord(n.charLit))
    else: g.body.i32Const 0

proc binArith(g: var WasmGen; n: var Cursor; i32op, i64op, f64op: byte) =
  ## `(op TYPE a b)` — the type operand fixes the WASM class. Ints wrap natively.
  n.into:
    let ak = accessOf(g.m, n)
    let w = vt(ak)
    skip n                        # result type
    g.genExpr(n, w)
    g.genExpr(n, w)
    case w
    of vI32: g.body.op i32op
    of vI64: g.body.op i64op
    of vF64: g.body.op f64op
    of vF32: g.body.op f64op      # f32 arithmetic uses its own ops; slice1 rarely
    # narrow the i32 result back to the sub-32-bit type width (WASM wraps at 32).
    if w == vI32:
      case ak
      of akI8:  g.body.op 0xC0'u8            # i32.extend8_s
      of akI16: g.body.op 0xC1'u8            # i32.extend16_s
      of akU8:  g.body.i32Const 0xFF; g.body.op 0x71'u8   # & 0xFF
      of akU16: g.body.i32Const 0xFFFF; g.body.op 0x71'u8
      else: discard
    while n.hasMore: skip n

proc binDivMod(g: var WasmGen; n: var Cursor; signed, unsigned, i64s, i64u, fop: byte) =
  n.into:
    let ak = accessOf(g.m, n)
    let w = vt(ak)
    let uns = ak in {akU8, akU16, akU32, akU64, akPtr}
    skip n
    g.genExpr(n, w)
    g.genExpr(n, w)
    case w
    of vI32: g.body.op (if uns: unsigned else: signed)
    of vI64: g.body.op (if uns: i64u else: i64s)
    of vF64, vF32: g.body.op fop
    while n.hasMore: skip n

proc cmp(g: var WasmGen; n: var Cursor; i32s, i32u, i64s, i64u, fop: byte) =
  ## `(op a b)` comparison (no type operand). Signedness/width from operand type.
  n.into:
    let ak = g.exprAk(n)             # type of the first operand (read, not consumed)
    let w = vt(ak)
    let uns = ak in {akU8, akU16, akU32, akU64, akPtr, akAggregate}
    g.genExpr(n, w)
    g.genExpr(n, w)
    case w
    of vI32: g.body.op (if uns: i32u else: i32s)
    of vI64: g.body.op (if uns: i64u else: i64s)
    of vF64, vF32: g.body.op fop
    while n.hasMore: skip n

proc cmpEq(g: var WasmGen; n: var Cursor; i32op, i64op, fop: byte) =
  n.into:
    let w = vt(g.exprAk(n))
    g.genExpr(n, w)
    g.genExpr(n, w)
    case w
    of vI32: g.body.op i32op
    of vI64: g.body.op i64op
    of vF64, vF32: g.body.op fop
    while n.hasMore: skip n

proc genShortCircuit(g: var WasmGen; n: var Cursor; isAnd: bool) =
  ## `(and a b)`/`(or a b)` with short-circuit semantics via a value-typed `if`:
  ## `and` = `if a then b else 0`; `or` = `if a then 1 else b`.
  n.into:
    g.genExpr(n, vI32)                      # a  (leaves the branch condition)
    g.body.blockOp opIf, byte(ord(vI32))    # if (result i32)
    inc g.nframes
    if isAnd:
      g.genExpr(n, vI32)                    # then: b
      g.body.op opElse
      g.body.i32Const 0                     # else: 0
    else:
      g.body.i32Const 1                     # then: 1
      g.body.op opElse
      g.genExpr(n, vI32)                    # else: b
    g.body.op opEnd
    dec g.nframes
    while n.hasMore: skip n

proc loadOp(ak: AccessKind): byte =
  ## The WASM load opcode for a scalar access kind.
  case ak
  of akI8:  0x2C   # i32.load8_s
  of akU8:  0x2D   # i32.load8_u
  of akI16: 0x2E   # i32.load16_s
  of akU16: 0x2F   # i32.load16_u
  of akI32, akU32, akPtr: 0x28   # i32.load
  of akI64, akU64: 0x29          # i64.load
  of akF32: 0x2A
  of akF64: 0x2B
  of akAggregate: 0x28

proc storeOp(ak: AccessKind): byte =
  case ak
  of akI8, akU8: 0x3A            # i32.store8
  of akI16, akU16: 0x3B         # i32.store16
  of akI32, akU32, akPtr: 0x36  # i32.store
  of akI64, akU64: 0x37         # i64.store
  of akF32: 0x38
  of akF64: 0x39
  of akAggregate: 0x36

proc alignOf(ak: AccessKind): uint32 =
  ## Natural alignment log2 for the memarg hint.
  case ak
  of akI8, akU8: 0
  of akI16, akU16: 1
  of akI32, akU32, akF32, akPtr: 2
  of akI64, akU64, akF64: 3
  of akAggregate: 2

proc pointeeAk(g: var WasmGen; ptrExpr: Cursor): AccessKind =
  ## The access kind of `*p`: the pointee's access kind if `p` has pointer type,
  ## else the type's own (robustness for already-lowered addresses).
  var t = getNominalType(g.m, ptrExpr)
  if t.typeKind in {PtrT, AptrT}:
    var inner = t
    inner.into:
      result = accessOf(g.m, inner)
      while inner.hasMore: skip inner
  else:
    result = accessOf(g.m, t)

proc objTypeOf(g: var WasmGen; base: Cursor): Cursor =
  ## The object type reached through `base`: if `base` is a `ptr T`/`aptr T`, its
  ## pointee; otherwise the (aggregate) type itself.
  var t = getNominalType(g.m, base)
  result = t
  if t.typeKind in {PtrT, AptrT}:
    var inner = t
    inner.into:
      result = inner
      while inner.hasMore: skip inner

proc genFieldAddr(g: var WasmGen; n: var Cursor): AccessKind =
  ## Push the byte address of `(dot base field ...)` and return the field's access
  ## kind. The address is `base(address) + fieldOffset` — the SAME offset
  ## `jslayout` gives the JS backend, emitted as an `i32.add`.
  n.into:
    let objTy = g.objTypeOf(n)
    var probe = n
    skip probe
    let fsym = probe.symId
    var off = 0'i64
    var ak = akAggregate
    for f in objectFields(g.m, objTy):
      if f.sym == fsym: off = f.offset; ak = accessOf(g.m, f.typ)
    g.genExpr(n, vI32)              # base address (advances past base)
    inc n                          # skip field symbol
    if off != 0:
      g.body.i32Const int32(off)
      g.body.op 0x6A'u8            # i32.add
    while n.hasMore: skip n
    result = ak

proc genElemAddr(g: var WasmGen; n: var Cursor; ptrStride: bool): AccessKind =
  ## Push the byte address of `(at arr i)` / `(pat p i)` and return the element
  ## access kind. Address = base + i*stride, stride from `jslayout`.
  n.into:
    var stride = 0'i64
    var ak = akAggregate
    let baseTy = getNominalType(g.m, n)
    var elemTy = baseTy
    if baseTy.typeKind in {ArrayT, FlexarrayT}:
      var a = baseTy
      a.into:
        elemTy = a
        while a.hasMore: skip a
    elif baseTy.typeKind in {PtrT, AptrT}:
      var a = baseTy
      a.into:
        elemTy = a
        while a.hasMore: skip a
    let lay = typeLayout(g.m, elemTy)
    stride = (if lay.align <= 1: lay.size else: (lay.size + lay.align - 1) and not (lay.align - 1))
    ak = accessOf(g.m, elemTy)
    g.genExpr(n, vI32)             # base address
    g.genExpr(n, vI32)             # index
    if stride != 1:
      g.body.i32Const int32(stride)
      g.body.op 0x6C'u8           # i32.mul
    g.body.op 0x6A'u8             # i32.add
    while n.hasMore: skip n
  result = ak

proc genConv(g: var WasmGen; n: var Cursor) =
  ## `(conv TYPE x)` / `(cast TYPE x)` — width/representation change.
  n.into:
    let tgt = accessOf(g.m, n)
    skip n
    let src = g.exprAk(n)
    let sw = vt(src)
    let tw = vt(tgt)
    g.genExpr(n, sw)
    if sw == tw:
      # same WASM class: narrow if the target is a smaller int width
      case tgt
      of akI8:  g.body.op 0xC0'u8
      of akI16: g.body.op 0xC1'u8
      of akU8:  g.body.i32Const 0xFF; g.body.op 0x71'u8
      of akU16: g.body.i32Const 0xFFFF; g.body.op 0x71'u8
      else: discard
    else:
      let suns = src in {akU8, akU16, akU32, akU64, akPtr}
      if   sw == vI32 and tw == vI64: g.body.op (if suns: 0xAD'u8 else: 0xAC'u8)  # extend_i32
      elif sw == vI64 and tw == vI32: g.body.op 0xA7'u8                            # wrap_i64
      elif sw == vI32 and tw == vF64: g.body.op (if suns: 0xB8'u8 else: 0xB7'u8)  # convert_i32
      elif sw == vI64 and tw == vF64: g.body.op (if suns: 0xBA'u8 else: 0xB9'u8)  # convert_i64
      elif sw == vF64 and tw == vI32: g.body.op (if tgt in {akU8,akU16,akU32,akPtr}: 0xAB'u8 else: 0xAA'u8)
      elif sw == vF64 and tw == vI64: g.body.op (if tgt in {akU64}: 0xB1'u8 else: 0xB0'u8)
      elif sw == vF32 and tw == vF64: g.body.op 0xBB'u8                            # promote
      elif sw == vF64 and tw == vF32: g.body.op 0xB6'u8                            # demote
      elif sw == vI32 and tw == vF32: g.body.op (if suns: 0xB3'u8 else: 0xB2'u8)
      elif sw == vF32 and tw == vI32: g.body.op 0xA8'u8
      else: discard
    while n.hasMore: skip n

proc genCall(g: var WasmGen; n: var Cursor; wantResult: bool) =
  ## `(call callee args...)`. Direct call to a same-module defined proc. Args are
  ## coerced to the callee's declared param types.
  n.into:
    if n.kind == Symbol and g.procBySym.hasKey(n.symId):
      let pi = g.procs[g.procBySym[n.symId]]
      var params = pi.params
      let callee = pi.funcIdx
      inc n
      # walk params and args in lock-step to know each arg's wanted type
      if params.kind != DotToken:
        params.into:
          while params.hasMore:
            var pd = takeParamDecl(params)
            if n.hasMore:
              g.genExpr(n, vt(accessOf(g.m, pd.typ)))
      else:
        while n.hasMore: g.genExpr(n, vI32)
      while n.hasMore: skip n     # (defensive: arg count matches param count)
      g.body.call callee
      if pi.hasResult and not wantResult:
        g.body.op opDrop
      elif not pi.hasResult and wantResult:
        g.body.i32Const 0      # void call in value position (shouldn't happen)
    else:
      # cross-module / importc / indirect: unsupported in this slice
      while n.hasMore: skip n
      g.todo("call")
      if wantResult: g.body.i32Const 0

proc genExpr(g: var WasmGen; n: var Cursor; want: ValType) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit, UIntLit, FloatLit, CharLit:
      g.constLit(n, want); inc n
    of Symbol:
      if g.localIdx.hasKey(n.symId):
        g.body.localGet g.localIdx[n.symId]
      else:
        g.todo("global-sym")
      inc n
    of DotToken:
      g.body.i32Const 0; inc n
    else:
      g.todo("atom"); skip n
  of TrueC: g.body.i32Const 1; skip n
  of FalseC: g.body.i32Const 0; skip n
  of NilC: g.body.i32Const 0; skip n     # nil pointer = offset 0
  of SufC, ParC:
    n.into:
      g.genExpr(n, want)
      while n.hasMore: skip n
  of AddC: binArith g, n, 0x6A, 0x7C, 0xA0
  of SubC: binArith g, n, 0x6B, 0x7D, 0xA1
  of MulC: binArith g, n, 0x6C, 0x7E, 0xA2
  of DivC: binDivMod g, n, 0x6D, 0x6E, 0x7F, 0x80, 0xA3
  of ModC: binDivMod g, n, 0x6F, 0x70, 0x81, 0x82, 0xA3  # f64 has no rem (todo)
  of ShlC: binArith g, n, 0x74, 0x86, 0xA0
  of ShrC:
    # logical vs arithmetic shift by signedness
    var probe = n
    probe.into:
      let uns = accessOf(g.m, probe) in {akU8, akU16, akU32, akU64, akPtr}
      binArith g, n, (if uns: 0x76 else: 0x75), (if uns: 0x88 else: 0x87), 0xA0
  of BitandC: binArith g, n, 0x71, 0x83, 0xA0
  of BitorC:  binArith g, n, 0x72, 0x84, 0xA0
  of BitxorC: binArith g, n, 0x73, 0x85, 0xA0
  of BitnotC:
    n.into:
      let w = vt(accessOf(g.m, n)); skip n
      g.genExpr(n, w)
      if w == vI64: (g.body.i64Const -1; g.body.op 0x85'u8)
      else: (g.body.i32Const -1; g.body.op 0x73'u8)
      while n.hasMore: skip n
  of NegC:
    n.into:
      let w = vt(accessOf(g.m, n)); skip n
      if w == vF64: (g.genExpr(n, w); g.body.op 0x9A'u8)
      elif w == vI64: (g.body.i64Const 0; g.genExpr(n, w); g.body.op 0x7D'u8)
      else: (g.body.i32Const 0; g.genExpr(n, w); g.body.op 0x6B'u8)
      while n.hasMore: skip n
  of AndC: genShortCircuit g, n, true
  of OrC:  genShortCircuit g, n, false
  of NotC:
    n.into:
      g.genExpr(n, vI32); g.body.op 0x45'u8   # i32.eqz
      while n.hasMore: skip n
  of EqC:  cmpEq g, n, 0x46, 0x51, 0x61
  of NeqC: cmpEq g, n, 0x47, 0x52, 0x62
  of LtC:  cmp g, n, 0x48, 0x49, 0x53, 0x54, 0x63
  of LeC:  cmp g, n, 0x4C, 0x4D, 0x57, 0x58, 0x65
  of CastC, ConvC: genConv g, n
  of CallC: g.genCall(n, wantResult = true)
  of DotC:
    let ak = g.genFieldAddr(n)
    if ak != akAggregate: g.body.memArg(loadOp(ak), alignOf(ak), 0)
    # aggregate field: leave the address on the stack (it IS the value/offset)
  of AtC:
    let ak = g.genElemAddr(n, false)
    if ak != akAggregate: g.body.memArg(loadOp(ak), alignOf(ak), 0)
  of PatC:
    let ak = g.genElemAddr(n, true)
    if ak != akAggregate: g.body.memArg(loadOp(ak), alignOf(ak), 0)
  of DerefC:
    n.into:
      let ak = g.pointeeAk(n)
      g.genExpr(n, vI32)            # the pointer (address)
      if ak != akAggregate: g.body.memArg(loadOp(ak), alignOf(ak), 0)
      while n.hasMore: skip n
  of AddrC:
    # `(addr lvalue)` — the byte address. For a field/element that is the address
    # `genFieldAddr`/`genElemAddr` compute (without the trailing load).
    n.into:
      case n.exprKind
      of DotC: discard g.genFieldAddr(n)
      of AtC: discard g.genElemAddr(n, false)
      of PatC: discard g.genElemAddr(n, true)
      of DerefC:
        n.into:
          g.genExpr(n, vI32)       # addr(deref p) == p
          while n.hasMore: skip n
      else: g.todo("addr")
      while n.hasMore: skip n
  of SizeofC:
    n.into:
      g.body.i32Const int32(typeLayout(g.m, n).size)
      while n.hasMore: skip n
  else:
    g.todo("expr:" & $n.exprKind); skip n

# ── statements ────────────────────────────────────────────────────────────────

proc genStore(g: var WasmGen; lval: Cursor; val: Cursor) =
  var n = lval
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol and g.localIdx.hasKey(n.symId):
      let w = g.localVT[n.symId]
      var v = val
      g.genExpr(v, w)
      g.body.localSet g.localIdx[n.symId]
    else:
      var v = val
      g.genExpr(v, vI32)
      g.body.op opDrop
      g.todo("store-global")
  of DotC:
    let ak = g.genFieldAddr(n)          # pushes address
    let w = vt(ak)
    var v = val
    g.genExpr(v, w)                     # value
    if ak != akAggregate: g.body.memArg(storeOp(ak), alignOf(ak), 0)
    else: (g.body.op opDrop; g.body.op opDrop; g.todo("agg-store"))
  of AtC, PatC:
    let ak = g.genElemAddr(n, n.exprKind == PatC)
    let w = vt(ak)
    var v = val
    g.genExpr(v, w)
    if ak != akAggregate: g.body.memArg(storeOp(ak), alignOf(ak), 0)
    else: (g.body.op opDrop; g.body.op opDrop; g.todo("agg-store"))
  of DerefC:
    var nn = n
    nn.into:
      let ak = g.pointeeAk(nn)
      let w = vt(ak)
      g.genExpr(nn, vI32)              # pointer
      var v = val
      g.genExpr(v, w)
      if ak != akAggregate: g.body.memArg(storeOp(ak), alignOf(ak), 0)
      while nn.hasMore: skip nn
  else:
    var v = val
    g.genExpr(v, vI32)
    g.body.op opDrop
    g.todo("store")

proc genVar(g: var WasmGen; n: var Cursor) =
  var d = takeVarDecl(n)
  let ak = accessOf(g.m, d.typ)
  let w = vt(ak)
  # register a fresh local
  let idx = g.numParams + uint32(g.locals.len)
  g.locals.add w
  g.localIdx[d.name.symId] = idx
  g.localVT[d.name.symId] = w
  g.m.registerLocal(d.name.symId, d.typ)
  if d.value.kind != DotToken:
    var v = d.value
    g.genExpr(v, w)
    g.body.localSet idx

proc genIfBranches(g: var WasmGen; n: var Cursor) =
  if not n.hasMore: return
  case n.substructureKind
  of ElifU:
    n.into:
      g.genExpr(n, vI32)                 # condition
      g.body.blockOp opIf                # if (void)
      inc g.nframes
      g.genStmtList(n)                   # then
      while n.hasMore: skip n
    # else-part = the remaining branches
    if n.hasMore:
      g.body.op opElse
      g.genIfBranches(n)
    g.body.op opEnd
    dec g.nframes
  of ElseU:
    n.into:
      g.genStmtList(n)
      while n.hasMore: skip n
  else:
    g.todo("if-branch"); skip n

proc genWhile(g: var WasmGen; n: var Cursor) =
  ## `while cond: body`  ->  block { loop { (eqz cond) br_if $block; body; br $loop } }
  n.into:
    g.body.blockOp opBlock             # break target
    inc g.nframes
    let blockLevel = g.nframes
    g.body.blockOp opLoop              # continue target
    inc g.nframes
    let loopLevel = g.nframes
    g.genExpr(n, vI32)                  # condition
    g.body.op 0x45'u8                  # i32.eqz  (exit when false)
    g.body.brIf uint32(g.nframes - blockLevel)
    g.breakBlocks.add blockLevel
    g.genStmtList(n)                   # body
    discard g.breakBlocks.pop()
    g.body.br uint32(g.nframes - loopLevel)   # loop back
    g.body.op opEnd; dec g.nframes     # end loop
    g.body.op opEnd; dec g.nframes     # end block
    while n.hasMore: skip n

proc genStmt(g: var WasmGen; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken: inc n
    else: (g.todo("stmt:" & $n.kind); skip n)
  of StmtsS, ScopeS:
    g.genStmtList(n)
  of VarS, GvarS, TvarS, ConstS:
    g.genVar n
  of AsgnS:
    n.into:
      let lval = n
      skip n
      let val = n
      g.genStore(lval, val)
      while n.hasMore: skip n
  of StoreS, KeepovfS:
    n.into:
      let val = n
      skip n
      let lval = n
      g.genStore(lval, val)
      while n.hasMore: skip n
  of CallS:
    g.genCall(n, wantResult = false)
  of DiscardS:
    n.into:
      if n.kind == DotToken: inc n
      else:
        let ak = g.exprAk(n)
        g.genExpr(n, vt(ak))
        g.body.op opDrop
      while n.hasMore: skip n
  of RetS:
    n.into:
      if n.kind != DotToken and g.curHasResult:
        g.genExpr(n, g.curResultVT)
      else:
        if n.kind != DotToken: skip n
      g.body.op opReturn
      while n.hasMore: skip n
  of IfS:
    n.into:
      g.genIfBranches n
  of WhileS: g.genWhile n
  of BreakS:
    if g.breakBlocks.len > 0:
      g.body.br uint32(g.nframes - g.breakBlocks[^1])
    skip n
  of LabS:
    skip n                    # a label is a block boundary (handled by emitStmtRange)
  of JmpS:
    let lsym = labelSym(n)
    if g.labelBlocks.hasKey(lsym):
      g.body.br uint32(g.nframes - g.labelBlocks[lsym])
    else:
      g.todo("jmp-unbound")
    skip n
  else:
    g.todo("stmt:" & $n.stmtKind); skip n

proc labelSym(n: Cursor): SymId =
  ## The label symbol a `(lab SYMDEF)` / `(jmp SYM)` carries.
  var c = n
  c.into:
    result = c.symId
    while c.hasMore: skip c

proc emitStmtRange(g: var WasmGen; stmts: seq[Cursor]; lo, hi: int) =
  ## Emit `stmts[lo..<hi]`, lowering goto labels to WASM blocks. Hexer's `jmp`s are
  ## forward-only and scoped (see jscodegen), so a `(lab L)` becomes the END of a
  ## `block` that wraps everything before it; `(jmp L)` is a `br` out of that block
  ## (landing right after it, at the label's position) — no relooper needed.
  # find the LAST label in the range: it becomes the outermost wrapping block.
  var k = -1
  for i in countdown(hi - 1, lo):
    if stmts[i].stmtKind == LabS: k = i; break
  if k < 0:
    for i in lo ..< hi:
      var s = stmts[i]
      g.genStmt s
    return
  let lsym = labelSym(stmts[k])
  g.body.blockOp opBlock
  inc g.nframes
  g.labelBlocks[lsym] = g.nframes
  g.emitStmtRange(stmts, lo, k)          # everything before the label, inside the block
  g.body.op opEnd
  dec g.nframes
  g.labelBlocks.del lsym
  g.emitStmtRange(stmts, k + 1, hi)      # everything after the label follows the block

proc genStmtList(g: var WasmGen; n: var Cursor) =
  var stmts: seq[Cursor] = @[]
  if n.stmtKind in {StmtsS, ScopeS}:
    n.into:
      while n.hasMore:
        stmts.add n
        skip n
  else:
    stmts.add n
  g.emitStmtRange(stmts, 0, stmts.len)

# ── proc emission ─────────────────────────────────────────────────────────────

proc buildSig(g: var WasmGen; pi: var ProcInfo) =
  var params: seq[ValType] = @[]
  if pi.params.kind != DotToken:
    var p = pi.params
    p.into:
      while p.hasMore:
        var d = takeParamDecl(p)
        params.add vt(accessOf(g.m, d.typ))
  var results: seq[ValType] = @[]
  if not isVoidType(pi.returnType):
    pi.hasResult = true
    pi.resultVT = vt(accessOf(g.m, pi.returnType))
    results.add pi.resultVT
  pi.typeIdx = g.md.addType(FuncType(params: params, results: results))

proc emitProc(g: var WasmGen; pi: ProcInfo): WasmFunc =
  g.body = @[]
  g.locals = @[]
  g.localIdx = initTable[SymId, uint32]()
  g.localVT = initTable[SymId, ValType]()
  g.numParams = 0
  g.curHasResult = pi.hasResult
  g.curResultVT = pi.resultVT
  g.nframes = 0
  g.breakBlocks = @[]
  g.labelBlocks = initTable[SymId, int]()
  g.m.openScope()
  g.m.registerParams(pi.params)
  if pi.params.kind != DotToken:
    var p = pi.params
    p.into:
      while p.hasMore:
        var d = takeParamDecl(p)
        let w = vt(accessOf(g.m, d.typ))
        g.localIdx[d.name.symId] = g.numParams
        g.localVT[d.name.symId] = w
        inc g.numParams
  var body = pi.body
  g.genStmtList(body)
  # A non-void function must leave a result on `end`; if control can fall through
  # without a `ret`, top it up with a zero so the module stays valid.
  if pi.hasResult:
    case pi.resultVT
    of vI32: g.body.i32Const 0
    of vI64: g.body.i64Const 0
    of vF32: g.body.f32Const 0.0
    of vF64: g.body.f64Const 0.0
  g.m.closeScope()
  result = WasmFunc(typeIdx: pi.typeIdx, locals: g.locals, body: g.body)

# ── reachability ──────────────────────────────────────────────────────────────

proc scanCalls(g: var WasmGen; n: var Cursor; reach: var HashSet[SymId]; work: var seq[SymId]) =
  if n.kind == TagLit:
    if n.exprKind == CallC or n.stmtKind == CallS:
      var c = n
      c.into:
        if c.kind == Symbol and g.procBySym.hasKey(c.symId) and not reach.containsOrIncl(c.symId):
          work.add c.symId
        while c.hasMore: g.scanCalls(c, reach, work)
      skip n
    else:
      n.into:
        while n.hasMore: g.scanCalls(n, reach, work)
  else:
    inc n

# ── module driver ─────────────────────────────────────────────────────────────

proc collectProcs(g: var WasmGen; n: var Cursor) =
  if n.stmtKind == ProcS:
    var pos = n
    var pd = takeProcDecl(pos)
    let sym = pd.name.symId
    let d = g.m.getDeclOrNil(sym)
    var pi = ProcInfo(sym: sym, name: pd.name, params: pd.params,
                      returnType: pd.returnType, body: pd.body)
    if d != nil:
      pi.isImport = d.isImport
      if d.extern != StrId(0) and not d.isImport:
        pi.extern = g.m.pool.strings[d.extern]
    g.procBySym[sym] = g.procs.len
    g.procs.add pi
    n = pos
  elif n.stmtKind == StmtsS:
    n.into:
      while n.hasMore: g.collectProcs n
  else:
    skip n

proc generateWasmCode*(s: var State; inp, outp: string; flags: set[WasmGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var g = WasmGen(m: m, md: initModule(), flags: flags,
                  procBySym: initTable[SymId, int]())

  # 1. collect every proc decl (so calls can be resolved as forward references).
  var probe = beginRead(g.m.src)
  g.collectProcs probe

  # 2. roots = exportc procs that aren't the C `main` shim; mark reachable.
  var reach = initHashSet[SymId]()
  var work: seq[SymId] = @[]
  for pi in g.procs:
    if pi.extern.len > 0 and pi.extern != "main" and not pi.isImport:
      if not reach.containsOrIncl(pi.sym): work.add pi.sym
  while work.len > 0:
    let s0 = work.pop()
    let idx = g.procBySym[s0]
    var b = g.procs[idx].body
    g.scanCalls(b, reach, work)

  # 3. assign function indices to reachable procs (imports first — none here) and
  #    build their signatures.
  g.md.importMemory("env", "memory", 1)
  var order: seq[int] = @[]
  for i in 0 ..< g.procs.len:
    if g.procs[i].sym in reach and not g.procs[i].isImport:
      g.buildSig(g.procs[i])
      g.procs[i].funcIdx = g.md.numImportedFuncs + uint32(order.len)
      order.add i

  # 4. emit each reachable proc; export the exportc ones.
  for i in order:
    let f = g.emitProc(g.procs[i])
    discard g.md.addFunc(f)
    if g.procs[i].extern.len > 0:
      g.md.exportFunc(g.procs[i].extern, g.procs[i].funcIdx)
  g.md.exportMemory("memory")

  let bytes = g.md.encode()
  writeFile(outp, cast[string](bytes))
  if g.todos > 0:
    stdout.writeLine "[lengc wasm] " & inp & ": " & $g.todos & " unsupported node(s) emitted as `unreachable`"

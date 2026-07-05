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
import mangler                      # mangleToC — external name for a cross-module import
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

  ImportSig = object
    funcIdx: uint32
    params: seq[ValType]
    hasResult: bool
    resultVT: ValType

  WasmGen = object
    m: MainModule
    md: WasmModule
    flags: set[WasmGenFlag]
    todos: int
    procs: seq[ProcInfo]
    procBySym: Table[SymId, int]     ## symId -> index into `procs`
    importSigs: Table[SymId, ImportSig]  ## cross-module/importc callee -> host import
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
    hpGlobal: uint32                 ## the bump-allocation pointer ($hp), a mutable i32 global
    globalOff: Table[SymId, int64]   ## module-level var/const -> its byte offset (a memory slot)
    globalAk: Table[SymId, AccessKind]
    dataEnd: int64                   ## next free byte in the globals region

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
proc constructObjInto(g: var WasmGen; destLocal: uint32; n: var Cursor)
proc constructArrInto(g: var WasmGen; destLocal: uint32; n: var Cursor; arrTyp: Cursor; baseOff: int64)

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
    var baseTy = getNominalType(g.m, n)
    # resolve a named type (a local's declared array/ptr type is a symbol) to its body
    if baseTy.typeKind == NoType and baseTy.kind == Symbol:
      let def = g.m.getDeclOrNil(baseTy.symId)
      if def != nil and def.pos.stmtKind == TypeS:
        var p = def.pos
        baseTy = asTypeDecl(p).body
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

proc coerceScalar(g: var WasmGen; fromVT, toVT: ValType) =
  ## Best-effort width coercion when a produced scalar's WASM type differs from
  ## the one required (a call arg vs. the callee's param, a `ret` vs. the result).
  ## The Leng IR is usually explicit, but cross-module boundaries occasionally
  ## need this (e.g. a platform-`int` value into an `int32` slot).
  if fromVT == toVT: return
  if   fromVT == vI64 and toVT == vI32: g.body.op 0xA7'u8   # i32.wrap_i64
  elif fromVT == vI32 and toVT == vI64: g.body.op 0xAC'u8   # i64.extend_i32_s
  elif fromVT == vI32 and toVT == vF64: g.body.op 0xB7'u8   # f64.convert_i32_s
  elif fromVT == vF64 and toVT == vI32: g.body.op 0xAA'u8   # i32.trunc_f64_s
  elif fromVT == vI64 and toVT == vF64: g.body.op 0xB9'u8   # f64.convert_i64_s
  elif fromVT == vF64 and toVT == vI64: g.body.op 0xB0'u8   # i64.trunc_f64_s
  elif fromVT == vF32 and toVT == vF64: g.body.op 0xBB'u8
  elif fromVT == vF64 and toVT == vF32: g.body.op 0xB6'u8

proc isLiteralAtom(n: Cursor): bool {.inline.} =
  n.exprKind == NoExpr and n.kind in {IntLit, UIntLit, FloatLit, CharLit}

proc genArg(g: var WasmGen; n: var Cursor; want: ValType) =
  ## Emit a call argument, coercing a mismatched scalar to the param's WASM type.
  ## A literal is already emitted in `want` by `genExpr`, so it needs no coercion.
  if isLiteralAtom(n):
    g.genExpr(n, want)
  else:
    let aVT = vt(g.exprAk(n))
    g.genExpr(n, want)
    g.coerceScalar(aVT, want)

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
              g.genArg(n, vt(accessOf(g.m, pd.typ)))
      else:
        while n.hasMore: g.genArg(n, vI32)
      while n.hasMore: skip n     # (defensive: arg count matches param count)
      g.body.call callee
      if pi.hasResult and not wantResult:
        g.body.op opDrop
      elif not pi.hasResult and wantResult:
        g.body.i32Const 0      # void call in value position (shouldn't happen)
    elif n.kind == Symbol and g.importSigs.hasKey(n.symId):
      # a cross-module / importc / runtime call: dispatched to a host import whose
      # signature was collected in the pre-pass. Args are coerced to its params.
      let sig = g.importSigs[n.symId]
      inc n
      var pi = 0
      while n.hasMore:
        let w = (if pi < sig.params.len: sig.params[pi] else: vI32)
        g.genArg(n, w)
        inc pi
      g.body.call sig.funcIdx
      if sig.hasResult and not wantResult: g.body.op opDrop
      elif not sig.hasResult and wantResult: g.body.i32Const 0
    else:
      # indirect / unresolved: unsupported in this slice
      while n.hasMore: skip n
      g.todo("call")
      if wantResult: g.body.i32Const 0

# ── linear-memory allocation & aggregate construction ─────────────────────────
# Aggregates (objects/arrays) live in linear memory, exactly as in the JS backend.
# Storage comes from a bump pointer `$hp` — a mutable i32 global — that only ever
# grows (never freed), mirroring the JS backend's `allocFixed` (the pre-existing
# GC/#1518 gap; whole-program reclamation is a later concern). A pointer is the
# integer byte offset the bump returns.

proc roundUp8(x: int64): int64 {.inline.} = (x + 7) and not 7'i64

proc freshLocal(g: var WasmGen; w: ValType): uint32 =
  ## A nameless temporary local (for a construction destination address).
  result = g.numParams + uint32(g.locals.len)
  g.locals.add w

proc emitAlloc(g: var WasmGen; size: int64) =
  ## Bump-allocate `size` bytes, leaving the allocation's byte address on the
  ## stack: `addr = $hp; $hp += roundUp8(size)`.
  let sz = roundUp8(max(size, 0))
  g.body.globalGet g.hpGlobal            # the address (result)
  g.body.globalGet g.hpGlobal
  g.body.i32Const int32(sz)
  g.body.op 0x6A'u8                      # i32.add
  g.body.globalSet g.hpGlobal            # $hp += sz

proc memCopy(g: var WasmGen) =
  ## `memory.copy` — expects (dest, src, len) already on the stack.
  g.body.op 0xFC'u8
  g.body.putU 10                         # memory.copy
  g.body.putByte 0x00                    # dst mem index
  g.body.putByte 0x00                    # src mem index

proc arrayElemInfo(g: var WasmGen; arrTyp: Cursor): (int64, AccessKind) =
  ## (elementStride, elementAccessKind) for an array/flexarray type.
  var t = arrTyp
  if t.typeKind == NoType and t.kind == Symbol:
    let def = g.m.getDeclOrNil(t.symId)
    if def != nil and def.pos.stmtKind == TypeS:
      var p = def.pos
      t = asTypeDecl(p).body
  if t.typeKind notin {ArrayT, FlexarrayT}: return (0'i64, akAggregate)
  var n = t
  var stride = 0'i64
  var ak = akAggregate
  n.into:
    let lay = typeLayout(g.m, n)
    stride = (if lay.align <= 1: lay.size else: (lay.size + lay.align - 1) and not (lay.align - 1))
    ak = accessOf(g.m, n)
    while n.hasMore: skip n
  (stride, ak)

proc storeFieldAt(g: var WasmGen; destLocal: uint32; off: int64; ak: AccessKind; val: var Cursor) =
  ## Emit a store of the value at `val` into `destLocal + off` (scalar) or a
  ## `memory.copy` of `size` bytes (aggregate field: dest, src, len).
  g.body.localGet destLocal
  if off != 0: (g.body.i32Const int32(off); g.body.op 0x6A'u8)
  if ak == akAggregate:
    g.genExpr(val, vI32)                 # src address
    g.body.i32Const int32(typeLayout(g.m, getNominalType(g.m, val)).size)
    g.memCopy()
  else:
    g.genExpr(val, vt(ak))
    g.body.memArg(storeOp(ak), alignOf(ak), 0)

proc constructObjInto(g: var WasmGen; destLocal: uint32; n: var Cursor) =
  ## Build an `(oconstr Type (kv f v) …)` into the storage whose address is in
  ## `destLocal`: one store per field at its `jslayout` byte offset.
  n.into:
    let ty = n
    skip n
    let fields = objectFields(g.m, ty)
    while n.hasMore:
      if n.substructureKind == KvU:
        n.into:
          let fsym = n.symId; inc n
          var off = 0'i64
          var ftyp = n
          for f in fields:
            if f.sym == fsym: off = f.offset; ftyp = f.typ
          let ak = accessOf(g.m, ftyp)
          if n.exprKind == AconstrC:
            g.constructArrInto(destLocal, n, ftyp, off)
          else:
            g.storeFieldAt(destLocal, off, ak, n)
          while n.hasMore: skip n
      else:
        # inheritance-base initializer at offset 0
        if n.exprKind == OconstrC:
          g.constructObjInto(destLocal, n)
        else:
          g.storeFieldAt(destLocal, 0, akPtr, n)

proc constructArrInto(g: var WasmGen; destLocal: uint32; n: var Cursor; arrTyp: Cursor; baseOff: int64) =
  ## Build an `(aconstr T e0 e1 …)` into `destLocal`: one store per element at
  ## `baseOff + i*stride`.
  let (stride, ak) = g.arrayElemInfo(arrTyp)
  n.into:
    skip n                # array type operand
    var i = 0'i64
    while n.hasMore:
      g.storeFieldAt(destLocal, baseOff + i * stride, ak, n)
      inc i

proc isBufferAggregate(g: var WasmGen; typ: Cursor): bool =
  ## A declared object or an array — laid out in linear memory.
  var t = typ
  if t.typeKind in {ObjectT, ArrayT, FlexarrayT}: return true
  if t.typeKind == NoType and t.kind == Symbol:
    let def = g.m.getDeclOrNil(t.symId)
    if def != nil and def.pos.stmtKind == TypeS:
      var p = def.pos
      return asTypeDecl(p).body.typeKind in {ObjectT, ArrayT, FlexarrayT}
  false

proc genConstr(g: var WasmGen; n: var Cursor; isArray: bool) =
  ## An `(oconstr …)` / `(aconstr …)` in value position: allocate storage, build
  ## into it, and leave its byte address on the stack (a pointer, like any value).
  # peek the aggregate type + size without consuming
  var aty = n
  block:
    var probe = n
    probe.into:
      aty = probe
      while probe.hasMore: skip probe
  var size = typeLayout(g.m, aty).size
  if isArray and size == 0:
    # a flexarray/unsized array: size from the element count
    let (stride, _) = g.arrayElemInfo(aty)
    var cnt = 0'i64
    var c = n
    c.into:
      skip c
      while c.hasMore: (inc cnt; skip c)
    size = cnt * stride
  let dst = g.freshLocal(vI32)
  g.emitAlloc(size)
  g.body.localSet dst
  if isArray: g.constructArrInto(dst, n, aty, 0)
  else: g.constructObjInto(dst, n)
  g.body.localGet dst                    # the address is the value

proc genExpr(g: var WasmGen; n: var Cursor; want: ValType) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit, UIntLit, FloatLit, CharLit:
      g.constLit(n, want); inc n
    of Symbol:
      if g.localIdx.hasKey(n.symId):
        g.body.localGet g.localIdx[n.symId]
      elif g.globalOff.hasKey(n.symId):
        let off = g.globalOff[n.symId]
        let ak = g.globalAk[n.symId]
        g.body.i32Const int32(off)              # the slot's byte address
        if ak != akAggregate: g.body.memArg(loadOp(ak), alignOf(ak), 0)
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
  of OconstrC: genConstr g, n, false
  of AconstrC: genConstr g, n, true
  of BaseobjC:
    # `(baseobj Type Depth Expr)`: the base subobject is embedded at offset 0, so
    # in the linear-memory model the pointer is unchanged — just emit the operand.
    n.into:
      skip n            # target base type
      skip n            # inheritance depth
      g.genExpr(n, want)
      while n.hasMore: skip n
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
      of NoExpr:
        if n.kind == Symbol and g.globalOff.hasKey(n.symId):
          g.body.i32Const int32(g.globalOff[n.symId])       # a global's slot address
        elif n.kind == Symbol and g.localIdx.hasKey(n.symId) and
             g.localVT[n.symId] == vI32:
          g.body.localGet g.localIdx[n.symId]               # aggregate local: already an address
        else:
          g.todo("addr")
      else: g.todo("addr")
      while n.hasMore: skip n
  of SizeofC:
    n.into:
      g.body.i32Const int32(typeLayout(g.m, n).size)
      while n.hasMore: skip n
  else:
    g.todo("expr:" & $n.exprKind); skip n

# ── statements ────────────────────────────────────────────────────────────────

proc aggCopyFrom(g: var WasmGen; val: Cursor) =
  ## `dest` is already on the stack; push the source aggregate's address and its
  ## byte size and emit `memory.copy` (value-semantic aggregate assignment).
  var v = val
  g.genExpr(v, vI32)                    # src address
  g.body.i32Const int32(typeLayout(g.m, getNominalType(g.m, val)).size)
  g.memCopy()

proc genStore(g: var WasmGen; lval: Cursor; val: Cursor) =
  var n = lval
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol and g.localIdx.hasKey(n.symId):
      let (tok, tt) = (if n.kind == Symbol: (true, getNominalType(g.m, n)) else: (false, n))
      if tok and accessOf(g.m, tt) == akAggregate:
        # aggregate local = aggregate value: copy bytes (both sides are addresses)
        g.body.localGet g.localIdx[n.symId]
        g.aggCopyFrom(val)
      else:
        let w = g.localVT[n.symId]
        var v = val
        g.genExpr(v, w)
        g.body.localSet g.localIdx[n.symId]
    elif n.kind == Symbol and g.globalOff.hasKey(n.symId):
      let off = g.globalOff[n.symId]
      let ak = g.globalAk[n.symId]
      g.body.i32Const int32(off)               # the slot's byte address
      if ak == akAggregate:
        g.aggCopyFrom(val)
      else:
        var v = val
        g.genExpr(v, vt(ak))
        g.body.memArg(storeOp(ak), alignOf(ak), 0)
    else:
      var v = val
      g.genExpr(v, vI32)
      g.body.op opDrop
      g.todo("store-global")
  of DotC:
    let ak = g.genFieldAddr(n)          # pushes address
    if ak != akAggregate:
      var v = val
      g.genExpr(v, vt(ak))              # value
      g.body.memArg(storeOp(ak), alignOf(ak), 0)
    else:
      g.aggCopyFrom(val)               # dest already on stack
  of AtC, PatC:
    let ak = g.genElemAddr(n, n.exprKind == PatC)
    if ak != akAggregate:
      var v = val
      g.genExpr(v, vt(ak))
      g.body.memArg(storeOp(ak), alignOf(ak), 0)
    else:
      g.aggCopyFrom(val)
  of DerefC:
    var nn = n
    nn.into:
      let ak = g.pointeeAk(nn)
      g.genExpr(nn, vI32)              # pointer (dest)
      if ak != akAggregate:
        var v = val
        g.genExpr(v, vt(ak))
        g.body.memArg(storeOp(ak), alignOf(ak), 0)
      else:
        g.aggCopyFrom(val)
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
  if ak == akAggregate and g.isBufferAggregate(d.typ):
    # an aggregate local lives in linear memory; the local holds its byte address.
    if d.value.kind != DotToken and d.value.exprKind in {OconstrC, AconstrC}:
      var v = d.value            # the constructor self-allocates and returns its addr
      g.genExpr(v, vI32)
      g.body.localSet idx
    elif d.value.kind != DotToken:
      # aggregate copy: fresh storage, then copy the source bytes in (value semantics)
      let size = typeLayout(g.m, d.typ).size
      g.emitAlloc(size); g.body.localSet idx
      g.body.localGet idx
      var v = d.value; g.genExpr(v, vI32)
      g.body.i32Const int32(size)
      g.memCopy()
    else:
      g.emitAlloc(typeLayout(g.m, d.typ).size)   # uninitialised: empty storage
      g.body.localSet idx
    return
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

proc scanCalls(g: var WasmGen; n: var Cursor; reach: var HashSet[SymId];
               work: var seq[SymId]; imports: var HashSet[SymId]) =
  if n.kind == TagLit:
    if n.exprKind == CallC or n.stmtKind == CallS:
      var c = n
      c.into:
        if c.kind == Symbol:
          if not g.procBySym.hasKey(c.symId):
            # not a proc collected from THIS module — try to pull it in from a
            # foreign module (multi-module link). A foreign DEFINED proc is emitted
            # as a real WASM function; an importc stays a host import.
            var d: ptr Definition = nil
            try: d = g.m.getDeclOrNil(c.symId)
            except CatchableError, Defect: d = nil
            if d != nil and d.kind == ProcY and not d.isImport:
              var pos = d.pos
              var pd = takeProcDecl(pos)
              var pi = ProcInfo(sym: c.symId, name: pd.name, params: pd.params,
                                returnType: pd.returnType, body: pd.body)
              g.procBySym[c.symId] = g.procs.len
              g.procs.add pi
          if g.procBySym.hasKey(c.symId):
            if not reach.containsOrIncl(c.symId): work.add c.symId
          else:
            imports.incl c.symId          # a cross-module importc / unresolved callee
        while c.hasMore: g.scanCalls(c, reach, work, imports)
      skip n
    else:
      n.into:
        while n.hasMore: g.scanCalls(n, reach, work, imports)
  else:
    inc n

proc buildImportSig(g: var WasmGen; symId: SymId): (string, ImportSig) =
  ## Derive a host-import signature from the callee's (possibly foreign) decl: its
  ## external name (extern if importc, else the mangled C name) and param/result
  ## WASM types. The host/runtime provides a function of this shape.
  let d = g.m.getDeclOrNil(symId)
  var nm = mangleToC(g.m.pool.syms[symId])
  if d != nil and d.extern != StrId(0): nm = g.m.pool.strings[d.extern]
  var sig = ImportSig(resultVT: vI32)
  if d != nil and d.pos.stmtKind == ProcS:
    var p = d.pos
    var pd = takeProcDecl(p)
    if pd.params.kind != DotToken:
      var pp = pd.params
      pp.into:
        while pp.hasMore:
          var prm = takeParamDecl(pp)
          sig.params.add vt(accessOf(g.m, prm.typ))
    if not isVoidType(pd.returnType):
      sig.hasResult = true
      sig.resultVT = vt(accessOf(g.m, pd.returnType))
  result = (nm, sig)

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

proc collectGlobals(g: var WasmGen; n: var Cursor) =
  ## Assign every module-level var/const a fixed byte slot in the globals region.
  ## A scalar global is read/written by a typed load/store at its offset; an
  ## aggregate global's slot IS its address. importc globals (e.g. `stdout`) are
  ## external and get no slot. Slots are zero-initialised (linear memory starts 0).
  if n.stmtKind in {GvarS, VarS, ConstS, TvarS}:
    var pos = n
    var d = takeVarDecl(pos)
    let sym = d.name.symId
    let def = g.m.getDeclOrNil(sym)
    if def == nil or not def.isImport:
      let lay = typeLayout(g.m, d.typ)
      let align = max(lay.align, 1'i64)
      g.dataEnd = (g.dataEnd + align - 1) and not (align - 1)
      g.globalOff[sym] = g.dataEnd
      g.globalAk[sym] = accessOf(g.m, d.typ)
      g.dataEnd += max(lay.size, 1'i64)
    n = pos
  elif n.stmtKind == StmtsS:
    n.into:
      while n.hasMore: g.collectGlobals n
  else:
    skip n

proc generateWasmCode*(s: var State; inp, outp: string; flags: set[WasmGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var g = WasmGen(m: m, md: initModule(), flags: flags,
                  procBySym: initTable[SymId, int](),
                  importSigs: initTable[SymId, ImportSig](),
                  globalOff: initTable[SymId, int64](),
                  globalAk: initTable[SymId, AccessKind](),
                  dataEnd: 1024)          # globals live above the null/scratch region

  # 1. collect every proc decl (so calls resolve as forward refs) and every
  #    module-level global (each gets a fixed memory slot).
  var probe = beginRead(g.m.src)
  g.collectProcs probe
  var probe2 = beginRead(g.m.src)
  g.collectGlobals probe2

  # 2. roots = exportc procs that aren't the C `main` shim; mark reachable and
  #    collect the cross-module / importc callees they need as host imports.
  var reach = initHashSet[SymId]()
  var imports = initHashSet[SymId]()
  var work: seq[SymId] = @[]
  for pi in g.procs:
    if pi.extern.len > 0 and pi.extern != "main" and not pi.isImport:
      if not reach.containsOrIncl(pi.sym): work.add pi.sym
  while work.len > 0:
    let s0 = work.pop()
    let idx = g.procBySym[s0]
    var b = g.procs[idx].body
    g.scanCalls(b, reach, work, imports)

  # 3. imports FIRST (they occupy the low function indices): the memory, then each
  #    collected host function. Only after this can defined procs be numbered.
  g.md.importMemory("env", "memory", 1)
  for sym in imports:
    let (nm, sigBase) = g.buildImportSig(sym)
    var sig = sigBase
    sig.funcIdx = g.md.addImportFunc("env", nm, g.md.addType(
      FuncType(params: sig.params, results: (if sig.hasResult: @[sig.resultVT] else: @[]))))
    g.importSigs[sym] = sig
  # the bump-allocation pointer: a mutable i32 global starting ABOVE the globals
  # region (offset 0 stays null). Aggregate storage grows up from here (never freed,
  # like the JS backend's allocFixed).
  let hpBase = (g.dataEnd + 7) and not 7'i64
  g.hpGlobal = g.md.addGlobal(vI32, mutable = true, initI32 = int32(hpBase))
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

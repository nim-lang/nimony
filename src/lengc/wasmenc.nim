#
#
#           Leng Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## WebAssembly binary encoder for the Leng WASM backend.
##
## This is the WASM counterpart of `jsnif` (which spells out JS text) and
## `llvmserializer` (which spells out `.ll` text): the *only* place that knows
## how a `.wasm` module is spelled on the wire. It has **no knowledge of Leng**
## — it is a pure `WasmModule -> bytes` encoder — so it can be unit-tested
## against a hand-built module and reused by any front end.
##
## The WASM memory model is the same linear-memory model the JS backend
## (`jscodegen`/`jslayout`) already uses — one byte-addressable buffer, a pointer
## is an integer offset — so the codegen on top of this reuses `jslayout`'s byte
## offsets verbatim; only the emitted *instruction* differs (`i32.load` here vs.
## `HEAP32[p>>2]` there). This module is deliberately the low, mechanical layer:
## LEB128, section framing, opcode bytes. Instruction *selection* lives in
## `wasmcodegen`.

import std / [tables, hashes]

type
  ValType* = enum   ## WASM value types (their binary encoding byte).
    vI32 = 0x7F
    vI64 = 0x7E
    vF32 = 0x7D
    vF64 = 0x7C

  FuncType* = object
    params*: seq[ValType]
    results*: seq[ValType]

  ImportKind = enum ikFunc, ikMemory
  Import = object
    module, name: string
    case kind: ImportKind
    of ikFunc: typeIdx: uint32
    of ikMemory: minPages: uint32

  ExportKind* = enum ekFunc = 0x00, ekMemory = 0x02

  Export = object
    name: string
    kind: ExportKind
    index: uint32

  WasmFunc* = object
    ## A defined function. `locals` are the locals *beyond* the params (which are
    ## implicit locals 0..params-1). `body` is the raw instruction stream WITHOUT
    ## the trailing `end` (the encoder appends it).
    typeIdx*: uint32
    locals*: seq[ValType]
    body*: seq[byte]

  Global = object
    typ: ValType
    mutable: bool
    initI32: int32                     ## init is `i32.const initI32` (all we need: $hp)

  WasmModule* = object
    types: seq[FuncType]
    typeCache: Table[string, uint32]   ## dedup identical signatures
    imports: seq[Import]
    funcs*: seq[WasmFunc]
    globals: seq[Global]
    exports: seq[Export]
    numImportedFuncs*: uint32          ## imported funcs occupy the low func indices

# ── LEB128 ────────────────────────────────────────────────────────────────────

proc putU*(b: var seq[byte]; value: uint64) =
  ## Unsigned LEB128.
  var v = value
  while true:
    var by = byte(v and 0x7F)
    v = v shr 7
    if v != 0: by = by or 0x80
    b.add by
    if v == 0: break

proc putI*(b: var seq[byte]; value: int64) =
  ## Signed LEB128.
  var v = value
  while true:
    var by = byte(v and 0x7F)
    v = v shr 7                       # arithmetic shift (int64) — sign-propagating
    let signBit = (by and 0x40) != 0
    if (v == 0 and not signBit) or (v == -1 and signBit):
      b.add by
      break
    b.add (by or 0x80)

proc putByte*(b: var seq[byte]; x: byte) {.inline.} = b.add x

proc putBytes*(b: var seq[byte]; xs: openArray[byte]) {.inline.} =
  for x in xs: b.add x

proc putName(b: var seq[byte]; s: string) =
  b.putU uint64(s.len)
  for c in s: b.add byte(c)

# ── instruction-body helpers (used by wasmcodegen) ────────────────────────────
# These append into a function body buffer. They are the typed spelling of the
# opcodes; codegen never writes raw opcode bytes.

const
  # control
  opBlock*    = 0x02'u8
  opLoop*     = 0x03'u8
  opIf*       = 0x04'u8
  opElse*     = 0x05'u8
  opEnd*      = 0x0B'u8
  opBr*       = 0x0C'u8
  opBrIf*     = 0x0D'u8
  opReturn*   = 0x0F'u8
  opCall*     = 0x10'u8
  opDrop*     = 0x1A'u8
  # locals / globals
  opLocalGet* = 0x20'u8
  opLocalSet* = 0x21'u8
  opLocalTee* = 0x22'u8
  opGlobalGet* = 0x23'u8
  opGlobalSet* = 0x24'u8
  # consts
  opI32Const* = 0x41'u8
  opI64Const* = 0x42'u8
  opF32Const* = 0x43'u8
  opF64Const* = 0x44'u8
  # empty block type (no result)
  blockTypeVoid* = 0x40'u8

proc emitU(b: var seq[byte]; op: byte; idx: uint32) {.inline.} =
  b.add op
  b.putU uint64(idx)

proc localGet*(b: var seq[byte]; idx: uint32) = emitU(b, opLocalGet, idx)
proc localSet*(b: var seq[byte]; idx: uint32) = emitU(b, opLocalSet, idx)
proc localTee*(b: var seq[byte]; idx: uint32) = emitU(b, opLocalTee, idx)
proc globalGet*(b: var seq[byte]; idx: uint32) = emitU(b, opGlobalGet, idx)
proc globalSet*(b: var seq[byte]; idx: uint32) = emitU(b, opGlobalSet, idx)
proc call*(b: var seq[byte]; funcIdx: uint32) = emitU(b, opCall, funcIdx)

proc i32Const*(b: var seq[byte]; v: int32) =
  b.add opI32Const
  b.putI int64(v)

proc i64Const*(b: var seq[byte]; v: int64) =
  b.add opI64Const
  b.putI v

proc f64Const*(b: var seq[byte]; v: float64) =
  b.add opF64Const
  let bits = cast[uint64](v)
  for i in 0 ..< 8:                     # IEEE-754, little-endian (raw, not LEB128)
    b.add byte((bits shr (i * 8)) and 0xFF)

proc f32Const*(b: var seq[byte]; v: float32) =
  b.add opF32Const
  let bits = cast[uint32](v)
  for i in 0 ..< 4:
    b.add byte((bits shr (i * 8)) and 0xFF)

proc op*(b: var seq[byte]; o: byte) {.inline.} = b.add o
  ## Emit a nullary opcode (add/sub/eq/… — those take no immediate).

proc memArg*(b: var seq[byte]; op: byte; align: uint32; offset: uint32) =
  ## A load/store: opcode then the memarg (alignment hint log2, byte offset).
  b.add op
  b.putU uint64(align)
  b.putU uint64(offset)

proc blockOp*(b: var seq[byte]; op: byte; blockType: byte = blockTypeVoid) =
  ## `block`/`loop`/`if` with a (void by default) result type.
  b.add op
  b.add blockType

proc br*(b: var seq[byte]; depth: uint32) = emitU(b, opBr, depth)
proc brIf*(b: var seq[byte]; depth: uint32) = emitU(b, opBrIf, depth)

# ── module assembly ───────────────────────────────────────────────────────────

proc initModule*(): WasmModule =
  result = WasmModule(typeCache: initTable[string, uint32]())

proc sigKey(ft: FuncType): string =
  result = ""
  for p in ft.params: result.add char(ord(p))
  result.add '>'
  for r in ft.results: result.add char(ord(r))

proc addType*(m: var WasmModule; ft: FuncType): uint32 =
  ## Intern a function signature, returning its type index (dedup identical sigs).
  let k = sigKey(ft)
  m.typeCache.withValue(k, idx):
    return idx[]
  result = uint32(m.types.len)
  m.types.add ft
  m.typeCache[k] = result

proc addImportFunc*(m: var WasmModule; module, name: string; typeIdx: uint32): uint32 =
  ## Import a host function. Returns its function index (imports occupy the low
  ## indices, so this must be called before any defined function is referenced).
  result = m.numImportedFuncs
  m.imports.add Import(module: module, name: name, kind: ikFunc, typeIdx: typeIdx)
  inc m.numImportedFuncs

proc importMemory*(m: var WasmModule; module, name: string; minPages: uint32) =
  m.imports.add Import(module: module, name: name, kind: ikMemory, minPages: minPages)

proc addFunc*(m: var WasmModule; f: WasmFunc): uint32 =
  ## Add a defined function; returns its function index (in the shared func index
  ## space, i.e. after the imported functions).
  result = m.numImportedFuncs + uint32(m.funcs.len)
  m.funcs.add f

proc addGlobal*(m: var WasmModule; typ: ValType; mutable: bool; initI32: int32): uint32 =
  ## A module global with a constant `i32.const` initializer. Returns its index.
  result = uint32(m.globals.len)
  m.globals.add Global(typ: typ, mutable: mutable, initI32: initI32)

proc exportFunc*(m: var WasmModule; name: string; funcIdx: uint32) =
  m.exports.add Export(name: name, kind: ekFunc, index: funcIdx)

proc exportMemory*(m: var WasmModule; name: string; memIdx: uint32 = 0) =
  m.exports.add Export(name: name, kind: ekMemory, index: memIdx)

# ── final serialization ───────────────────────────────────────────────────────

proc section(dst: var seq[byte]; id: byte; payload: seq[byte]) =
  if payload.len == 0: return
  dst.add id
  dst.putU uint64(payload.len)
  dst.putBytes payload

proc encodeFuncType(b: var seq[byte]; ft: FuncType) =
  b.add 0x60                           # func type tag
  b.putU uint64(ft.params.len)
  for p in ft.params: b.add byte(ord(p))
  b.putU uint64(ft.results.len)
  for r in ft.results: b.add byte(ord(r))

proc encodeLocals(b: var seq[byte]; locals: seq[ValType]) =
  ## Run-length encode consecutive equal locals: (count, valtype) pairs.
  var runs: seq[(uint32, ValType)] = @[]
  for lt in locals:
    if runs.len > 0 and runs[^1][1] == lt:
      inc runs[^1][0]
    else:
      runs.add (1'u32, lt)
  b.putU uint64(runs.len)
  for (count, lt) in runs:
    b.putU uint64(count)
    b.add byte(ord(lt))

proc encode*(m: WasmModule): seq[byte] =
  ## Serialize the module to a complete `.wasm` byte image.
  result = @[]
  result.putBytes [0x00'u8, 0x61, 0x73, 0x6D]   # "\0asm"
  result.putBytes [0x01'u8, 0x00, 0x00, 0x00]   # version 1

  # 1: type section
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.types.len)
    for ft in m.types: encodeFuncType(s, ft)
    section(result, 1, s)

  # 2: import section
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.imports.len)
    for imp in m.imports:
      s.putName imp.module
      s.putName imp.name
      case imp.kind
      of ikFunc:
        s.add 0x00                     # import kind: func
        s.putU uint64(imp.typeIdx)
      of ikMemory:
        s.add 0x02                     # import kind: memory
        s.add 0x00                     # limits: min only (no max)
        s.putU uint64(imp.minPages)
    section(result, 2, s)

  # 3: function section (type index of each defined function)
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.funcs.len)
    for f in m.funcs: s.putU uint64(f.typeIdx)
    section(result, 3, s)

  # 6: global section
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.globals.len)
    for g in m.globals:
      s.add byte(ord(g.typ))
      s.add (if g.mutable: 0x01'u8 else: 0x00'u8)
      s.i32Const g.initI32           # init expr: i32.const N
      s.add opEnd                    # ... end
    section(result, 6, s)

  # 7: export section
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.exports.len)
    for e in m.exports:
      s.putName e.name
      s.add byte(ord(e.kind))
      s.putU uint64(e.index)
    section(result, 7, s)

  # 10: code section
  block:
    var s: seq[byte] = @[]
    s.putU uint64(m.funcs.len)
    for f in m.funcs:
      var body: seq[byte] = @[]
      encodeLocals(body, f.locals)
      body.putBytes f.body
      body.add opEnd                   # every function body ends with `end`
      s.putU uint64(body.len)
      s.putBytes body
    section(result, 10, s)

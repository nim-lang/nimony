#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## arkham's *program* model over the nifcore API — the nifcore analog of
## nimony's `programs.nim` (and nifc's `nifmodules.nim`). A NIFC name can refer
## to a declaration in **another module**: a mangled symbol like
## `Color.0.othermod` carries its defining module's suffix (`othermod`). This
## module manages that — it scans the main module's top level (`collect`) and
## lazily **loads foreign modules on demand** to resolve cross-module type
## references, keeping their buffers alive so cursors into them stay valid.
##
## All type-name resolution and size/layout queries (`lookupType`, `resolveType`,
## `slotOf`, `typeSizeAlign`, `aggrLayout`, …) route through the `Program`, so a
## named type defined in any module classifies correctly (e.g. a cross-module
## `enum` parameter is a scalar in a register, not a stack aggregate).

import std / [tables, assertions]
import nifcore, nifcdecl, nifcoreparse
import slots
import ".." / lib / [symparser, nifreader, stringviews]

type
  Extern* = object
    asmName*, extName*: string

  CallTarget* = object
    asmName*: string         ## the asm-NIF symbol to call
    extern*: bool            ## true → (extcall), false → (call)
    atomic*: string          ## non-empty → a GCC `__atomic_*` builtin lowered inline
    memIntrin*: string       ## non-empty → a mem* intrinsic (memcpy/…) lowered inline
    retFloat*: bool          ## true → returns a float (in v0)
    retType*: Cursor         ## the proc's return-type cursor (for `getType`)
    declarative*: bool       ## true → emit/use nifasm's declarative call ABI
                             ## (typed params + `(arg)`/`(res)` cross-checking);
                             ## false → manual marshalling (floats/aggregates/…)

  ProcInfo* = object
    asmName*: string         ## the proc's asm-NIF name (entry → "main.0")
    decl*: Cursor            ## the `(proc …)` declaration
    isEntry*: bool

  Module = ref object
    ## A loaded foreign module. Buffers are owned here so the cursors into them
    ## stay valid for the program's lifetime. Two modes:
    ##  * **indexed** — the `.nif` carries a `.indexat`/`.index` (real hexer
    ##    output): keep the reader open and parse just the *one* requested
    ##    declaration by `jumpTo`-ing its indexed byte offset (lazy, O(1) per
    ##    symbol — system.nim is 438 KB, we never parse it whole).
    ##  * **whole-file** — no index (e.g. hand-written tests): parse the file
    ##    once and index its top-level type decls by scanning.
    hasIndex: bool
    r: Reader                               ## open reader (indexed mode: lazy jumps)
    index: Table[string, int]               ## symbol → absolute byte offset (indexed)
    decls: Table[string, Cursor]            ## parsed-decl cache (both modes)
    declBufs: seq[TokenBuf]                 ## per-decl buffers, kept alive (indexed)
    wholeBuf: TokenBuf                      ## whole-file buffer (fallback)
    wholeTypes: Table[string, Cursor]       ## all `(type …)` decls (fallback)

  Program* = object
    externOrder*: seq[Extern]               ## extproc decls, in order (main module)
    callTarget*: Table[string, CallTarget]  ## NIFC proc symbol → how to call it
    procs*: seq[ProcInfo]                   ## internal procs to emit (entry first)
    globals*: Table[string, Cursor]         ## global (gvar/const) var name → its decl cursor
    tvars*: Table[string, Cursor]           ## thread-local var name → its decl cursor (macOS TLV)
    typeDecls*: TypeEnv                     ## resolved type env: main + requested foreign
    mainTypeList*: seq[(string, Cursor)]    ## main-module types, in declaration order
    requestedForeign*: seq[(string, Cursor)] ## foreign types referenced (cross-module
                                             ## dependency record; nifasm links them)
    needsLibSystem*: bool
    # ── cross-module machinery ──
    scheme: SplittedModulePath              ## path template (dir/<module>.ext)
    tags: TagPool                           ## shared tag pool for parsing foreign modules
    loaded: Table[string, Module]           ## module suffix → loaded foreign module

  TypeEnv* = Table[string, Cursor]          ## a type-symbol table

# ── pass 0: index a module's top-level type declarations ────────────────────

proc indexTypes(buf: var TokenBuf): Table[string, Cursor] =
  ## Map every top-level `(type :name …)` in `buf` to its declaration cursor.
  result = initTable[string, Cursor]()
  var c = buf.beginRead()
  if c.stmtKind != StmtsS: return
  c.into:
    while c.hasMore:
      if c.stmtKind == TypeS:
        let typeStart = c
        var tc = c
        tc.into:
          result[symName(tc)] = typeStart
          while tc.hasMore: skip tc           # drain so `into` stays balanced
        skip c
      else:
        skip c

# ── pass 0: collect the main module's top-level declarations ────────────────

proc parsePragmas(c: var Cursor; importcN, exportcN: var string) =
  if c.substructureKind == PragmasU:
    c.into:
      while c.hasMore:
        case c.pragmaKind
        of ImportcP:
          c.into:
            if c.hasMore: (importcN = strVal(c); inc c)
        of ExportcP:
          c.into:
            if c.hasMore: (exportcN = strVal(c); inc c)
        else: skip c
  else:
    skip c

proc abiScalarType(c: Cursor): bool =
  ## A type that travels in a single GPR with no layout resolution needed:
  ## a primitive non-float scalar (`(i N)`/`(u N)`/`(c N)`/`(bool)`) or a pointer
  ## (`(ptr …)`/`(aptr …)`/`(proctype …)`). Floats, aggregates and *named* types
  ## (enums/objects, possibly cross-module) conservatively answer false so they
  ## keep the manual marshalling path.
  if c.kind != TagLit: return false
  case c.typeKind
  of IT, UT, CT, BoolT, PtrT, AptrT, ProctypeT: true
  else: false

proc isDeclarativeAbi*(decl: Cursor): bool =
  ## Whether `decl`'s call boundary maps onto the simple declarative scheme:
  ## every parameter is a single-GPR scalar/pointer and the result is void or a
  ## single-GPR scalar. The first 8 scalar params travel in x0–x7; any beyond
  ## that are passed on the stack (AAPCS64). Everything else (floats, aggregates,
  ## by-ref, indirect result, named types) falls back to manual marshalling.
  var c = decl
  c.into:
    inc c                                     # name → params slot
    if c.kind == TagLit:                      # (params (param :n prag type) …)
      var pc = c
      pc.into:
        while pc.hasMore:
          var ok = false
          pc.into:                            # (param :name pragmas type)
            inc pc                            # name
            skip pc                           # pragmas
            ok = abiScalarType(pc)
            while pc.hasMore: skip pc          # type (+ anything else)
          if not ok: return false
    skip c                                    # params
    # return type: void, or a single-GPR scalar
    if not (c.kind == DotToken or (c.kind == TagLit and c.typeKind == VoidT)) and
       not abiScalarType(c):
      return false
    while c.hasMore: skip c                   # return type, pragmas, body
  result = true

proc collect*(buf: var TokenBuf; inputPath: string; tags: TagPool): Program =
  result = Program(callTarget: initTable[string, CallTarget](),
                   typeDecls: initTable[string, Cursor](),
                   globals: initTable[string, Cursor](),
                   tvars: initTable[string, Cursor](),
                   loaded: initTable[string, Module](),
                   scheme: splitModulePath(inputPath), tags: tags)
  var c = buf.beginRead()
  assert c.stmtKind == StmtsS, "NIFC top level must be (stmts …)"
  c.into:
    while c.hasMore:
      if c.stmtKind in {GvarS, TvarS, ConstS}:
        let gStart = c
        var gc = c
        let isTvar = c.stmtKind == TvarS
        gc.into:
          let nm = symName(gc)
          if isTvar: result.tvars[nm] = gStart   # thread-local (macOS TLV)
          else: result.globals[nm] = gStart      # ordinary .bss global / const
          while gc.hasMore: skip gc           # drain so `into` stays balanced
        skip c
      elif c.stmtKind == TypeS:
        let typeStart = c
        var tc = c
        tc.into:
          let nm = symName(tc)
          result.typeDecls[nm] = typeStart
          result.mainTypeList.add (nm, typeStart)
          while tc.hasMore: skip tc           # drain so `into` stays balanced
        skip c
      elif c.stmtKind == ProcS:
        let procStart = c
        var pname, importcN, exportcN = ""
        var retFloat = false
        var retType: Cursor
        c.into:
          pname = symName(c); inc c           # name
          skip c                              # params
          retType = c                         # return-type cursor (for getType)
          retFloat = c.kind == TagLit and c.typeKind == FT   # `(f N)` return → v0
          skip c                              # return type
          parsePragmas(c, importcN, exportcN)
          skip c                              # body
        if importcN.len >= 9 and importcN[0 .. 8] == "__atomic_":
          # GCC atomic builtin: not a real external call — arkham lowers it to a
          # lock-free instruction sequence (no extproc/libSystem dependency).
          result.callTarget[pname] = CallTarget(atomic: importcN, retType: retType)
        elif importcN in ["memcpy", "memmove", "memset", "memcmp"]:
          # C mem* intrinsic: lowered inline (no libc dependency) — see genMemIntrin.
          result.callTarget[pname] = CallTarget(memIntrin: importcN, retType: retType)
        elif importcN.len > 0:
          let asmN = importcN & ".0"
          result.externOrder.add Extern(asmName: asmN, extName: "_" & importcN)
          result.callTarget[pname] = CallTarget(asmName: asmN, extern: true,
                                                retFloat: retFloat, retType: retType)
          result.needsLibSystem = true
        else:
          let entry = exportcN.len > 0
          let asmN = if entry: "main.0" else: pname
          result.callTarget[pname] = CallTarget(asmName: asmN, extern: false,
                                                retFloat: retFloat, retType: retType,
                                                declarative: isDeclarativeAbi(procStart))
          result.procs.add ProcInfo(asmName: asmN, decl: procStart, isEntry: entry)
      else:
        skip c
  # Emit the entry proc first so it begins the text section.
  for i in 0 ..< result.procs.len:
    if result.procs[i].isEntry and i != 0:
      swap result.procs[0], result.procs[i]
      break

# ── cross-module type lookup (lazy foreign-module loading) ──────────────────

proc readEmbeddedIndex(r: var Reader): Table[string, int] =
  ## Read the embedded `(.index (h sym Δoff) (x sym Δoff) …)` section located by
  ## the `.indexat` directive, returning `symbol → absolute byte offset`. Offsets
  ## are stored delta-encoded (cumulative); `h`=hidden, `x`=exported (we keep
  ## both). Mirrors `nifindexes.readEmbeddedIndex`, but over the nifcore reader.
  result = initTable[string, int]()
  let indexPos = indexStartsAt(r)
  if indexPos <= 0: return
  r.jumpTo(indexPos)
  var prev = 0
  var t = default(ExpandedToken)
  next(r, t)                                  # `(.index`
  if not (t.tk == ParLe and t.data == ".index"): return
  next(r, t)
  while t.tk != EofToken and t.tk != ParRi:
    if t.tk == ParLe:                         # entry: `(h sym Δoff)` / `(x sym Δoff)`
      next(r, t)                              # the symbol
      var key = ""
      if t.tk == Symbol: key = decodeStr(r, t)
      next(r, t)                              # the (delta) offset
      if t.tk == IntLit:
        let off = int(decodeInt t) + prev
        if key.len > 0: result[key] = off
        prev = off
      next(r, t)                              # closing `)`
      if t.tk == ParRi: next(r, t)
    else:
      next(r, t)

proc loadModule(p: var Program; suffix: string): Module =
  ## Load (and cache) the foreign module identified by `suffix`. Its file is
  ## `<dir-of-main>/<suffix><ext-of-main>` (the same scheme nifc uses). An
  ## embedded `.indexat` index enables lazy per-symbol parsing; otherwise the
  ## whole file is parsed once and its type decls indexed by scanning.
  if p.loaded.hasKey(suffix): return p.loaded[suffix]
  var sc = p.scheme
  sc.name = suffix
  let path = $sc
  let m = Module()
  m.r = nifreader.open(path)                  # reads directives → sets `.indexat`
  if indexStartsAt(m.r) > 0:
    m.hasIndex = true
    m.index = readEmbeddedIndex(m.r)
  else:
    m.hasIndex = false
    m.wholeBuf = parseFromFile(path, sharedTags = p.tags)
    m.wholeTypes = indexTypes(m.wholeBuf)
  p.loaded[suffix] = m
  result = m

proc hasDecl(m: Module; name: string): bool =
  m.decls.hasKey(name) or
    (if m.hasIndex: m.index.hasKey(name) else: m.wholeTypes.hasKey(name))

proc getDecl(p: var Program; m: Module; name: string): Cursor =
  ## The declaration cursor for `name` (precondition: `hasDecl`). In indexed
  ## mode this `jumpTo`s the symbol's byte offset and parses just that one tree
  ## into its own buffer (kept alive in `declBufs`); in whole-file mode it
  ## returns the pre-scanned cursor. Cached either way.
  if m.decls.hasKey(name): return m.decls[name]
  if m.hasIndex:
    m.r.jumpTo(m.index[name])
    var buf = createTokenBuf(64, nil, p.tags)
    parse(m.r, buf)                           # exactly one `(type …)` tree
    m.declBufs.add ensureMove(buf)
    result = m.declBufs[^1].beginRead()
  else:
    result = m.wholeTypes[name]
  m.decls[name] = result

proc lookupType*(p: var Program; name: string): Cursor =
  ## The `(type :name …)` declaration for `name`, resolving across modules. A
  ## name with a module suffix (`Foo.0.othermod`) triggers loading that module
  ## (via its `.indexat` index when present); the decl is cached in `typeDecls`
  ## and recorded in `requestedForeign` as a cross-module dependency.
  if p.typeDecls.hasKey(name): return p.typeDecls[name]
  let s = splitSymName(name)
  if s.module.len == 0:
    raiseAssert "arkham: unknown type " & name
  let m = loadModule(p, s.module)
  if not hasDecl(m, name):
    raiseAssert "arkham: type " & name & " not found in module " & s.module
  let d = getDecl(p, m, name)
  p.typeDecls[name] = d
  p.requestedForeign.add (name, d)
  result = d

# ── named-type resolution ───────────────────────────────────────────────────

proc typeBody*(p: var Program; name: string): Cursor =
  ## The body (3rd child) of a named type decl `(type :name pragmas body)`.
  var d = lookupType(p, name)
  d.into:
    inc d; skip d                             # name, type-pragmas
    result = d                                # the body (a copy)
    skip d                                    # balance the `into`

proc resolveType*(p: var Program; c: Cursor): Cursor =
  ## Follow named-type `Symbol`s (across modules) to the underlying structural
  ## type. A type that is not a named alias is returned unchanged.
  result = c
  var guard = 0
  while result.kind == Symbol:
    result = typeBody(p, symName(result))
    inc guard
    assert guard < 1000, "arkham: cyclic type alias"

# ── size / layout (name-resolving — lives here, not in slots) ────────────────

proc typeSizeAlign*(p: var Program; c: Cursor): (int, int)

proc objSizeAlign(p: var Program; bodyc: Cursor): (int, int) =
  var oc = bodyc
  var off = 0
  var maxAl = 1
  oc.into:
    skip oc                                   # base / inheritance
    while oc.hasMore:
      oc.into:                                # (fld :name pragmas type)
        inc oc; skip oc                       # name, field-pragmas
        let (fsz, fal) = typeSizeAlign(p, oc)
        skip oc                               # consume the field type
        off = align(off, fal) + fsz
        if fal > maxAl: maxAl = fal
  result = (align(off, maxAl), maxAl)

proc typeSizeAlign*(p: var Program; c: Cursor): (int, int) =
  ## Size and alignment (bytes) of a NIFC type, mirroring nifasm's layout.
  case c.kind
  of Symbol:
    var d = lookupType(p, symName(c))
    d.into:
      inc d; skip d                           # name, type-pragmas
      let r = typeSizeAlign(p, d); skip d
      result = r
  of TagLit:
    case c.typeKind
    of IT, UT, FT, CT:
      let bits = typeBits(c)
      let bytes = (if bits > 0: bits else: 64) div 8
      result = (bytes, bytes)
    of BoolT: result = (1, 1)
    of PtrT, AptrT, ProctypeT: result = (8, 8)
    of ObjectT: result = objSizeAlign(p, c)
    of EnumT:                                 # collapses to its base integer type
      var t = c
      t.into:
        result = typeSizeAlign(p, t); skip t
        while t.hasMore: skip t               # efld members
    of ArrayT:
      var t = c
      t.into:
        let (esz, eal) = typeSizeAlign(p, t); skip t
        let n = if t.kind == IntLit: int(intVal(t)) else: 0
        while t.hasMore: skip t               # consume the length (+ any extra)
        result = (esz * n, eal)
    else: raiseAssert "arkham: cannot size type " & $c.typeKind
  else: raiseAssert "arkham: malformed type for sizing"

proc slotOf*(p: var Program; c: Cursor): AsmSlot =
  ## Classify a type cursor into an `AsmSlot`, resolving named types (across
  ## modules) first. A named `enum`/scalar typedef becomes its underlying scalar
  ## slot; a named/inline object, union or array stays `AMem` with its real byte
  ## size and alignment filled in (for AAPCS64 size-based ABI decisions).
  let r = resolveType(p, c)
  if r.kind != TagLit:
    return typeToSlot(r)                       # defensive: shouldn't occur
  case r.typeKind
  of EnumT:
    var base = r
    var t = r
    t.into:
      base = t; skip t                         # base type (a copy)
      while t.hasMore: skip t                  # efld members
    result = slotOf(p, base)                   # base may itself be a named type
  of ObjectT, UnionT, ArrayT:
    let (sz, al) = typeSizeAlign(p, r)
    result = AsmSlot(kind: AMem, size: sz, align: al)
  else:
    result = typeToSlot(r)                      # scalars, ptr, void, …

# ── aggregate layout (shared by the allocator + the code generator) ─────────

proc aggrByteSize*(p: var Program; typeName: string): int =
  var d = lookupType(p, typeName)
  d.into:
    inc d; skip d                             # name, type-pragmas
    let r = typeSizeAlign(p, d); skip d
    result = r[0]

# ── structural type navigation (the pieces arkham's `getType` walks) ────────

proc fieldType*(p: var Program; objType: Cursor; field: string): Cursor =
  ## The structural type cursor of `field` in a resolved `(object …)` type.
  assert objType.kind == TagLit and objType.typeKind == ObjectT,
    "arkham: field access requires an object type"
  var oc = objType
  oc.into:
    skip oc                                   # base / inheritance
    while oc.hasMore:
      oc.into:                                # (fld :name pragmas type)
        let fn = symName(oc); inc oc
        skip oc                               # field-pragmas
        result = oc; skip oc                  # field type (a copy)
        if fn == field: return
  raiseAssert "arkham: field '" & field & "' not found"

proc innerType*(p: var Program; t: Cursor): Cursor =
  ## The element/pointee type of a resolved `(ptr T)` / `(aptr T)` / `(array T …)`.
  assert t.kind == TagLit, "arkham: expected a pointer/array type"
  case t.typeKind
  of PtrT, AptrT, ArrayT:
    var tc = t
    tc.into:
      result = tc; skip tc                    # the pointee / element type
      while tc.hasMore: skip tc
  else: raiseAssert "arkham: deref/index of a non-pointer/array type"

proc aggrWordCount*(p: var Program; typeName: string): int =
  ## Number of 8-byte GPRs a ≤16-byte aggregate occupies (1 or 2).
  let sz = aggrByteSize(p, typeName)
  assert sz <= 16, "arkham v1: >16-byte aggregate ABI (by-ref / x8) not yet supported"
  (sz + 7) div 8

proc aggrLayout*(p: var Program; typeName: string): seq[FieldInfo] =
  result = @[]
  var d = lookupType(p, typeName)
  var body: Cursor
  d.into:
    inc d; skip d                             # name, type-pragmas
    body = d; skip d                          # the body
  assert body.kind == TagLit and body.typeKind == ObjectT,
    "arkham: aggregate ABI requires an object type: " & typeName
  var oc = body
  var off = 0
  oc.into:
    skip oc                                   # base / inheritance
    while oc.hasMore:
      oc.into:                                # (fld :name pragmas type)
        let fn = symName(oc); inc oc
        skip oc                               # field-pragmas
        let (fsz, fal) = typeSizeAlign(p, oc)
        skip oc
        off = align(off, fal)
        result.add (name: fn, off: off, size: fsz)
        off += fsz

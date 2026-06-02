#
#           Arkham — shared front-end for the native code generators
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Architecture-neutral front-end shared by the per-target backends
## (`codegen_a64`, `codegen_x64`). Holds the `CodeGen` state object, the NIFC
## type/lvalue analysis (`getType` / `exprSlot` / `asLvalue` and friends) and
## the type predicates. None of this emits instructions — instruction selection
## and the machine frame live in the backends. The `md` field carries the
## target `MachineDesc` so the backends drive the (shared) register allocator
## and scratch pools from it.

import std / [tables, sets, assertions]
import nifcore, nifcdecl
import slots, machinedesc, analyser, register_allocator, programs
import asmbuf

type
  CodeGen* = object
    ab*: AsmBuf
    ra*: RegAlloc
    buf*: ptr TokenBuf
    md*: MachineDesc                         ## target register file + ABI
    prog*: Program                           ## the whole program (cross-module type env)
    callTarget*: Table[string, CallTarget]
    globals*: Table[string, Cursor]          ## global var name → its decl cursor
    tvars*: Table[string, Cursor]            ## thread-local var name → its decl cursor (macOS TLV)
    tvarNames*: HashSet[string]              ## tvar names, for the per-proc analyser
    freeTmp*: set[Reg]                       ## volatile temps free for scratch
    freeFTmp*: set[FReg]                     ## volatile SIMD/FP temps free for scratch
    retIsFloat*: bool                        ## current proc returns a float (in v0)
    retFloatBits*: int                       ## width (32/64) of the float return type
    rodata*: seq[(string, string)]           ## module-level string literals
    hasFrame*: bool                          ## current proc needs a stack frame
    frameRegs*: seq[Reg]                     ## callee-saved GPRs to save (even count)
    frameFRegs*: seq[FReg]                   ## callee-saved SIMD regs to save (even count)
    framePad*: int                           ## x64: extra prologue `sub rsp` for 16-byte call alignment
    labelCount*: int                         ## fresh-label counter
    loopEnds*: seq[string]                   ## stack of enclosing-loop end labels (for `break`)
    retAggrName*: string                     ## current proc's aggregate return type (or "")
    retIndirect*: bool                       ## return type is >16B (x8 indirect result)
    isEntryProc*: bool                       ## the proc currently emitted is the entry
    indirectReg*: Reg                        ## callee-saved reg holding the x8 dest pointer
    varType*: Table[string, string]          ## aggregate var/param name → its type name
    symType*: Table[string, Cursor]          ## local/param name → its NIFC type cursor (for getType)
    regLocal*: Table[Reg, string]            ## reg → the named local currently bound to it
                                             ## (x64 named-locals: emit the name, not `(reg)`)
    scopeLocals*: seq[seq[tuple[name: string, reg: Reg]]]  ## per-scope register locals to `kill`

# ── type predicates ─────────────────────────────────────────────────────────

proc isSignedType*(c: Cursor): bool =
  ## NIFC arithmetic carries its result type as the first child; treat it as
  ## signed unless it is an unsigned/char integer. (A `case` disambiguates the
  ## NifcType enum members, which share spellings with nifasm's NifasmType.)
  if c.kind != TagLit: return true
  case c.typeKind
  of UT, CT: false
  else: true

proc intTypeWidth*(c: Cursor): int =
  ## Bit width of an integer/char type; 64 for pointer/bool/other (register width).
  if c.kind != TagLit: return 64
  case c.typeKind
  of IT, UT, CT:
    var t = c; inc t
    if t.kind == IntLit and intVal(t) > 0: int(intVal(t)) else: 64
  else: 64

proc slotWidthSigned*(s: AsmSlot): tuple[width: int, signed: bool] =
  ## A scalar slot's significant bit width and signedness (for extension).
  case s.kind
  of AInt:  (s.size * 8, true)
  of AUInt: (s.size * 8, false)
  of ABool: (8, false)
  else:     (64, true)                      # float/aggregate: no widening extend

proc isPtrType*(c: Cursor): bool =
  ## A `case` (not an `in {…}` set) so the discriminant type picks nifcdecl's
  ## `NifcType.PtrT`, not nifasm's same-spelled `NifasmType` member.
  if c.kind != TagLit: return false
  case c.typeKind
  of PtrT, AptrT, ProctypeT: true
  else: false

# ── structural type / slot analysis ─────────────────────────────────────────

proc getType*(g: var CodeGen; c: Cursor): Cursor =
  ## The structural NIFC type cursor of expression `c` (arkham's analog of
  ## `typenav.getType`). Symbols resolve through `symType` / the global/tvar
  ## decls; `dot`/`at`/`deref` navigate into the base's object/array/pointer
  ## type; typed nodes (arith, conv, cast, call) read their carried type. This is
  ## the single source of truth for "is this float?" — no per-form special cases.
  case c.kind
  of Symbol:
    let nm = symName(c)
    if g.symType.hasKey(nm): return g.symType[nm]
    var decl: Cursor
    if g.globals.hasKey(nm): decl = g.globals[nm]
    elif g.tvars.hasKey(nm): decl = g.tvars[nm]
    else: raiseAssert "arkham: getType — unknown symbol " & nm
    var d = decl
    d.into:
      inc d; skip d                           # name, pragmas
      result = d                              # the declared type (a copy)
      while d.hasMore: skip d
  of TagLit:
    case c.exprKind
    of AddC, SubC, MulC, DivC, ModC, ShlC, ShrC, BitandC, BitorC, BitxorC,
       BitnotC, NegC, NotC, ConvC, CastC:
      var t = c
      t.into:
        result = t                            # the carried result/target type
        while t.hasMore: skip t
    of DotC:
      var t = c
      t.into:
        let objTy = resolveType(g.prog, g.getType(t)); skip t  # past the base subtree
        result = fieldType(g.prog, objTy, symName(t)); inc t
        while t.hasMore: skip t
    of AtC:
      var t = c
      t.into:
        let arrTy = resolveType(g.prog, g.getType(t)); skip t  # past the base subtree
        result = innerType(g.prog, arrTy)
        while t.hasMore: skip t
    of DerefC:
      var t = c
      t.into:
        let ptrTy = resolveType(g.prog, g.getType(t))          # pointer type
        result = innerType(g.prog, ptrTy)
        while t.hasMore: skip t
    of CallC:
      var t = c
      t.into:
        result = g.callTarget.getOrDefault(symName(t)).retType
        while t.hasMore: skip t
    else: raiseAssert "arkham: getType — unsupported expression " & $c.exprKind
  else: raiseAssert "arkham: getType — literal has no stored type"

proc exprSlot*(g: var CodeGen; c: Cursor): AsmSlot =
  ## The classified slot of any expression — `getType` for structural forms,
  ## with literals/`addr` (which carry no type cursor) handled directly.
  case c.kind
  of FloatLit: AsmSlot(kind: AFloat, size: 8, align: 8)   # default f64; width refined by context
  of IntLit, UIntLit: AsmSlot(kind: AInt, size: 8, align: 8)
  of CharLit: AsmSlot(kind: AUInt, size: 1, align: 1)
  of StrLit: AsmSlot(kind: AUInt, size: 8, align: 8)      # a pointer
  of Symbol: slotOf(g.prog, g.getType(c))
  of TagLit:
    if c.exprKind == AddrC: AsmSlot(kind: AUInt, size: 8, align: 8)  # &lvalue → a pointer
    else: slotOf(g.prog, g.getType(c))
  else: AsmSlot(kind: AMem)

proc isFloatExpr*(g: var CodeGen; c: Cursor): bool =
  ## Whether `c` has floating-point type (so it flows through the SIMD path).
  g.exprSlot(c).kind == AFloat

proc floatBits*(g: var CodeGen; c: Cursor): int =
  ## Bit width (32 or 64) of a float expression; 64 when undeterminable (e.g. a
  ## bare literal — the caller's context width should be used instead).
  if g.exprSlot(c).size == 4: 32 else: 64

proc srcWidthSigned*(g: var CodeGen; c: Cursor): tuple[width: int, signed: bool] =
  ## Best-effort source scalar (bit width, signedness) of the expression at `c`,
  ## *without* consuming it — used to pick sign- vs zero-extension when a
  ## conversion *widens*. Unknown → (64, true): treated as full register width,
  ## i.e. no widening extension is applied (the pre-source-aware behaviour).
  case c.kind
  of Symbol:
    let nm = symName(c)
    let loc = g.ra.locationOfSym(nm)
    if loc.kind != Undef:
      return slotWidthSigned(loc.typ)        # a local/param: the allocator knows it
    var decl: Cursor                          # a global / thread-local: read its decl type
    if g.globals.hasKey(nm): decl = g.globals[nm]
    elif g.tvars.hasKey(nm): decl = g.tvars[nm]
    else: return (64, true)
    var d = decl
    d.into:
      inc d; skip d                           # name, pragmas
      return slotWidthSigned(slotOf(g.prog, d))
  of TagLit:
    case c.exprKind
    of AddC, SubC, MulC, DivC, ModC, ShlC, ShrC,
       BitandC, BitorC, BitxorC, BitnotC, NegC, ConvC, CastC:
      var t = c                               # these carry their result type first
      t.into:
        return slotWidthSigned(slotOf(g.prog, t))
    else: return (64, true)
  else: return (64, true)

# ── unified lvalue model (addressing modes) ─────────────────────────────────
# A single descriptor for "where a scalar value lives", parsed once from a NIFC
# lvalue cursor and consumed by one load/store family — the analogue of TCC's
# `SValue` + `load()`/`store()`. This collapses the per-form (symbol / global /
# thread-local / field / element / deref) × per-direction (load / store) × int/
# float matrix that used to be spread across genInto / genIntoF / genAsgn.

type
  LvalKind* = enum
    lvReg          ## scalar in a GPR (register-resident local)
    lvFReg         ## scalar in a SIMD register (float local)
    lvGlobal       ## module-level global: address via `(adr name)`
    lvTvar         ## thread-local: address via the macOS TLV thunk
    lvStackScalar  ## spilled scalar `(s)` slot, addressed by name
    lvAggrVar      ## aggregate stack var (only an address; no scalar load/store)
    lvField        ## `(dot base field)` memory operand
    lvElem         ## `(at arr idx)` — re-emitted from the captured cursor
    lvDeref        ## `(deref p)` — pointer recomputed from the captured cursor

  Lvalue* = object
    slot*: AsmSlot               ## float-ness / width (caller picks int vs float path)
    n*: Cursor                   ## the original NIFC lvalue subtree. A backend can
                                 ## re-emit it recursively as a nifasm memory operand
                                 ## (`(dot …)`/`(at …)`/`(deref …)`), letting nifasm
                                 ## collapse the whole access chain to `base+offset` —
                                 ## the flat `base`/`field` fields are a legacy shortcut.
    case kind*: LvalKind
    of lvReg: r*: Reg
    of lvFReg: f*: FReg
    of lvGlobal, lvTvar, lvStackScalar, lvAggrVar: name*: string
    of lvField: base*, field*: string
    of lvElem, lvDeref: cur*: Cursor

proc asLvalue*(g: var CodeGen; c: var Cursor): Lvalue =
  ## Classify and consume an lvalue (Symbol / dot / at / deref). The slot records
  ## float-ness/width for the caller. Scalar callers (genInto/genIntoF/genAsgn)
  ## use the load/store family; `emitAddr` additionally handles `lvAggrVar` (an
  ## aggregate stack var — only its address is meaningful).
  let slot = g.exprSlot(c)
  let nCur = c                                 # capture the subtree before consuming
  case c.kind
  of Symbol:
    let nm = symName(c); inc c
    if g.tvars.hasKey(nm):
      result = Lvalue(kind: lvTvar, name: nm, slot: slot)
    elif g.globals.hasKey(nm):
      result = Lvalue(kind: lvGlobal, name: nm, slot: slot)
    else:
      let loc = g.ra.locationOfSym(nm)
      case loc.kind
      of InReg: result = Lvalue(kind: lvReg, r: loc.r, slot: slot)
      of InFReg: result = Lvalue(kind: lvFReg, f: loc.f, slot: slot)
      of NamedStack:
        if loc.typ.kind == AMem:
          result = Lvalue(kind: lvAggrVar, name: nm, slot: slot)
        else:
          result = Lvalue(kind: lvStackScalar, name: nm, slot: slot)
      else: raiseAssert "arkham v1: symbol is not an lvalue: " & nm
  of TagLit:
    case c.exprKind
    of DotC:
      var base, field = ""
      c.into:
        if c.kind == Symbol: base = symName(c)  # flat shortcut: only a single-level base
        skip c                                  # base subtree (may itself be (dot …)/(at …))
        field = symName(c); inc c
        while c.hasMore: skip c              # depth / access selector
      result = Lvalue(kind: lvField, base: base, field: field, slot: slot)
    of AtC:
      result = Lvalue(kind: lvElem, cur: c, slot: slot); skip c
    of DerefC:
      result = Lvalue(kind: lvDeref, cur: c, slot: slot); skip c
    else: raiseAssert "arkham v1: not an lvalue: " & $c.exprKind
  else: raiseAssert "arkham v1: not an lvalue: " & $c.kind
  result.n = nCur

# ── value descriptors ───────────────────────────────────────────────────────
# Where a just-computed scalar lives (TCC's `SValue`). Expression codegen
# returns a `Val` from the dont-care evaluator `genVal`, so a literal stays an
# immediate and a register-resident local stays in place — values are only
# committed to a specific register when something forces it (`forceReg`/`place`).
# `vkMem` carries an `Lvalue` so an x86 ALU op can fold the operand
# (`add dst, [mem]`) instead of loading it first.

type
  ValKind* = enum
    vkNone       ## no value / dont-care target
    vkImm        ## a known integer immediate
    vkReg        ## integer/pointer in a GPR; `ownsR` → a borrowed temp to release
    vkFReg       ## float in a SIMD register; `ownsF` → a borrowed temp to release
    vkMem        ## a memory operand (an `Lvalue`)

  Val* = object
    case kind*: ValKind
    of vkNone: discard
    of vkImm: imm*: int64
    of vkReg:
      r*: Reg
      ownsR*: bool
    of vkFReg:
      f*: FReg
      ownsF*: bool
    of vkMem: mem*: Lvalue

proc retIsVoid*(t: Cursor): bool {.inline.} =
  t.kind == DotToken or (t.kind == TagLit and t.typeKind == VoidT)

proc paramName*(idx: int): string {.inline.} =
  ## The asm-NIF symbol for positional call parameter `idx`. nifasm's scope keys
  ## symbols by NIF *basename* (the part before the `.<counter>` suffix), so the
  ## counter cannot disambiguate — `p.0` and `p.1` would both reduce to basename
  ## `p` and collide. Each param therefore gets a distinct basename `pN`.
  result = "p" & $idx & ".0"

proc operandInReg*(g: var CodeGen; operand: Cursor; dest: Reg): bool =
  ## Does the (peeked, not consumed) `operand` resolve to a register-resident
  ## local whose home register is `dest`? The accumulator codegen evaluates a
  ## binary op's left operand into `dest`; if the *right* operand lives in `dest`
  ## that would clobber it before use, so the caller must save it first. Only
  ## a bare register symbol can alias — a literal has no register, and a nested
  ## expression is materialized into a fresh scratch (never a live local's home).
  result = false
  if operand.kind == Symbol:
    let loc = g.ra.locationOfSym(symName(operand))
    result = loc.kind == InReg and loc.r == dest

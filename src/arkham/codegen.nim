#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Pass 3: code generation. Walks a NIFC module, runs the analyser + register
## allocator per proc, and emits typed AArch64 / Darwin asm-NIF that `nifasm`
## type-checks, assembles and links.
##
## All asm-NIF tags are emitted through nifasm's own enums (`A64Inst` /
## `NifasmDecl`, see asmbuf) — the single source of truth for the vocabulary.
##
## ABI: AAPCS64. Integer/pointer arguments and the integer return go in x0–x7 /
## x0 (NGRN). Aggregates ≤16 bytes pack into GPRs; aggregates >16 bytes are
## passed by reference (a pointer to a caller copy); large aggregate results use
## the x8 indirect-result register. v1 implements the scalar (int/pointer) path
## end-to-end; floats (HFAs in v0–v7), stack-passed args, and aggregate value
## codegen `raiseAssert` for now.

import std / [assertions, tables, sets, strformat]
import nifcore, nifcdecl
import slots, machine, analyser, register_allocator, programs
import asmbuf

const DarwinLibSystem = "/usr/lib/libSystem.B.dylib"

type
  CodeGen = object
    ab: AsmBuf
    ra: RegAlloc
    buf: ptr TokenBuf
    prog: Program                           ## the whole program (cross-module type env)
    callTarget: Table[string, CallTarget]
    globals: Table[string, Cursor]          ## global var name → its decl cursor
    tvars: Table[string, Cursor]            ## thread-local var name → its decl cursor (macOS TLV)
    tvarNames: HashSet[string]              ## tvar names, for the per-proc analyser
    freeTmp: set[Reg]                       ## volatile temps free for scratch
    freeFTmp: set[FReg]                      ## volatile SIMD/FP temps free for scratch
    retIsFloat: bool                         ## current proc returns a float (in v0)
    retFloatBits: int                        ## width (32/64) of the float return type
    rodata: seq[(string, string)]           ## module-level string literals
    hasFrame: bool                          ## current proc needs a stack frame
    frameRegs: seq[Reg]                     ## callee-saved GPRs to save (even count)
    frameFRegs: seq[FReg]                   ## callee-saved SIMD regs to save (even count)
    labelCount: int                         ## fresh-label counter
    loopEnds: seq[string]                   ## stack of enclosing-loop end labels (for `break`)
    retAggrName: string                     ## current proc's aggregate return type (or "")
    retIndirect: bool                       ## return type is >16B (x8 indirect result)
    indirectReg: Reg                        ## callee-saved reg holding the x8 dest pointer
    varType: Table[string, string]          ## aggregate var/param name → its type name
    symType: Table[string, Cursor]          ## local/param name → its NIFC type cursor (for getType)

# ── low-level emit helpers ──────────────────────────────────────────────────

proc emReg(g: var CodeGen; r: Reg) {.inline.} = g.ab.reg r   # `(xN)`

proc movImm(g: var CodeGen; d: Reg; v: int64) =
  g.ab.tree MovA64: g.emReg d; g.ab.intLit v

proc movReg(g: var CodeGen; d, s: Reg) =
  if d == s: return
  g.ab.tree MovA64: g.emReg d; g.emReg s

proc binReg(g: var CodeGen; op: A64Inst; d, s: Reg) =
  g.ab.tree op: g.emReg d; g.emReg s

proc binImm(g: var CodeGen; op: A64Inst; d: Reg; v: int64) =
  g.ab.tree op: g.emReg d; g.ab.intLit v

proc emAdr(g: var CodeGen; d: Reg; sym: string) =
  g.ab.tree AdrA64: g.emReg d; g.ab.sym sym

proc emLdaxr(g: var CodeGen; rt, rn: Reg) =        # rt ← exclusive-acquire [rn]
  g.ab.tree LdaxrA64: g.emReg rt; g.emReg rn
proc emStlxr(g: var CodeGen; rs, rt, rn: Reg) =    # store-release-exclusive rt→[rn]; rs←status
  g.ab.tree StlxrA64: g.emReg rs; g.emReg rt; g.emReg rn
proc emLdar(g: var CodeGen; rt, rn: Reg) =         # rt ← acquire [rn]
  g.ab.tree LdarA64: g.emReg rt; g.emReg rn
proc emStlr(g: var CodeGen; rt, rn: Reg) =         # release store rt→[rn]
  g.ab.tree StlrA64: g.emReg rt; g.emReg rn
proc emLdrb(g: var CodeGen; rt, base, idx: Reg) =  # rt ← zero-extended byte [base+idx]
  g.ab.tree LdrbA64: g.emReg rt; g.emReg base; g.emReg idx
proc emStrb(g: var CodeGen; rt, base, idx: Reg) =  # store low byte of rt → [base+idx]
  g.ab.tree StrbA64: g.emReg rt; g.emReg base; g.emReg idx

proc genTlvAddr(g: var CodeGen; name: string; dest: Reg) =
  ## `dest ← &threadlocal(name)`. nifasm lowers `(adr dest tvar)` into the macOS
  ## TLV descriptor thunk call, which clobbers x0 and lr. Procs that touch a
  ## thread-local are therefore analysed as having a call: they get a stack frame
  ## (lr saved) and keep their params out of the volatile argument registers.
  g.ab.tree AdrA64:
    g.emReg dest
    g.ab.sym name

proc emPair(g: var CodeGen; op: A64Inst; r1, r2: Reg; off: int) =
  g.ab.tree op: g.emReg r1; g.emReg r2; g.emReg SP; g.ab.intLit off

proc emFPair(g: var CodeGen; op: A64Inst; f1, f2: FReg; off: int) =
  g.ab.tree op: g.ab.dreg f1; g.ab.dreg f2; g.emReg SP; g.ab.intLit off

proc framePush(g: var CodeGen) =
  ## Push fp/lr, then the used callee-saved GPRs, then the callee-saved SIMD
  ## registers — a LIFO stack of pairs.
  g.emPair(StpA64, FP, LR, -16)
  var i = 0
  while i < g.frameRegs.len:
    g.emPair(StpA64, g.frameRegs[i], g.frameRegs[i+1], -16)
    i += 2
  i = 0
  while i < g.frameFRegs.len:
    g.emFPair(FstpA64, g.frameFRegs[i], g.frameFRegs[i+1], -16)
    i += 2

proc framePop(g: var CodeGen) =
  ## Restore in reverse (post-index): SIMD, then callee-saved GPRs, then fp/lr.
  var i = g.frameFRegs.len - 2
  while i >= 0:
    g.emFPair(FldpA64, g.frameFRegs[i], g.frameFRegs[i+1], 16)
    i -= 2
  i = g.frameRegs.len - 2
  while i >= 0:
    g.emPair(LdpA64, g.frameRegs[i], g.frameRegs[i+1], 16)
    i -= 2
  g.emPair(LdpA64, FP, LR, 16)

proc framePushBytes(g: CodeGen): int =
  ## Bytes `framePush` lowers SP by: the fp/lr pair plus each saved callee-saved
  ## GPR / SIMD pair (16 bytes apiece). Used to address incoming stack arguments
  ## relative to SP right after the prologue's pushes (before locals are carved).
  if not g.hasFrame: 0
  else: 16 * (1 + g.frameRegs.len div 2 + g.frameFRegs.len div 2)

# ── scratch register pool (volatile temps not held by a local) ──────────────

proc borrowTmp(g: var CodeGen): Reg =
  for r in IntTempRegs:
    if r in g.freeTmp and not g.ra.isSealed(r):
      excl g.freeTmp, r
      return r
  raiseAssert "arkham v1: out of scratch registers"

proc giveBack(g: var CodeGen; r: Reg) =
  if r in IntTempRegs: g.freeTmp.incl r

# ── SIMD/FP scratch pool + emit helpers (double precision) ──────────────────

proc borrowFTmp(g: var CodeGen): FReg =
  for f in FloatTempRegs:
    if f in g.freeFTmp:
      excl g.freeFTmp, f
      return f
  raiseAssert "arkham v1: out of SIMD scratch registers"

proc giveBackF(g: var CodeGen; f: FReg) =
  if f in FloatTempRegs: g.freeFTmp.incl f

# `bits` (32 or 64) selects the s/d register view; nifasm reads the operand tag
# to pick single- vs double-precision encodings.
proc emFReg(g: var CodeGen; f: FReg; bits: int) {.inline.} = g.ab.freg(f, bits)

proc fmovF(g: var CodeGen; d, s: FReg; bits: int) =
  if d == s: return
  g.ab.tree FmovA64: g.emFReg(d, bits); g.emFReg(s, bits)

proc fmovFromGpr(g: var CodeGen; d: FReg; s: Reg; bits: int) =   # fmov dD/sD, xS/wS (bits)
  g.ab.tree FmovA64: g.emFReg(d, bits); g.ab.reg s

proc fmovToGpr(g: var CodeGen; d: Reg; s: FReg; bits: int) =     # fmov xD/wD, dS/sS (bits)
  g.ab.tree FmovA64: g.ab.reg d; g.emFReg(s, bits)

proc fbin(g: var CodeGen; op: A64Inst; d, s: FReg; bits: int) =  # d = d op s
  g.ab.tree op: g.emFReg(d, bits); g.emFReg(s, bits)

proc fcvtI2F(g: var CodeGen; op: A64Inst; d: FReg; s: Reg; bits: int) =  # scvtf/ucvtf dD, xS
  g.ab.tree op: g.emFReg(d, bits); g.ab.reg s

proc fcvtF2I(g: var CodeGen; op: A64Inst; d: Reg; s: FReg; bits: int) =  # fcvtzs/fcvtzu xD, dS
  g.ab.tree op: g.ab.reg d; g.emFReg(s, bits)

proc emFcvt(g: var CodeGen; d, s: FReg; dstBits, srcBits: int) =  # fcvt: precision convert
  g.ab.tree FcvtA64: g.emFReg(d, dstBits); g.emFReg(s, srcBits)

proc emFLoad(g: var CodeGen; d: FReg; addrReg: Reg; bits: int) =  # fldr dD/sD, [addrReg]
  g.ab.tree FldrA64:
    g.emFReg(d, bits)
    g.ab.tree MemX: g.ab.reg addrReg

proc emFStore(g: var CodeGen; d: FReg; addrReg: Reg; bits: int) = # fstr dD/sD, [addrReg]
  g.ab.tree FstrA64:
    g.ab.tree MemX: g.ab.reg addrReg
    g.emFReg(d, bits)

# ── expressions: target-into-register ───────────────────────────────────────

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg)
proc genIntoF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int)
proc genCall(g: var CodeGen; c: var Cursor)
proc genAtomic(g: var CodeGen; c: var Cursor; builtin: string)
proc genMemIntrin(g: var CodeGen; c: var Cursor; builtin: string)
proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg)
proc materializeCond(g: var CodeGen; c: var Cursor; dest: Reg)
proc genReg(g: var CodeGen; c: var Cursor): tuple[r: Reg, temp: bool]
proc genFReg(g: var CodeGen; c: var Cursor; bits: int): tuple[f: FReg, temp: bool]
proc isFloatExpr(g: var CodeGen; c: Cursor): bool
proc floatBits(g: var CodeGen; c: Cursor): int
proc structToRegs(g: var CodeGen; varName, typeName: string; firstArg: int)
proc regsToStruct(g: var CodeGen; varName, typeName: string; firstArg: int)
proc aggrAddr(g: var CodeGen; c: var Cursor): tuple[r: Reg, temp: bool]
proc byteCopyConst(g: var CodeGen; dst, src: Reg; size: int)

proc emFieldMem(g: var CodeGen; base, field: string) =
  ## `(mem (dot base field))` — nifasm resolves the field offset from the
  ## aggregate's type. `base` is a `(s)` stack var.
  g.ab.tree MemX:
    g.ab.tree DotX:
      g.ab.sym base
      g.ab.sym field

proc emPtrFieldMem(g: var CodeGen; ptrReg: Reg; typeName, field: string) =
  ## `(mem (dot (cast (ptr T) (xN)) field))` — field access through a register
  ## holding a pointer to the aggregate (for >16B by-ref / x8-indirect). The
  ## `cast` types the bare register so nifasm's `dot` can compute the offset.
  g.ab.tree MemX:
    g.ab.tree DotX:
      g.ab.tree CastX:
        g.ab.ptrType: g.ab.sym typeName
        g.emReg ptrReg
      g.ab.sym field

proc emAggrFieldMem(g: var CodeGen; base, field: string) =
  ## Field memory operand for the aggregate named `base`, dispatching on how it
  ## is held: a `(s)` stack struct → direct `(dot …)`; a pointer in a register
  ## (a by-reference param) → through the pointer.
  let loc = g.ra.locationOfSym(base)
  case loc.kind
  of NamedStack: g.emFieldMem(base, field)
  of InReg:      g.emPtrFieldMem(loc.r, g.varType[base], field)
  else: raiseAssert "arkham: aggregate base neither stack nor pointer: " & base

proc emAggrDot(g: var CodeGen; base, field: string) =
  ## The `(dot …)` operand alone (no `mem` wrapper), location-aware — for `lea`
  ## (address-of a field). Stack struct → `(dot var field)`; pointer → cast.
  let loc = g.ra.locationOfSym(base)
  case loc.kind
  of NamedStack:
    g.ab.tree DotX:
      g.ab.sym base
      g.ab.sym field
  of InReg:
    g.ab.tree DotX:
      g.ab.tree CastX:
        g.ab.ptrType: g.ab.sym g.varType[base]
        g.emReg loc.r
      g.ab.sym field
  else: raiseAssert "arkham: aggregate base neither stack nor pointer: " & base

proc emAt(g: var CodeGen; c: var Cursor) =
  ## Consume a NIFC `(at arr idx)` and emit the asm `(at arrvar idx)` operand —
  ## nifasm scales the index by the element size from `arr`'s array type. `arr`
  ## is a stack-array var; `idx` is an immediate or a register value.
  var arr: string
  var idxImm = false
  var idxV = 0'i64
  var ir = NoReg
  var it = false
  c.into:
    arr = symName(c); inc c                 # the array var
    if c.kind == IntLit:
      idxImm = true; idxV = intVal(c); inc c
    else:
      let (r, t) = g.genReg(c); ir = r; it = t
    while c.hasMore: skip c
  g.ab.tree AtX:
    g.ab.sym arr
    if idxImm: g.ab.intLit idxV else: g.emReg ir
  if it: g.giveBack ir

proc emStackVar(g: var CodeGen; name, typeName: string) =
  ## Declare a nifasm-managed stack slot `(var :name (s) typeName)`.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.sym typeName
  g.ab.close()

proc emScalarStackVar(g: var CodeGen; name: string) =
  ## Declare a spilled integer/pointer scalar's stack slot `(var :name (s) (i 64))`.
  ## Always 8-byte wide / 8-aligned (arkham keeps scalars 64-bit in registers and
  ## nifasm's `ldr`/`str` need an 8-aligned slot), regardless of the logical width.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.intType(64)
  g.ab.close()

proc emScalarLoad(g: var CodeGen; dest: Reg; name: string) =
  ## `dest ← [slot]` — load a spilled scalar (nifasm resolves the `(s)` var to
  ## `[sp,#off]`).
  g.ab.tree MovA64: (g.emReg dest; g.ab.sym name)

proc emScalarStore(g: var CodeGen; name: string; src: Reg) =
  ## `[slot] ← src` — store to a spilled scalar's `(s)` var.
  g.ab.tree MovA64: (g.ab.sym name; g.emReg src)

proc emFloatStackVar(g: var CodeGen; name: string; bits: int) =
  ## Declare a spilled float scalar's stack slot `(var :name (s) (f N))`. nifasm
  ## sizes/aligns the slot and resolves the bare symbol to `[sp,#off]`.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.floatType(bits)
  g.ab.close()

proc emFloatScalarLoad(g: var CodeGen; dest: FReg; name: string; bits: int) =
  ## `dest ← [slot]` — load a spilled float (nifasm resolves the `(s)` var operand).
  g.ab.tree FldrA64: (g.emFReg(dest, bits); g.ab.sym name)

proc emFloatScalarStore(g: var CodeGen; name: string; src: FReg; bits: int) =
  ## `[slot] ← src` — store to a spilled float's `(s)` var.
  g.ab.tree FstrA64: (g.ab.sym name; g.emFReg(src, bits))

proc genReg(g: var CodeGen; c: var Cursor): tuple[r: Reg, temp: bool] =
  ## Evaluate `c` into *some* register. A symbol already in a register is used
  ## in place (no copy); otherwise a scratch register is borrowed (caller must
  ## `giveBack`).
  if c.kind == Symbol:
    let loc = g.ra.locationOfSym(symName(c))
    if loc.kind == InReg:
      result = (loc.r, false); inc c; return
  let r = g.borrowTmp()
  g.genInto(c, r)
  result = (r, true)

proc isSignedType(c: Cursor): bool =
  ## NIFC arithmetic carries its result type as the first child; treat it as
  ## signed unless it is an unsigned/char integer. (A `case` disambiguates the
  ## NifcType enum members, which share spellings with nifasm's NifasmType.)
  if c.kind != TagLit: return true
  case c.typeKind
  of UT, CT: false
  else: true

proc genBin(g: var CodeGen; c: var Cursor; dest: Reg;
            signedOp: A64Inst; unsignedOp = NoA64Inst) =
  ## `(op Type a b)` → `dest = a op b`, computing `a` straight into `dest`. For
  ## ops whose instruction depends on signedness (div, shr) pass both variants.
  c.into:
    let op = if unsignedOp != NoA64Inst and not isSignedType(c): unsignedOp
             else: signedOp
    skip c                                  # the result type
    g.genInto(c, dest)                      # a → dest
    if op in {AddA64, SubA64} and c.kind == IntLit and intVal(c) >= 0 and intVal(c) <= 0xFFFF:
      g.binImm(op, dest, intVal(c)); inc c  # dest op= small immediate
    else:
      let (br, bt) = g.genReg(c)
      g.binReg(op, dest, br)                # dest op= b
      if bt: g.giveBack br

proc genMod(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(mod Type a b)` → `dest = a - (a div b)*b` (nifasm has no `msub`).
  c.into:
    let signed = isSignedType(c); skip c
    g.genInto(c, dest)                      # dest = a
    let (br, bt) = g.genReg(c)              # br = b
    let q = g.borrowTmp()
    g.movReg(q, dest)                       # q = a
    g.binReg(if signed: SdivA64 else: UdivA64, q, br)  # q = a div b
    g.binReg(MulA64, q, br)                 # q = (a div b)*b
    g.binReg(SubA64, dest, q)               # dest = a - q
    g.giveBack q
    if bt: g.giveBack br

proc genNeg(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(neg Type a)` → `dest = -a`.
  c.into:
    skip c                                  # type
    g.genInto(c, dest)
    g.ab.tree NegA64: g.emReg dest

proc genBitnot(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(bitnot Type a)` → `~a = -a - 1` (nifasm has no `mvn`).
  c.into:
    skip c                                  # type
    g.genInto(c, dest)
    g.ab.tree NegA64: g.emReg dest          # dest = -a
    g.binImm(SubA64, dest, 1)               # dest = -a - 1

proc genNot(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## boolean `(not a)` → `dest = 1 - a` (a ∈ {0,1}); no result-type child.
  c.into:
    let (br, bt) = g.genReg(c)
    g.movImm(dest, 1)
    g.binReg(SubA64, dest, br)
    if bt: g.giveBack br

proc intTypeWidth(c: Cursor): int =
  ## Bit width of an integer/char type; 64 for pointer/bool/other (register width).
  if c.kind != TagLit: return 64
  case c.typeKind
  of IT, UT, CT:
    var t = c; inc t
    if t.kind == IntLit and intVal(t) > 0: int(intVal(t)) else: 64
  else: 64

proc extendTo(g: var CodeGen; dest: Reg; width: int; signed: bool) =
  ## Normalize the low `width` bits of `dest` to its full 64-bit register form
  ## (sign- or zero-extended). No-op for 64-bit. nifasm has no sxtb/uxtb, so we
  ## use the `lsl #(64-w); asr|lsr #(64-w)` shift pair (immediate shifts), written
  ## here as an inline asm-NIF fragment.
  if width <= 0 or width >= 64: return
  let d = regName(dest)
  let sh = 64 - width
  let down = if signed: "asr" else: "lsr"
  g.ab.splice &"(lsl ({d}) {sh}) ({down} ({d}) {sh})"

proc slotWidthSigned(s: AsmSlot): tuple[width: int, signed: bool] =
  ## A scalar slot's significant bit width and signedness (for extension).
  case s.kind
  of AInt:  (s.size * 8, true)
  of AUInt: (s.size * 8, false)
  of ABool: (8, false)
  else:     (64, true)                      # float/aggregate: no widening extend

proc srcWidthSigned(g: var CodeGen; c: Cursor): tuple[width: int, signed: bool] =
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

proc isPtrType(c: Cursor): bool =
  ## A `case` (not an `in {…}` set) so the discriminant type picks nifcdecl's
  ## `NifcType.PtrT`, not nifasm's same-spelled `NifasmType` member.
  if c.kind != TagLit: return false
  case c.typeKind
  of PtrT, AptrT, ProctypeT: true
  else: false

proc getType(g: var CodeGen; c: Cursor): Cursor =
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
        let objTy = resolveType(g.prog, g.getType(t)); inc t   # base's object type
        result = fieldType(g.prog, objTy, symName(t)); inc t
        while t.hasMore: skip t
    of AtC:
      var t = c
      t.into:
        let arrTy = resolveType(g.prog, g.getType(t)); inc t   # array/aptr type
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

proc exprSlot(g: var CodeGen; c: Cursor): AsmSlot =
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

proc isFloatExpr(g: var CodeGen; c: Cursor): bool =
  ## Whether `c` has floating-point type (so it flows through the SIMD path).
  g.exprSlot(c).kind == AFloat

proc floatBits(g: var CodeGen; c: Cursor): int =
  ## Bit width (32 or 64) of a float expression; 64 when undeterminable (e.g. a
  ## bare literal — the caller's context width should be used instead).
  if g.exprSlot(c).size == 4: 32 else: 64

# ── unified lvalue model (addressing modes) ─────────────────────────────────
# A single descriptor for "where a scalar value lives", parsed once from a NIFC
# lvalue cursor and consumed by one load/store family — the analogue of TCC's
# `SValue` + `load()`/`store()`. This collapses the per-form (symbol / global /
# thread-local / field / element / deref) × per-direction (load / store) × int/
# float matrix that used to be spread across genInto / genIntoF / genAsgn.

type
  LvalKind = enum
    lvReg          ## scalar in a GPR (register-resident local)
    lvFReg         ## scalar in a SIMD register (float local)
    lvGlobal       ## module-level global: address via `(adr name)`
    lvTvar         ## thread-local: address via the macOS TLV thunk
    lvStackScalar  ## spilled scalar `(s)` slot, addressed by name
    lvAggrVar      ## aggregate stack var (only an address; no scalar load/store)
    lvField        ## `(dot base field)` memory operand
    lvElem         ## `(at arr idx)` — re-emitted from the captured cursor
    lvDeref        ## `(deref p)` — pointer recomputed from the captured cursor

  Lvalue = object
    slot: AsmSlot                ## float-ness / width (caller picks int vs float path)
    case kind: LvalKind
    of lvReg: r: Reg
    of lvFReg: f: FReg
    of lvGlobal, lvTvar, lvStackScalar, lvAggrVar: name: string
    of lvField: base, field: string
    of lvElem, lvDeref: cur: Cursor

proc asLvalue(g: var CodeGen; c: var Cursor): Lvalue =
  ## Classify and consume an lvalue (Symbol / dot / at / deref). The slot records
  ## float-ness/width for the caller. Scalar callers (genInto/genIntoF/genAsgn)
  ## use the load/store family; `emitAddr` additionally handles `lvAggrVar` (an
  ## aggregate stack var — only its address is meaningful).
  let slot = g.exprSlot(c)
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
      var base, field: string
      c.into:
        base = symName(c); inc c
        field = symName(c); inc c
        while c.hasMore: skip c              # depth / access selector
      result = Lvalue(kind: lvField, base: base, field: field, slot: slot)
    of AtC:
      result = Lvalue(kind: lvElem, cur: c, slot: slot); skip c
    of DerefC:
      result = Lvalue(kind: lvDeref, cur: c, slot: slot); skip c
    else: raiseAssert "arkham v1: not an lvalue: " & $c.exprKind
  else: raiseAssert "arkham v1: not an lvalue: " & $c.kind

proc derefPtr(g: var CodeGen; l: Lvalue): tuple[r: Reg, temp: bool] =
  ## Recompute the pointer of a captured `(deref p)` lvalue into a register.
  var p = l.cur
  var pr = NoReg
  var pt = false
  p.into:
    let (r, t) = g.genReg(p); pr = r; pt = t
    while p.hasMore: skip p                  # (cppref)?
  result = (pr, pt)

proc emitLoad(g: var CodeGen; l: Lvalue; dest: Reg) =
  ## `dest ← <lvalue>` (integer/pointer). One switch over every addressing mode.
  case l.kind
  of lvReg: g.movReg(dest, l.r)
  of lvStackScalar: g.emScalarLoad(dest, l.name)
  of lvGlobal:
    let tmp = g.borrowTmp(); g.emAdr(tmp, l.name)
    g.ab.tree MovA64:
      g.emReg dest
      g.ab.tree MemX: g.emReg tmp
    g.giveBack tmp
  of lvTvar:
    g.genTlvAddr(l.name, dest)               # dest ← &var
    g.ab.tree MovA64:
      g.emReg dest
      g.ab.tree MemX: g.emReg dest            # dest ← [dest]
  of lvField:
    g.ab.tree MovA64:
      g.emReg dest
      g.emAggrFieldMem(l.base, l.field)
  of lvElem:
    var a = l.cur
    g.ab.tree MovA64:
      g.emReg dest
      g.ab.tree MemX: g.emAt(a)
  of lvDeref:
    let (pr, pt) = g.derefPtr(l)
    g.ab.tree MovA64:
      g.emReg dest
      g.ab.tree MemX: g.emReg pr
    if pt: g.giveBack pr
  of lvFReg: raiseAssert "arkham: integer load from a float local"
  of lvAggrVar: raiseAssert "arkham: scalar load from an aggregate: " & l.name

proc emitLoadF(g: var CodeGen; l: Lvalue; dest: FReg; bits: int) =
  ## `dest ← <lvalue>` (float). `bits` is the contextual precision (s/d view).
  case l.kind
  of lvFReg: g.fmovF(dest, l.f, bits)
  of lvStackScalar: g.emFloatScalarLoad(dest, l.name, bits)
  of lvGlobal:
    let tmp = g.borrowTmp(); g.emAdr(tmp, l.name)
    g.emFLoad(dest, tmp, bits)
    g.giveBack tmp
  of lvField:
    g.ab.tree FldrA64:
      g.emFReg(dest, bits)
      g.emAggrFieldMem(l.base, l.field)
  of lvElem:
    var a = l.cur
    g.ab.tree FldrA64:
      g.emFReg(dest, bits)
      g.ab.tree MemX: g.emAt(a)
  of lvDeref:
    let (pr, pt) = g.derefPtr(l)
    g.emFLoad(dest, pr, bits)
    if pt: g.giveBack pr
  of lvReg: raiseAssert "arkham: float load from an integer local"
  of lvTvar: raiseAssert "arkham v1: float thread-local load unsupported"
  of lvAggrVar: raiseAssert "arkham: scalar load from an aggregate: " & l.name

proc emitStore(g: var CodeGen; l: Lvalue; src: Reg) =
  ## `<lvalue> ← src` (integer/pointer). `src` already holds the value, so for a
  ## deref/tvar destination the address can be (re)computed safely afterwards.
  case l.kind
  of lvReg: g.movReg(l.r, src)
  of lvStackScalar: g.emScalarStore(l.name, src)
  of lvGlobal:
    let tmp = g.borrowTmp(); g.emAdr(tmp, l.name)
    g.ab.tree MovA64:
      g.ab.tree MemX: g.emReg tmp
      g.emReg src
    g.giveBack tmp
  of lvTvar:
    g.genTlvAddr(l.name, IntRet)             # x0 ← &var (clobbers x0/lr, not src)
    g.ab.tree MovA64:
      g.ab.tree MemX: g.emReg IntRet
      g.emReg src
  of lvField:
    g.ab.tree MovA64:
      g.emAggrFieldMem(l.base, l.field)
      g.emReg src
  of lvElem:
    var a = l.cur
    g.ab.tree MovA64:
      g.ab.tree MemX: g.emAt(a)
      g.emReg src
  of lvDeref:
    let (pr, pt) = g.derefPtr(l)
    g.ab.tree MovA64:
      g.ab.tree MemX: g.emReg pr
      g.emReg src
    if pt: g.giveBack pr
  of lvFReg: raiseAssert "arkham: integer store to a float local"
  of lvAggrVar: raiseAssert "arkham: scalar store to an aggregate: " & l.name

proc emitStoreF(g: var CodeGen; l: Lvalue; src: FReg; bits: int) =
  ## `<lvalue> ← src` (float).
  case l.kind
  of lvFReg: g.fmovF(l.f, src, bits)
  of lvStackScalar: g.emFloatScalarStore(l.name, src, bits)
  of lvGlobal:
    let tmp = g.borrowTmp(); g.emAdr(tmp, l.name)
    g.emFStore(src, tmp, bits)
    g.giveBack tmp
  of lvField:
    g.ab.tree FstrA64:
      g.emAggrFieldMem(l.base, l.field)
      g.emFReg(src, bits)
  of lvElem:
    var a = l.cur
    g.ab.tree FstrA64:
      g.ab.tree MemX: g.emAt(a)
      g.emFReg(src, bits)
  of lvDeref:
    let (pr, pt) = g.derefPtr(l)
    g.emFStore(src, pr, bits)
    if pt: g.giveBack pr
  of lvReg: raiseAssert "arkham: float store to an integer local"
  of lvTvar: raiseAssert "arkham v1: float thread-local store unsupported"
  of lvAggrVar: raiseAssert "arkham: scalar store to an aggregate: " & l.name

proc emitAddr(g: var CodeGen; l: Lvalue; dest: Reg) =
  ## `dest ← &<lvalue>`. The address counterpart to the load/store family — one
  ## switch over every addressing mode. (Register-resident scalars are spilled
  ## when address-taken, and by-reference aggregates are intercepted by
  ## `aggrAddr`, so those never reach here.)
  case l.kind
  of lvTvar: g.genTlvAddr(l.name, dest)
  of lvGlobal: g.emAdr(dest, l.name)
  of lvStackScalar, lvAggrVar:
    g.ab.tree LeaA64:
      g.emReg dest
      g.ab.sym l.name
  of lvField:
    g.ab.tree LeaA64:
      g.emReg dest
      g.emAggrDot(l.base, l.field)
  of lvElem:
    var a = l.cur
    g.ab.tree LeaA64:
      g.emReg dest
      g.emAt(a)
  of lvDeref:                                  # &(deref p) == p
    var p = l.cur
    p.into:
      g.genInto(p, dest)
      while p.hasMore: skip p
  of lvReg: raiseAssert "arkham: cannot take the address of a register-resident value"
  of lvFReg: raiseAssert "arkham: cannot take the address of a float register value"

proc genCoerce(g: var CodeGen; c: var Cursor; dest: Reg; isCast: bool) =
  ## Shared lowering for `(conv Type Expr)` / `(cast Type Expr)`. Both evaluate
  ## `Expr` and re-represent it in `Type`'s 64-bit register form. The choice of
  ## sign- vs zero-extension depends on the direction:
  ##  * widening  — extend from the *source* width: a `conv` follows the source
  ##                signedness (value-preserving); a `cast` zero-extends the bits.
  ##  * narrowing/equal — truncate to the target width, extend per the *target*.
  ##  * pointer target — zero-extend a narrower int (int→ptr); else keep the bits.
  ## Integer/char/bool/pointer only (floats `raiseAssert` upstream).
  c.into:
    let tc = resolveType(g.prog, c)           # resolve named types/enums
    let targetSigned = isSignedType(tc)
    let targetW = intTypeWidth(tc)
    let targetPtr = isPtrType(tc)
    skip c                                    # target type
    if g.isFloatExpr(c):
      # float source → integer/pointer target (`dest` is a GPR).
      let fbits = g.floatBits(c)
      let f = g.borrowFTmp()
      g.genIntoF(c, f, fbits)
      if isCast:
        g.fmovToGpr(dest, f, fbits)           # reinterpret the float's bits
      else:
        g.fcvtF2I(if targetSigned: FcvtzsA64 else: FcvtzuA64, dest, f, fbits)  # truncate
        if targetW < 64 and not targetPtr:
          g.extendTo(dest, targetW, signed = targetSigned)
      g.giveBackF f
    else:
      let (srcW, srcSigned) = g.srcWidthSigned(c)
      g.genInto(c, dest)                      # value → dest
      if targetPtr:
        if srcW < 64: g.extendTo(dest, srcW, signed = false)   # int→ptr: zero-extend
      elif srcW < targetW:                    # widening int→int
        g.extendTo(dest, srcW, signed = (not isCast) and srcSigned)
      else:                                   # narrowing or equal width
        g.extendTo(dest, targetW, signed = targetSigned)
    while c.hasMore: skip c

proc genConv(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(conv Type Expr)` — value-preserving numeric conversion.
  g.genCoerce(c, dest, isCast = false)

proc genCast(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(cast Type Expr)` — reinterpret the bits in the target type.
  g.genCoerce(c, dest, isCast = true)

# ── floating-point expressions (single + double precision) ──────────────────
# `bits` (32/64) is the value's precision, threaded top-down: it selects s/d
# register views and single/double instructions. A bare literal has no inherent
# width, so it adopts the contextual `bits`.

proc genFBin(g: var CodeGen; c: var Cursor; dest: FReg; op: A64Inst; bits: int) =
  ## `(op (f N) a b)` → `dest = a op b` (fadd/fsub/fmul/fdiv).
  c.into:
    skip c                                    # result float type
    g.genIntoF(c, dest, bits)                  # a → dest
    let (fr, ft) = g.genFReg(c, bits)          # b → fp temp
    g.fbin(op, dest, fr, bits)
    if ft: g.giveBackF fr

proc genConvToF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int) =
  ## `(conv (f N) Expr)` — produce a `bits`-wide float in `dest`: int→float
  ## (`scvtf`/`ucvtf`) or float→float (`fcvt` if the precision changes).
  c.into:
    skip c                                    # target float type
    if g.isFloatExpr(c):
      let srcBits = g.floatBits(c)
      if srcBits == bits:
        g.genIntoF(c, dest, bits)              # same precision: copy
      else:
        let (sf, st) = g.genFReg(c, srcBits)
        g.emFcvt(dest, sf, bits, srcBits)      # precision convert
        if st: g.giveBackF sf
    else:
      let (srcW, srcSigned) = g.srcWidthSigned(c)
      let tmp = g.borrowTmp()
      g.genInto(c, tmp)                          # int value → GPR
      g.extendTo(tmp, srcW, srcSigned)           # normalize to its full int value
      g.fcvtI2F(if srcSigned: ScvtfA64 else: UcvtfA64, dest, tmp, bits)
      g.giveBack tmp
    while c.hasMore: skip c

proc genCastToF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int) =
  ## `(cast (f N) Expr)` — reinterpret an integer's bits as a float (or copy a
  ## same-precision float unchanged).
  c.into:
    skip c                                    # target float type
    if g.isFloatExpr(c):
      g.genIntoF(c, dest, bits)
    else:
      let tmp = g.borrowTmp()
      g.genInto(c, tmp)                          # integer bit pattern → GPR
      g.fmovFromGpr(dest, tmp, bits)             # reinterpret as float
      g.giveBack tmp
    while c.hasMore: skip c

proc genFReg(g: var CodeGen; c: var Cursor; bits: int): tuple[f: FReg, temp: bool] =
  ## Evaluate the float expression `c` into *some* SIMD register (a symbol's home
  ## register in place, or a borrowed scratch the caller must `giveBackF`).
  if c.kind == Symbol:
    let loc = g.ra.locationOfSym(symName(c))
    if loc.kind == InFReg:
      result = (loc.f, false); inc c; return
  let f = g.borrowFTmp()
  g.genIntoF(c, f, bits)
  result = (f, true)

proc genIntoF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int) =
  ## Evaluate a `bits`-wide float expression into the SIMD register `dest`.
  case c.kind
  of FloatLit:
    let tmp = g.borrowTmp()                     # materialize the bit pattern via a GPR
    if bits == 32:
      g.movImm(tmp, int64(cast[uint32](float32(floatVal(c)))))
    else:
      g.movImm(tmp, cast[int64](floatVal(c)))
    g.fmovFromGpr(dest, tmp, bits)
    g.giveBack tmp
    inc c
  of Symbol:
    let l = g.asLvalue(c)
    g.emitLoadF(l, dest, bits)
  of TagLit:
    case c.exprKind
    of AddC: g.genFBin(c, dest, FaddA64, bits)
    of SubC: g.genFBin(c, dest, FsubA64, bits)
    of MulC: g.genFBin(c, dest, FmulA64, bits)
    of DivC: g.genFBin(c, dest, FdivA64, bits)
    of NegC:
      c.into:
        skip c                                # result type
        g.genIntoF(c, dest, bits)
        g.ab.tree FnegA64: g.emFReg(dest, bits)
    of ConvC: g.genConvToF(c, dest, bits)
    of CastC: g.genCastToF(c, dest, bits)
    of CallC:
      g.genCall(c)                            # float result lands in v0 …
      g.fmovF(dest, FloatRet, bits)           # … move it to the destination
    of DotC:                                  # float struct field: dest ← [base+off]
      let l = g.asLvalue(c); g.emitLoadF(l, dest, bits)
    of AtC:                                   # float array element: dest ← arr[idx]
      let l = g.asLvalue(c); g.emitLoadF(l, dest, bits)
    of DerefC:                                # `(deref p)` → dest ← [p]
      let l = g.asLvalue(c); g.emitLoadF(l, dest, bits)
    else: raiseAssert "arkham v1: float expression not supported: " & $c.exprKind
  else:
    raiseAssert "arkham v1: float operand not supported: " & $c.kind

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg) =
  case c.kind
  of IntLit:
    g.movImm(dest, intVal(c)); inc c
  of StrLit:
    let nm = "msg." & $g.rodata.len
    g.rodata.add (nm, strVal(c))
    g.emAdr(dest, nm); inc c
  of Symbol:
    let l = g.asLvalue(c)
    g.emitLoad(l, dest)
  of TagLit:
    case c.exprKind
    of AddC: g.genBin(c, dest, AddA64)
    of SubC: g.genBin(c, dest, SubA64)
    of MulC: g.genBin(c, dest, MulA64)
    of DivC: g.genBin(c, dest, SdivA64, UdivA64)
    of ModC: g.genMod(c, dest)
    of ShlC: g.genBin(c, dest, LslA64)
    of ShrC: g.genBin(c, dest, AsrA64, LsrA64)
    of BitandC: g.genBin(c, dest, AndA64)
    of BitorC: g.genBin(c, dest, OrrA64)
    of BitxorC: g.genBin(c, dest, EorA64)
    of BitnotC: g.genBitnot(c, dest)
    of NegC: g.genNeg(c, dest)
    of NotC: g.genNot(c, dest)
    of EqC, NeqC, LtC, LeC, AndC, OrC:      # comparison/logic as a value → 0/1
      g.materializeCond(c, dest)
    of ConvC: g.genConv(c, dest)
    of CastC: g.genCast(c, dest)
    of CallC:
      g.genCall(c)                          # result lands in x0 …
      g.movReg(dest, IntRet)                # … move it to the destination
    of DotC:                                # field load: dest ← [base+offset]
      let l = g.asLvalue(c); g.emitLoad(l, dest)
    of DerefC:                              # `(deref p)` → dest ← [p]
      let l = g.asLvalue(c); g.emitLoad(l, dest)
    of AddrC:                               # `(addr lvalue)` → dest ← &lvalue
      c.into:
        g.genAddr(c, dest)
        while c.hasMore: skip c             # (cppref)?
    of AtC:                                 # `(at arr idx)` → dest ← arr[idx]
      let l = g.asLvalue(c); g.emitLoad(l, dest)
    else: raiseAssert "arkham v1: expression not supported: " & $c.exprKind
  else:
    raiseAssert "arkham v1: operand not supported: " & $c.kind

proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `dest ← &lvalue`, with `c` positioned at the lvalue. Parse the addressing
  ## mode once, then let `emitAddr` form the address.
  let l = g.asLvalue(c)
  g.emitAddr(l, dest)

# ── calls ────────────────────────────────────────────────────────────────────

proc retIsVoid(t: Cursor): bool {.inline.} =
  t.kind == DotToken or (t.kind == TagLit and t.typeKind == VoidT)

proc genCall(g: var CodeGen; c: var Cursor) =
  ## `(call f arg…)` — internal `(call)` or external `(extcall)`. Integer/
  ## pointer args go straight into x0,x1,… left-to-right (no nested calls — the
  ## optimizer flattens them), each committed arg register sealed so scratch use
  ## during marshalling can't clobber it. The result lands in x0.
  c.into:
    let fsym = symName(c); inc c
    assert g.callTarget.hasKey(fsym), "arkham v1: unknown call target: " & fsym
    let tgt = g.callTarget[fsym]
    if tgt.atomic.len > 0:                     # GCC `__atomic_*` builtin → inline
      g.genAtomic(c, tgt.atomic)               # consumes the args; result in x0
    elif tgt.memIntrin.len > 0:                # C mem* intrinsic → inline byte loop
      g.genMemIntrin(c, tgt.memIntrin)         # consumes the args; result in x0
    elif tgt.declarative:
      # Declarative call: bind each scalar argument to its positional param `p{i}`
      # and the result to `ret.0`, so nifasm cross-checks the ABI. The `(mov (arg
      # p{i}) x{i})` / `(mov (x0) (res ret.0))` bindings target the value's own
      # register, so they assemble to nothing (self-move elision) yet still mark
      # the argument/result as satisfied.
      var sealedHere: set[Reg] = {}
      var stackTmps: seq[Reg] = @[]            # held values for stack args
      var stackArgIdx: seq[int] = @[]          # their positional param index
      g.ab.tree PrepareA64:
        g.ab.sym tgt.asmName
        var idx = 0
        # Phase 1 — evaluate every argument into a register. The first 8 bind to
        # x0–x7 (self-move-elided); the rest land in sealed temps held until the
        # outgoing stack area is reserved. All evaluation happens before any SP
        # adjustment, so reads of the caller's own locals stay correctly offset.
        while c.hasMore:
          if idx < IntArgRegs.len:
            g.genInto(c, IntArgRegs[idx])      # value → x{idx}
            g.ab.tree MovA64:
              g.ab.tree ArgX: g.ab.sym ("p." & $idx)
              g.emReg IntArgRegs[idx]
            g.ra.seal IntArgRegs[idx]; sealedHere.incl IntArgRegs[idx]
          else:
            let t = g.borrowTmp()
            g.genInto(c, t)
            g.ra.seal t; sealedHere.incl t
            stackTmps.add t; stackArgIdx.add idx
          inc idx
        # Phase 2 — reserve the outgoing stack area and store the held args into
        # it. nifasm resolves `(arg p.k)` to each stack slot's byte offset.
        if stackTmps.len > 0:
          g.ab.tree SubA64: g.emReg SP; g.ab.keyword CsizeX
          for k in 0 ..< stackTmps.len:
            g.ab.tree MovA64:
              g.ab.tree MemX:
                g.emReg SP
                g.ab.tree ArgX: g.ab.sym ("p." & $stackArgIdx[k])
              g.emReg stackTmps[k]
        g.ab.keyword CallA64
        if stackTmps.len > 0:
          g.ab.tree AddA64: g.emReg SP; g.ab.keyword CsizeX
        if not retIsVoid(tgt.retType):
          g.ab.tree MovA64:
            g.emReg IntRet
            g.ab.tree ResX: g.ab.sym "ret.0"
      for t in stackTmps: g.giveBack t
      g.ra.unseal sealedHere
    else:
      var idx = 0
      var fidx = 0
      var sealedHere: set[Reg] = {}
      while c.hasMore:
        if g.isFloatExpr(c):
          # A float argument goes in v{fidx}. v0–v7 are disjoint from the GPR
          # scratch pool (v16–v31), so no sealing is needed to protect it.
          assert fidx < FloatArgRegs.len, "arkham v1: >8 float args (stack passing TODO)"
          g.genIntoF(c, FloatArgRegs[fidx], g.floatBits(c))
          inc fidx
        elif c.kind == Symbol and g.varType.hasKey(symName(c)):
          let vn = symName(c)
          let tn = g.varType[vn]
          if aggrByteSize(g.prog, tn) > 16:
            # >16B → by reference: pass a pointer to it in x{idx}.
            # v1 passes &original; AAPCS64's caller-made copy is a TODO.
            assert idx < IntArgRegs.len, "arkham v1: >8 args (stack passing TODO)"
            let loc = g.ra.locationOfSym(vn)
            case loc.kind
            of NamedStack:
              g.ab.tree LeaA64:
                g.emReg IntArgRegs[idx]
                g.ab.sym vn
            of InReg: g.movReg(IntArgRegs[idx], loc.r)   # already a pointer
            else: raiseAssert "arkham v1: by-ref arg neither stack nor pointer: " & vn
            g.ra.seal IntArgRegs[idx]; sealedHere.incl IntArgRegs[idx]
            inc idx
          else:
            # ≤16B → by value: marshal its words into x{idx..}
            let nw = aggrWordCount(g.prog, tn)
            assert idx + nw <= IntArgRegs.len, "arkham v1: aggregate arg exceeds GPRs"
            g.structToRegs(vn, tn, idx)
            for k in 0 ..< nw:
              g.ra.seal IntArgRegs[idx + k]; sealedHere.incl IntArgRegs[idx + k]
            idx += nw
          inc c
        else:
          assert idx < IntArgRegs.len, "arkham v1: >8 integer args (stack passing TODO)"
          let ar = IntArgRegs[idx]
          g.genInto(c, ar)
          g.ra.seal ar; sealedHere.incl ar
          inc idx
      g.ab.tree PrepareA64:
        g.ab.sym tgt.asmName
        g.ab.keyword (if tgt.extern: ExtcallA64 else: CallA64)
      g.ra.unseal sealedHere

# ── statements ──────────────────────────────────────────────────────────────

proc genOconstr(g: var CodeGen; c: var Cursor; destVar: string) =
  ## `(oconstr Type (kv Field Value)*)` → store each field value into the
  ## aggregate stack var `destVar` (field-wise; no temporary copy).
  c.into:
    skip c                                  # the constructed type
    while c.hasMore:
      assert c.substructureKind == KvU, "arkham v1: oconstr expects (kv …) pairs"
      c.into:                               # (kv Field Value [InheritDepth])
        let field = symName(c); inc c
        let (rr, rt) = g.genReg(c)          # value → register
        g.ab.tree MovA64:
          g.emAggrFieldMem(destVar, field)
          g.emReg rr
        if rt: g.giveBack rr
        while c.hasMore: skip c             # optional inherited-object INTLIT

proc genTypeBody(g: var CodeGen; c: var Cursor) =
  ## Translate a NIFC type at `c` into asm-NIF, advancing `c` past it. Named
  ## types are inlined (resolved against `typeDecls`); object field pragmas are
  ## dropped. v1: int/uint/bool/ptr scalars and objects.
  case c.kind
  of Symbol:
    var d = lookupType(g.prog, symName(c))  # resolves across modules
    d.into:                                 # (type SymbolDef TypePragmas body)
      inc d                                 # name
      skip d                                # TypePragmas (one slot: `.` or (pragmas …))
      g.genTypeBody(d)
    inc c
  of TagLit:
    case c.typeKind
    of IT:
      var t = c; inc t
      g.ab.intType(if t.kind == IntLit: int(intVal(t)) else: 64); skip c
    of UT:
      var t = c; inc t
      g.ab.uintType(if t.kind == IntLit: int(intVal(t)) else: 64); skip c
    of CT:
      var t = c; inc t
      g.ab.charType(if t.kind == IntLit: int(intVal(t)) else: 8); skip c
    of FT:
      var t = c; inc t
      g.ab.floatType(if t.kind == IntLit: int(intVal(t)) else: 64); skip c
    of BoolT:
      g.ab.boolType(); skip c
    of PtrT:
      g.ab.ptrType:
        c.into: g.genTypeBody(c)            # pointee
    of ArrayT:
      c.into:                               # NIFC `(array Type Expr)`
        g.ab.arrayType:
          g.genTypeBody(c)                  # element type
          if c.kind == IntLit:
            g.ab.intLit intVal(c); inc c
          else:
            raiseAssert "arkham v1: array length must be a literal"
    of EnumT:
      c.into:                               # NIFC `(enum BaseType efld*)`
        g.genTypeBody(c)                    # collapse to the base integer type
        while c.hasMore: skip c             # efld members
    of ObjectT:
      c.into:
        skip c                              # inheritance (`.`)
        g.ab.objectType:
          while c.hasMore:
            c.into:                         # (fld :name pragmas type)
              let fn = symName(c); inc c
              skip c                        # field pragmas (dropped)
              g.ab.fldDef(fn):
                g.genTypeBody(c)            # field type
    else:
      raiseAssert "arkham v1: type not supported: " & $c.typeKind
  else:
    raiseAssert "arkham v1: malformed type"

# ── AAPCS64 small-aggregate (≤16B) marshalling ──────────────────────────────
# A ≤16-byte aggregate travels in 1–2 consecutive GPRs; word i ↔ the field at
# byte offset 8·i (word-aligned fields only for now — sub-word packing and the
# >16-byte by-reference / x8-indirect paths `raiseAssert`). Layout/size live in
# slots.nim so the register allocator shares them.

proc structToRegs(g: var CodeGen; varName, typeName: string; firstArg: int) =
  ## Aggregate → x{firstArg+i} (one GPR per 8-byte word).
  let lay = aggrLayout(g.prog, typeName)
  for i in 0 ..< aggrWordCount(g.prog, typeName):
    let fn = fieldAtOffset(lay, i * 8)
    if fn.len == 0: raiseAssert "arkham v1: sub-word-packed aggregate ABI unsupported"
    g.ab.tree MovA64:
      g.emReg IntArgRegs[firstArg + i]
      g.emAggrFieldMem(varName, fn)

proc regsToStruct(g: var CodeGen; varName, typeName: string; firstArg: int) =
  ## x{firstArg+i} → aggregate (one GPR per 8-byte word).
  let lay = aggrLayout(g.prog, typeName)
  for i in 0 ..< aggrWordCount(g.prog, typeName):
    let fn = fieldAtOffset(lay, i * 8)
    if fn.len == 0: raiseAssert "arkham v1: sub-word-packed aggregate ABI unsupported"
    g.ab.tree MovA64:
      g.emAggrFieldMem(varName, fn)
      g.emReg IntArgRegs[firstArg + i]

proc copyStructThroughPtr(g: var CodeGen; srcVar, typeName: string; ptrReg: Reg) =
  ## Field-wise copy of the aggregate `srcVar` to the memory `ptrReg` points at
  ## (any layout — sub-word fields are fine, it copies per field).
  for f in aggrLayout(g.prog, typeName):
    let tmp = g.borrowTmp()
    g.ab.tree MovA64: (g.emReg tmp; g.emAggrFieldMem(srcVar, f.name))
    g.ab.tree MovA64: (g.emPtrFieldMem(ptrReg, typeName, f.name); g.emReg tmp)
    g.giveBack tmp

proc genVarDecl(g: var CodeGen; c: var Cursor) =
  ## `(var :name pragmas type value)`. Scalars land in their allocated register;
  ## aggregates become a nifasm-managed `(var :name (s) type)` stack var, with
  ## field stores doing the initialization (v1: no `oconstr` initializer).
  c.into:
    let name = symName(c); inc c            # name
    skip c                                  # pragmas
    let typeCur = c                         # capture the type before skipping
    g.symType[name] = typeCur               # record for getType
    skip c                                  # type
    let loc = g.ra.locationOfSym(name)
    case loc.kind
    of NamedStack:
      if loc.typ.kind == AFloat:
        # spilled float scalar: a nifasm `(s) (f N)` slot + a store of its
        # initializer (computed into a scratch SIMD register first).
        let bits = loc.typ.size * 8
        g.emFloatStackVar(name, bits)
        if c.kind == DotToken: inc c          # no initializer
        else:
          let f = g.borrowFTmp()
          g.genIntoF(c, f, bits)
          g.emFloatScalarStore(name, f, bits)
          g.giveBackF f
      elif loc.typ.kind != AMem:
        # spilled integer/pointer scalar: a nifasm `(s)` (i 64) slot + a store of
        # its initializer (computed into a scratch register first).
        g.emScalarStackVar(name)
        if c.kind == DotToken: inc c          # no initializer
        else:
          let tmp = g.borrowTmp()
          g.genInto(c, tmp)
          g.emScalarStore(name, tmp)
          g.giveBack tmp
      else:
        g.ab.open NifasmDecl.VarD
        g.ab.symDef name
        g.ab.keyword SO                       # `(s)` — nifasm-managed stack slot
        var tc = typeCur                       # nifasm wants a *named* type ref
        if tc.kind == Symbol: g.ab.sym symName(tc)
        else: g.genTypeBody(tc)               # inline only for scalar aggregates
        g.ab.close()
        let typeName = if tc.kind == Symbol: symName(tc) else: ""
        if typeName.len > 0: g.varType[name] = typeName
        if c.kind == DotToken: inc c          # no initializer
        elif c.exprKind == OconstrC: g.genOconstr(c, name)
        elif c.exprKind == CallC:             # receive an aggregate return
          assert typeName.len > 0, "arkham v1: call-returned aggregate needs a named type"
          if aggrByteSize(g.prog, typeName) > 16:
            # >16B: hand the callee a pointer to this var via x8; it writes there.
            g.ab.tree LeaA64: g.emReg IndirectResultReg; g.ab.sym name
            g.genCall(c)
          else:
            g.genCall(c)
            g.regsToStruct(name, typeName, 0)
        else:                                 # copy-init from another aggregate: `var b = a`
          assert typeName.len > 0, "arkham v1: aggregate copy-init needs a named type: " & name
          let dstA = g.borrowTmp()
          g.ab.tree LeaA64: (g.emReg dstA; g.ab.sym name)
          let (srcA, srcT) = g.aggrAddr(c)
          g.byteCopyConst(dstA, srcA, aggrByteSize(g.prog, typeName))
          if srcT: g.giveBack srcA
          g.giveBack dstA
    of InReg:
      if c.kind == DotToken: inc c          # no initializer
      else: g.genInto(c, loc.r)
    of InFReg:
      if c.kind == DotToken: inc c          # no initializer
      else: g.genIntoF(c, loc.f, loc.typ.size * 8)   # float local
    else: raiseAssert "arkham v1: stack-resident local: " & name

proc genStmt(g: var CodeGen; c: var Cursor)

proc genActionStmts(g: var CodeGen; c: var Cursor) =
  ## Emit the statements of an `(elif … action)` / `(else action)` body,
  ## flattening a `(stmts …)` arg.
  if c.stmtKind == StmtsS:
    c.into:
      while c.hasMore: genStmt(g, c)
  else:
    genStmt(g, c)

# ── control flow: labels + goto ─────────────────────────────────────────────

proc freshLabel(g: var CodeGen): string =
  # Name must be a NIF *symbol* (needs a '.'), but `extractBasename` strips a
  # trailing `.<digits>`, so put the counter *before* the suffix ("L0.0", …)
  # to keep basenames ("L0", "L1") distinct.
  result = "L" & $g.labelCount & ".0"
  inc g.labelCount

proc emLab(g: var CodeGen; name: string) =
  g.ab.tree LabA64: g.ab.symDef name        # (lab :L)

proc emBr(g: var CodeGen; tag: A64Inst; name: string) =
  g.ab.tree tag: g.ab.sym name              # (b L) / (beq L) / …

# ── atomics: GCC __atomic_* builtins → AArch64 load/store-exclusive loops ─────
# arkham lowers the call-shaped atomic builtins (see programs.collect) the way
# the LLVM backend does, but to a portable LL/SC retry loop. Memory ordering is
# always the strong acquire/release form (matching the backend's seq_cst); the
# `memorder` argument is ignored. Every variant leaves its result in x0.

proc genAtomicRmw(g: var CodeGen; pReg, val: Reg; isXchg: bool;
                  op: A64Inst; returnNew: bool) =
  ## `loop: ldaxr old,[p]; new = old op val (or val for xchg); stlxr st,new,
  ## [p]; cbnz st,loop`. Result (old or new) → x0. `pReg`/`val` stay live.
  let xOld = g.borrowTmp()
  let xNew = g.borrowTmp()
  let xStatus = g.borrowTmp()
  let loop = g.freshLabel()
  let (p, v, old, neu, st) = (regName pReg, regName val,
                              regName xOld, regName xNew, regName xStatus)
  let update =                              # new ← val (xchg) | old op val (rmw)
    if isXchg: &"(mov ({neu}) ({v}))"
    else:      &"(mov ({neu}) ({old})) ({op} ({neu}) ({v}))"
  g.ab.splice &"(lab :{loop}) " &
              &"(ldaxr ({old}) ({p})) " &
              update & " " &
              &"(stlxr ({st}) ({neu}) ({p})) " &
              &"(cmp ({st}) 0) (bne {loop})"   # retry on store-exclusive failure
  g.movReg(IntRet, if returnNew: xNew else: xOld)
  g.giveBack xStatus; g.giveBack xNew; g.giveBack xOld

proc genAtomicCmpXchg(g: var CodeGen; c: var Cursor) =
  ## `__atomic_compare_exchange_n(ptr, expected_ptr, desired, weak, succ, fail)`.
  ## Returns 1 on success, 0 on failure; on failure *expected is updated.
  let (pReg, pT) = g.genReg(c)
  let (expPtr, expT) = g.genReg(c)
  let (des, desT) = g.genReg(c)
  skip c; skip c; skip c                    # weak, success order, failure order
  let xExp = g.borrowTmp()
  let xOld = g.borrowTmp()
  let xStatus = g.borrowTmp()
  let loop = g.freshLabel()
  let lFail = g.freshLabel()
  let lDone = g.freshLabel()
  let (p, ep, d) = (regName pReg, regName expPtr, regName des)
  let (exp, old, st, ret) = (regName xExp, regName xOld,
                             regName xStatus, regName IntRet)
  g.ab.splice(
    &"(ldar ({exp}) ({ep})) " &              # expected = *expected_ptr
    &"(lab :{loop}) (ldaxr ({old}) ({p})) " &
    &"(cmp ({old}) ({exp})) (bne {lFail}) " &      # mismatch → fail
    &"(stlxr ({st}) ({d}) ({p})) " &
    &"(cmp ({st}) 0) (bne {loop}) " &              # store-exclusive lost → retry
    &"(mov ({ret}) 1) (b {lDone}) " &
    &"(lab :{lFail}) (clrex) " &                   # drop the exclusive reservation
    &"(stlr ({old}) ({ep})) " &                    # *expected = actual old value
    &"(mov ({ret}) 0) (lab :{lDone})")
  g.giveBack xStatus; g.giveBack xOld; g.giveBack xExp
  if desT: g.giveBack des
  if expT: g.giveBack expPtr
  if pT: g.giveBack pReg

proc genAtomic(g: var CodeGen; c: var Cursor; builtin: string) =
  ## Lower one `__atomic_*` builtin call. `c` is positioned at the first
  ## argument; this consumes all of them. Result (if any) lands in x0.
  case builtin
  of "__atomic_load_n":                      # (ptr, memorder) → *ptr
    let (pReg, pT) = g.genReg(c); skip c
    g.emLdar(IntRet, pReg)
    if pT: g.giveBack pReg
  of "__atomic_store_n":                     # (ptr, val, memorder) → void
    let (pReg, pT) = g.genReg(c)
    let (val, valT) = g.genReg(c); skip c
    g.emStlr(val, pReg)
    if valT: g.giveBack val
    if pT: g.giveBack pReg
  of "__atomic_clear":                       # (ptr, memorder) → void; *ptr = 0
    let (pReg, pT) = g.genReg(c); skip c
    let z = g.borrowTmp(); g.movImm(z, 0)
    g.emStlr(z, pReg)
    g.giveBack z
    if pT: g.giveBack pReg
  of "__atomic_thread_fence":                # (memorder) → void
    skip c
    g.ab.keyword DmbA64
  of "__atomic_signal_fence":                # (memorder) → void; compiler barrier
    skip c                                   # no hardware fence needed
  of "__atomic_exchange_n",
     "__atomic_fetch_add", "__atomic_fetch_sub",
     "__atomic_fetch_and", "__atomic_fetch_or", "__atomic_fetch_xor",
     "__atomic_add_fetch", "__atomic_sub_fetch":
    let (pReg, pT) = g.genReg(c)
    let (val, valT) = g.genReg(c); skip c    # ptr, val, memorder
    case builtin
    of "__atomic_exchange_n":  g.genAtomicRmw(pReg, val, true, NoA64Inst, false)
    of "__atomic_fetch_add":   g.genAtomicRmw(pReg, val, false, AddA64, false)
    of "__atomic_fetch_sub":   g.genAtomicRmw(pReg, val, false, SubA64, false)
    of "__atomic_fetch_and":   g.genAtomicRmw(pReg, val, false, AndA64, false)
    of "__atomic_fetch_or":    g.genAtomicRmw(pReg, val, false, OrrA64, false)
    of "__atomic_fetch_xor":   g.genAtomicRmw(pReg, val, false, EorA64, false)
    of "__atomic_add_fetch":   g.genAtomicRmw(pReg, val, false, AddA64, true)
    of "__atomic_sub_fetch":   g.genAtomicRmw(pReg, val, false, SubA64, true)
    else: discard
    if valT: g.giveBack val
    if pT: g.giveBack pReg
  of "__atomic_test_and_set":                # (ptr, memorder) → bool (old != 0)
    let (pReg, pT) = g.genReg(c); skip c
    let one = g.borrowTmp(); g.movImm(one, 1)
    g.genAtomicRmw(pReg, one, true, NoA64Inst, false)  # x0 = old
    let xOld = g.borrowTmp(); g.movReg(xOld, IntRet)
    let lSkip = g.freshLabel()
    let (old, ret) = (regName xOld, regName IntRet)
    g.ab.splice &"(mov ({ret}) 0) (cmp ({old}) 0) (beq {lSkip}) (mov ({ret}) 1) (lab :{lSkip})"
    g.giveBack xOld; g.giveBack one
    if pT: g.giveBack pReg
  of "__atomic_compare_exchange_n":
    g.genAtomicCmpXchg(c)
  else:
    raiseAssert "arkham: unsupported atomic builtin: " & builtin

# ── mem* intrinsics: inline byte loops (no libc) ─────────────────────────────
# memcpy/memmove/memset/memcmp masquerade as importc calls (see programs.collect).
# arkham has no C runtime, so each lowers to a short inline AArch64 byte loop
# (register-offset ldrb/strb). Sizes are runtime values; result lands in x0
# (memcpy/memmove/memset return dest, memcmp returns the first byte difference).

proc genMemIntrin(g: var CodeGen; c: var Cursor; builtin: string) =
  ## Lower one `mem*` intrinsic call. `c` is at the first argument; this consumes
  ## all of them.
  case builtin
  of "memcpy":                                 # (dst, src, n) → dst
    let (dst, dstT) = g.genReg(c)
    let (src, srcT) = g.genReg(c)
    let (n, nT) = g.genReg(c)
    let i = g.borrowTmp()
    let b = g.borrowTmp()
    let loop = g.freshLabel()
    let done = g.freshLabel()
    g.movImm(i, 0)
    g.emLab(loop)
    g.ab.tree CmpA64: g.emReg i; g.emReg n
    g.emBr(BhsA64, done)                        # i >= n (unsigned) → done
    g.emLdrb(b, src, i)
    g.emStrb(b, dst, i)
    g.binImm(AddA64, i, 1)
    g.emBr(BA64, loop)
    g.emLab(done)
    g.movReg(IntRet, dst)                       # memcpy returns dest
    g.giveBack b; g.giveBack i
    if nT: g.giveBack n
    if srcT: g.giveBack src
    if dstT: g.giveBack dst
  of "memmove":                                # (dst, src, n) → dst; overlap-safe
    let (dst, dstT) = g.genReg(c)
    let (src, srcT) = g.genReg(c)
    let (n, nT) = g.genReg(c)
    let i = g.borrowTmp()
    let b = g.borrowTmp()
    let fwd = g.freshLabel()
    let bwd = g.freshLabel()
    let fwdLoop = g.freshLabel()
    let done = g.freshLabel()
    g.ab.tree CmpA64: g.emReg dst; g.emReg src
    g.emBr(BlsA64, fwd)                         # dst <= src → forward copy is safe
    # backward: i = n; while i != 0: i -= 1; dst[i] = src[i]
    g.movReg(i, n)
    g.emLab(bwd)
    g.ab.tree CmpA64: g.emReg i; g.ab.intLit 0
    g.emBr(BeqA64, done)
    g.binImm(SubA64, i, 1)
    g.emLdrb(b, src, i)
    g.emStrb(b, dst, i)
    g.emBr(BA64, bwd)
    # forward: i = 0; while i < n: dst[i] = src[i]; i += 1
    g.emLab(fwd)
    g.movImm(i, 0)
    g.emLab(fwdLoop)
    g.ab.tree CmpA64: g.emReg i; g.emReg n
    g.emBr(BhsA64, done)
    g.emLdrb(b, src, i)
    g.emStrb(b, dst, i)
    g.binImm(AddA64, i, 1)
    g.emBr(BA64, fwdLoop)
    g.emLab(done)
    g.movReg(IntRet, dst)
    g.giveBack b; g.giveBack i
    if nT: g.giveBack n
    if srcT: g.giveBack src
    if dstT: g.giveBack dst
  of "memset":                                 # (dst, val, n) → dst
    let (dst, dstT) = g.genReg(c)
    let (val, valT) = g.genReg(c)
    let (n, nT) = g.genReg(c)
    let i = g.borrowTmp()
    let loop = g.freshLabel()
    let done = g.freshLabel()
    g.movImm(i, 0)
    g.emLab(loop)
    g.ab.tree CmpA64: g.emReg i; g.emReg n
    g.emBr(BhsA64, done)
    g.emStrb(val, dst, i)                       # store the low byte of `val`
    g.binImm(AddA64, i, 1)
    g.emBr(BA64, loop)
    g.emLab(done)
    g.movReg(IntRet, dst)
    g.giveBack i
    if nT: g.giveBack n
    if valT: g.giveBack val
    if dstT: g.giveBack dst
  of "memcmp":                                 # (a, b, n) → first byte difference
    let (pa, paT) = g.genReg(c)
    let (pb, pbT) = g.genReg(c)
    let (n, nT) = g.genReg(c)
    let i = g.borrowTmp()
    let ba = g.borrowTmp()
    let bb = g.borrowTmp()
    let loop = g.freshLabel()
    let diff = g.freshLabel()
    let equal = g.freshLabel()
    let done = g.freshLabel()
    g.movImm(i, 0)
    g.emLab(loop)
    g.ab.tree CmpA64: g.emReg i; g.emReg n
    g.emBr(BhsA64, equal)                       # ran off the end with no diff → 0
    g.emLdrb(ba, pa, i)
    g.emLdrb(bb, pb, i)
    g.ab.tree CmpA64: g.emReg ba; g.emReg bb
    g.emBr(BneA64, diff)
    g.binImm(AddA64, i, 1)
    g.emBr(BA64, loop)
    g.emLab(diff)                               # bytes are 0..255 → signed sub gives sign
    g.movReg(IntRet, ba)
    g.binReg(SubA64, IntRet, bb)
    g.emBr(BA64, done)
    g.emLab(equal)
    g.movImm(IntRet, 0)
    g.emLab(done)
    g.giveBack bb; g.giveBack ba; g.giveBack i
    if nT: g.giveBack n
    if pbT: g.giveBack pb
    if paT: g.giveBack pa
  else:
    raiseAssert "arkham: unsupported mem intrinsic: " & builtin

proc emitCmpBranch(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool) =
  ## `c` is a comparison `(op a b)` (NO type child). Emit `cmp a, b` and branch
  ## to `toLabel` when the condition is true/false. Ordering signedness comes
  ## from the first operand's slot (unsigned var → unsigned branch).
  let ek = c.exprKind
  var tag: A64Inst
  c.into:
    if g.isFloatExpr(c):
      # Floating compare: `fcmp a, b` sets the flags so the *unsigned* branch
      # conditions match the ordered comparison (lo = a<b, ls = a<=b); NaN makes
      # them false, as required. Only `==`/`!=`/`<`/`<=` appear (NIFC normalizes).
      tag =
        case ek
        of EqC:  (if whenTrue: BeqA64 else: BneA64)
        of NeqC: (if whenTrue: BneA64 else: BeqA64)
        of LtC:  (if whenTrue: BloA64 else: BhsA64)
        of LeC:  (if whenTrue: BlsA64 else: BhiA64)
        else: raiseAssert "arkham v1: float condition not supported: " & $ek
      let fbits = g.floatBits(c)
      let (fa, fat) = g.genFReg(c, fbits)
      let (fb, fbt) = g.genFReg(c, fbits)
      g.ab.tree FcmpA64: g.emFReg(fa, fbits); g.emFReg(fb, fbits)
      if fbt: g.giveBackF fb
      if fat: g.giveBackF fa
    else:
      var signed = true
      if c.kind == Symbol and g.ra.locationOfSym(symName(c)).typ.kind == AUInt:
        signed = false
      tag =
        case ek
        of EqC:  (if whenTrue: BeqA64 else: BneA64)
        of NeqC: (if whenTrue: BneA64 else: BeqA64)
        of LtC:  (if whenTrue: (if signed: BltA64 else: BloA64)
                  else:        (if signed: BgeA64 else: BhsA64))
        of LeC:  (if whenTrue: (if signed: BleA64 else: BlsA64)
                  else:        (if signed: BgtA64 else: BhiA64))
        else: raiseAssert "arkham v1: condition not supported: " & $ek
      let a = g.genReg(c)
      var bImm = false
      var bImmV = 0'i64
      var b = (r: NoReg, temp: false)
      if c.kind == IntLit and intVal(c) >= 0 and intVal(c) <= 0xFFFF:
        bImm = true; bImmV = intVal(c); inc c
      else:
        b = g.genReg(c)
      g.ab.tree CmpA64:
        g.emReg a.r
        if bImm: g.ab.intLit bImmV else: g.emReg b.r
      if not bImm and b.temp: g.giveBack b.r
      if a.temp: g.giveBack a.r
  g.emBr(tag, toLabel)

proc emitCondJump(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool) =
  ## Short-circuit conditional jump: emit code that branches to `toLabel` when
  ## the condition `c` evaluates to `whenTrue`. Handles and/or/not, comparisons,
  ## and a plain boolean value (`cmp v, #0`).
  if c.kind == TagLit:
    case c.exprKind
    of AndC:
      c.into:
        if whenTrue:                          # a&&b true: a false skips, b decides
          let lSkip = g.freshLabel()
          g.emitCondJump(c, lSkip, false)
          g.emitCondJump(c, toLabel, true)
          g.emLab(lSkip)
        else:                                 # a&&b false: either false → jump
          g.emitCondJump(c, toLabel, false)
          g.emitCondJump(c, toLabel, false)
      return
    of OrC:
      c.into:
        if whenTrue:                          # a||b true: either true → jump
          g.emitCondJump(c, toLabel, true)
          g.emitCondJump(c, toLabel, true)
        else:                                 # a||b false: a true skips, b decides
          let lSkip = g.freshLabel()
          g.emitCondJump(c, lSkip, true)
          g.emitCondJump(c, toLabel, false)
          g.emLab(lSkip)
      return
    of NotC:
      c.into:
        g.emitCondJump(c, toLabel, not whenTrue)
      return
    of EqC, NeqC, LtC, LeC:
      g.emitCmpBranch(c, toLabel, whenTrue)
      return
    else: discard
  # a plain boolean value: branch on `v != 0` / `v == 0`
  let (r, t) = g.genReg(c)
  g.ab.tree CmpA64: (g.emReg r; g.ab.intLit 0)
  g.emBr(if whenTrue: BneA64 else: BeqA64, toLabel)
  if t: g.giveBack r

proc materializeCond(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## Spill a pending condition into `dest` as a `0`/`1` boolean — TCC's `gv()`
  ## on a `VT_CMP` value. The compare itself (and short-circuit `&&`/`||`) is
  ## shared with the branch path via `emitCondJump`: assume true, jump over the
  ## reset when the condition holds, otherwise fall through to `dest ← 0`.
  let lEnd = g.freshLabel()
  g.movImm(dest, 1)
  g.emitCondJump(c, lEnd, whenTrue = true)
  g.movImm(dest, 0)
  g.emLab(lEnd)

proc emitChain(g: var CodeGen; c: var Cursor; lEnd: string) =
  if not c.hasMore: return
  case c.substructureKind
  of ElifU:
    var branch = c
    skip c                                  # `c` → the rest of the chain
    let lNext = g.freshLabel()
    branch.into:
      g.emitCondJump(branch, lNext, whenTrue = false)
      g.genActionStmts(branch)
      g.emBr(BA64, lEnd)
    g.emLab(lNext)
    g.emitChain(c, lEnd)
  of ElseU:
    c.into:
      g.genActionStmts(c)
  else:
    skip c

proc genIf(g: var CodeGen; c: var Cursor) =
  let lEnd = g.freshLabel()
  c.into:
    g.emitChain(c, lEnd)
  g.emLab(lEnd)

proc genWhile(g: var CodeGen; c: var Cursor) =
  ## Lstart: cmp; b<false> Lend; body; b Lstart; Lend:
  let lStart = g.freshLabel()
  let lEnd = g.freshLabel()
  g.loopEnds.add lEnd                       # so an inner `(break)` targets Lend
  c.into:
    let condStart = c
    skip c                                  # `c` → first body statement
    g.emLab(lStart)
    var cond = condStart
    g.emitCondJump(cond, lEnd, whenTrue = false)
    while c.hasMore: genStmt(g, c)
    g.emBr(BA64, lStart)
  g.emLab(lEnd)
  discard g.loopEnds.pop()

proc genBreak(g: var CodeGen; c: var Cursor) =
  ## `(break)` → unconditional jump to the enclosing loop's end label.
  assert g.loopEnds.len > 0, "arkham v1: `break` outside a loop"
  g.emBr(BA64, g.loopEnds[^1])
  skip c

# ── case statement ──────────────────────────────────────────────────────────

proc branchImm(c: var Cursor): int64 =
  ## Read a NIFC `BranchValue` (Number | CharLiteral | (true) | (false)) and
  ## advance past it. Symbol branch values (enum consts) are unsupported in v1.
  case c.kind
  of IntLit:  result = intVal(c); inc c
  of UIntLit: result = cast[int64](uintVal(c)); inc c
  of CharLit: result = int64(ord(charLit(c))); inc c
  of TagLit:
    case c.exprKind
    of TrueC:  result = 1; skip c
    of FalseC: result = 0; skip c
    else: raiseAssert "arkham v1: unsupported case branch value: " & $c.exprKind
  else: raiseAssert "arkham v1: unsupported case branch value kind: " & $c.kind

proc cmpImm(g: var CodeGen; selReg: Reg; v: int64) =
  ## `cmp selReg, #v` — immediate when it fits, otherwise via a scratch register.
  if v >= 0 and v <= 0xFFFF:
    g.ab.tree CmpA64: g.emReg selReg; g.ab.intLit v
  else:
    let tmp = g.borrowTmp()
    g.movImm(tmp, v)
    g.ab.tree CmpA64: g.emReg selReg; g.emReg tmp
    g.giveBack tmp

proc emitRangeTest(g: var CodeGen; selReg: Reg; c: var Cursor;
                   lBody: string; signed: bool) =
  ## Emit a test for one `BranchRange` against `selReg`, branching to `lBody`
  ## when it matches. `c` is advanced past the range.
  if c.kind == TagLit and c.substructureKind == RangeU:
    c.into:
      let lo = branchImm(c)
      let hi = branchImm(c)
      let lSkip = g.freshLabel()            # in-range: lo <= sel <= hi
      g.cmpImm(selReg, lo)
      g.emBr(if signed: BltA64 else: BloA64, lSkip)
      g.cmpImm(selReg, hi)
      g.emBr(if signed: BgtA64 else: BhiA64, lSkip)
      g.emBr(BA64, lBody)
      g.emLab(lSkip)
  else:
    g.cmpImm(selReg, branchImm(c))
    g.emBr(BeqA64, lBody)

proc genCase(g: var CodeGen; c: var Cursor) =
  ## `(case Expr (of (ranges BranchRange+) StmtList)* (else StmtList)?)`.
  ## Selector → a register; per-branch comparison tests jump to the branch body;
  ## a non-match falls through to the `else` body (or the end). NIFC `case` has
  ## no fall-through, so each body ends with a jump to the end label.
  let lEnd = g.freshLabel()
  c.into:
    # Selector signedness drives ordered range comparisons.
    var signed = true
    if c.kind == Symbol and g.ra.locationOfSym(symName(c)).typ.kind == AUInt:
      signed = false
    let (selReg, selTemp) = g.genReg(c)     # selector value, live across all tests
    # Pass 1: emit the comparison tests; remember each body's StmtList cursor.
    var bodies: seq[(string, Cursor)] = @[]
    var elseBody = c                        # placeholder; overwritten if an `else` exists
    var hasElse = false
    while c.hasMore:
      case c.substructureKind
      of OfU:
        let lBody = g.freshLabel()
        var branch = c
        skip c
        branch.into:                        # branch → (ranges …) then StmtList
          assert branch.substructureKind == RangesU, "arkham: case `of` needs `ranges`"
          branch.into:
            while branch.hasMore:
              g.emitRangeTest(selReg, branch, lBody, signed)
          bodies.add (lBody, branch)        # branch now at the StmtList (copy saved)
          skip branch                       # consume it so the outer `into` balances
      of ElseU:
        elseBody = c
        hasElse = true
        skip c
      else: skip c
    if selTemp: g.giveBack selReg
    # No match falls through here: run the else body (if any), then skip bodies.
    if hasElse:
      elseBody.into:
        g.genActionStmts(elseBody)
    g.emBr(BA64, lEnd)
    # Pass 2: emit each branch body.
    for (lBody, bc) in bodies:
      g.emLab(lBody)
      var body = bc
      g.genActionStmts(body)
      g.emBr(BA64, lEnd)
  g.emLab(lEnd)

proc aggrAddr(g: var CodeGen; c: var Cursor): tuple[r: Reg, temp: bool] =
  ## Address of an aggregate lvalue → a register, consuming the lvalue cursor. A
  ## by-reference param (InReg aggregate) already *is* the address; otherwise
  ## borrow a temp and `genAddr` (stack var → lea, global → adr, dot/at → lea).
  if c.kind == Symbol:
    let loc = g.ra.locationOfSym(symName(c))
    if loc.kind == InReg:
      result = (loc.r, false); inc c; return
  let r = g.borrowTmp()
  g.genAddr(c, r)
  result = (r, true)

proc byteCopyConst(g: var CodeGen; dst, src: Reg; size: int) =
  ## `dst[0..<size] ← src[0..<size]` — the same inline byte loop as `memcpy`
  ## (register-offset ldrb/strb), with `size` a compile-time constant. Used for
  ## whole-aggregate assignment / copy-init; `dst`/`src` stay live.
  let i = g.borrowTmp()
  let b = g.borrowTmp()
  let loop = g.freshLabel()
  let done = g.freshLabel()
  g.movImm(i, 0)
  g.emLab(loop)
  g.cmpImm(i, size)
  g.emBr(BhsA64, done)                          # i >= size (unsigned) → done
  g.emLdrb(b, src, i)
  g.emStrb(b, dst, i)
  g.binImm(AddA64, i, 1)
  g.emBr(BA64, loop)
  g.emLab(done)
  g.giveBack b; g.giveBack i

proc genAsgn(g: var CodeGen; c: var Cursor) =
  ## `(asgn lvalue rvalue)`. The lvalue's type (via `getType`/`exprSlot`) decides
  ## float vs integer — uniformly, with no per-form special-casing; the lvalue's
  ## *shape* decides only the address operand (register / global / dot / deref /
  ## at). A float lvalue stores with `fstr`, an integer one with `mov`.
  c.into:
    let slot = g.exprSlot(c)                # the lvalue's type → float / int / aggregate
    if slot.kind == AMem:                   # whole-aggregate copy (any size)
      let (dstA, dstT) = g.aggrAddr(c)      # &lvalue (consumes the lvalue)
      let (srcA, srcT) = g.aggrAddr(c)      # &rvalue (an aggregate lvalue)
      g.byteCopyConst(dstA, srcA, slot.size)
      if srcT: g.giveBack srcA
      if dstT: g.giveBack dstA
    else:
      let l = g.asLvalue(c)              # classify + consume lvalue; c → rvalue
      let bits = if slot.size == 4: 32 else: 64
      case l.kind
      of lvReg: g.genInto(c, l.r)           # compute the rhs straight into the home reg
      of lvFReg: g.genIntoF(c, l.f, bits)
      else:                                 # a memory destination: one store pair
        if slot.kind == AFloat:
          let (fr, ft) = g.genFReg(c, bits)
          g.emitStoreF(l, fr, bits)
          if ft: g.giveBackF fr
        else:
          let (rr, rt) = g.genReg(c)
          g.emitStore(l, rr)
          if rt: g.giveBack rr

proc genStmt(g: var CodeGen; c: var Cursor) =
  case c.stmtKind
  of StmtsS:
    c.into:
      while c.hasMore: genStmt(g, c)
  of VarS, GvarS, TvarS, ConstS:
    genVarDecl(g, c)
  of CallS:
    genCall(g, c)                           # statement call: result discarded
  of AsgnS:
    genAsgn(g, c)
  of IfS:
    genIf(g, c)
  of WhileS:
    genWhile(g, c)
  of CaseS:
    genCase(g, c)
  of BreakS:
    genBreak(g, c)
  of ScopeS:
    c.into:
      while c.hasMore: genStmt(g, c)
  of RetS:
    # (ret e?): place the value in x0 (or x0:x1 for a ≤16B aggregate), restore
    # the frame, then return.
    c.into:
      if c.hasMore and c.kind != DotToken:
        if g.retIndirect:
          # >16B: copy the result into the caller's buffer (x19) and return its
          # address in x0 (AAPCS64: x8 = result address, also returned in x0).
          assert c.kind == Symbol, "arkham v1: aggregate ret value must be a local"
          g.copyStructThroughPtr(symName(c), g.retAggrName, g.indirectReg)
          g.movReg(IntRet, g.indirectReg)
          inc c
        elif g.retAggrName.len > 0:
          assert c.kind == Symbol, "arkham v1: aggregate ret value must be a local"
          g.structToRegs(symName(c), g.retAggrName, 0)
          inc c
        elif g.retIsFloat:
          g.genIntoF(c, FloatRet, g.retFloatBits)   # float result in v0
        else:
          g.genInto(c, IntRet)
    if g.ra.hasStackVars:                     # release nifasm-managed slots
      g.ab.tree AddA64: g.emReg SP; g.ab.keyword SsizeX
    if g.hasFrame: framePop(g)
    g.ab.keyword RetA64
  else:
    raiseAssert "arkham v1: statement not supported: " & $c.stmtKind

# ── proc emission ────────────────────────────────────────────────────────────

proc initFreeTmp(g: var CodeGen) =
  g.freeTmp = {}
  for r in IntTempRegs: g.freeTmp.incl r
  g.freeFTmp = {}
  for f in FloatTempRegs: g.freeFTmp.incl f
  for name, pos in g.ra.symPos:               # locals occupying a volatile reg
    let loc = g.ra.locs[pos]
    if loc.kind == InReg: g.freeTmp.excl loc.r
    elif loc.kind == InFReg: g.freeFTmp.excl loc.f   # held by a float local/param

proc computeFrame(g: var CodeGen; hasCall: bool) =
  g.frameRegs = @[]
  for r in IntCalleeSaved:
    if r in g.ra.usedCallee: g.frameRegs.add r
  if g.frameRegs.len mod 2 == 1:              # save in pairs → pad to even
    for r in IntCalleeSaved:
      if r notin g.ra.usedCallee: (g.frameRegs.add r; break)
  g.frameFRegs = @[]
  for f in FloatCalleeSaved:
    if f in g.ra.usedCalleeF: g.frameFRegs.add f
  if g.frameFRegs.len mod 2 == 1:             # pad SIMD saves to an even count too
    for f in FloatCalleeSaved:
      if f notin g.ra.usedCalleeF: (g.frameFRegs.add f; break)
  g.hasFrame = hasCall or g.frameRegs.len > 0 or g.frameFRegs.len > 0

proc emitStackParamLoads(g: var CodeGen; decl: Cursor) =
  ## Load the incoming stack-passed parameters (the 9th integer/pointer arg
  ## onward) from the caller's outgoing argument area into their register homes.
  ## Emitted right after `framePush` and *before* SP is lowered for locals, so
  ## each arg sits at the statically-known offset `framePushBytes + k*8` from the
  ## current SP (the caller left SP pointing at the first stack arg on entry).
  var c = decl
  inc c                                       # proc head → name
  inc c                                       # name → params slot
  if c.kind != TagLit: return                 # (params) is `.` → no parameters
  let base = g.framePushBytes()
  var idx = 0
  var fidx = 0
  var stackOrd = 0
  c.into:
    while c.hasMore:
      var nm = ""
      var isFloat = false
      c.into:                                 # (param :name pragmas type)
        nm = symName(c); inc c
        skip c                                # pragmas
        if c.kind == TagLit and c.typeKind == FT: isFloat = true
        while c.hasMore: skip c               # type (+ anything else)
      if isFloat:
        inc fidx                              # floats use v0–v7; never stack here
      else:
        if idx >= IntArgRegs.len:
          let loc = g.ra.locationOfSym(nm)
          assert loc.kind == InReg,
            "arkham v1: stack parameter without a register home: " & nm
          g.ab.tree MovA64:                   # home ← [sp + base + stackOrd*8]
            g.emReg loc.r
            g.ab.tree MemX:
              g.emReg SP
              g.ab.intLit (base + stackOrd * 8)
          inc stackOrd
        inc idx

proc emitParamMoves(g: var CodeGen; decl: Cursor) =
  ## Move each parameter from its incoming ABI register to the home the
  ## allocator chose (a callee-saved register for cross-call params; arg regs
  ## stay put for leaf procs). Emitted after the prologue saved the homes.
  ## Stack-passed params (9th integer arg onward) are loaded separately by
  ## `emitStackParamLoads` and skipped here.
  var c = decl
  inc c                                       # proc head → name
  inc c                                       # name → params slot
  if c.kind != TagLit: return                 # (params) is `.` → no parameters
  var idx = 0
  var fidx = 0
  c.into:                                     # into (params …)
    while c.hasMore:
      var nm = ""
      var tn = ""
      c.into:                                 # (param :name pragmas type)
        nm = symName(c); inc c
        skip c                                # pragmas
        g.symType[nm] = c                     # record the param's type for getType
        # Only true aggregates get a `varType` entry; a named *enum* (or scalar
        # typedef), local or cross-module, resolves to a scalar and stays in the
        # register path. `slotOf` loads a foreign module if the type lives there.
        if c.kind == Symbol and slotOf(g.prog, c).kind == AMem: tn = symName(c)
        while c.hasMore: skip c               # type (+ anything else)
      let loc = g.ra.locationOfSym(nm)
      if tn.len > 0 and loc.kind == NamedStack:
        # ≤16B by-value aggregate: declare its stack home, fill from its GPR(s)
        g.varType[nm] = tn
        g.emStackVar(nm, tn)
        g.regsToStruct(nm, tn, idx)
        idx += aggrWordCount(g.prog, tn)
      elif tn.len > 0 and loc.kind == InReg:
        # >16B by-reference aggregate: a pointer, homed like a scalar; field
        # accesses route through it (recorded in varType).
        g.varType[nm] = tn
        g.movReg(loc.r, IntArgRegs[idx])
        inc idx
      elif loc.kind == InFReg:
        # Float parameter: in a leaf proc it stays in its incoming v{fidx}; if
        # the allocator gave it a callee-saved home, move it there.
        g.fmovF(loc.f, FloatArgRegs[fidx], loc.typ.size * 8)
        inc fidx
      elif loc.kind == NamedStack and loc.typ.kind == AFloat:
        # An address-taken / spilled float param: declare its `(s) (f N)` slot and
        # spill the incoming SIMD arg register into it so `addr`/loads/stores work.
        assert fidx < FloatArgRegs.len, "arkham v1: >8 float params (stack TODO)"
        let bits = loc.typ.size * 8
        g.emFloatStackVar(nm, bits)
        g.emFloatScalarStore(nm, FloatArgRegs[fidx], bits)
        inc fidx
      elif loc.kind == NamedStack:
        # An address-taken scalar param: declare its `(s)` slot and spill the
        # incoming argument register into it so `addr`/loads/stores work.
        assert idx < IntArgRegs.len, "arkham v1: >8 integer params (stack TODO)"
        g.emScalarStackVar(nm)
        g.emScalarStore(nm, IntArgRegs[idx])
        inc idx
      else:
        case loc.kind
        of InReg:
          if idx < IntArgRegs.len:
            g.movReg(loc.r, IntArgRegs[idx])
          # else: a stack-passed param — already loaded into loc.r by
          # emitStackParamLoads before SP was lowered. Nothing to move.
        else: raiseAssert "arkham v1: stack-resident parameter: " & nm
        inc idx

proc emitSignature(g: var CodeGen; decl: Cursor; declarative: bool) =
  ## Emit the proc's `(params)/(result)/(clobber)`. When `declarative`, the ABI
  ## is stated explicitly — positional `p{i}` register params and an `x0` result
  ## — so nifasm cross-checks every call site; otherwise both stay empty and
  ## arkham marshals by hand (floats/aggregates/by-ref/>8/named types). The
  ## clobber set is always the convention's, derived here (never per-proc
  ## precomputed), which is reliable across modules.
  if declarative:
    var c = decl
    c.into:
      inc c                                   # name → params slot
      g.ab.tree ParamsD:
        if c.kind == TagLit:                  # (params (param …) …)
          var idx = 0
          c.into:
            while c.hasMore:
              c.into:                         # (param :name pragmas type)
                inc c                         # name → use positional p{idx}
                skip c                        # pragmas
                g.ab.tree ParamD:
                  g.ab.symDef ("p." & $idx)
                  if idx < IntArgRegs.len:
                    g.emReg IntArgRegs[idx]   # x0–x7
                  else:
                    g.ab.keyword SO           # 9th+ → stack-passed `(s)`
                  g.genTypeBody(c)            # the param type (consumes it)
                while c.hasMore: skip c
              inc idx
        else:
          skip c                              # no params slot → consume it
      g.ab.tree ResultD:                      # c now at the return type
        if retIsVoid(c):
          skip c                              # void → empty (result)
        else:
          g.ab.symDef "ret.0"
          g.emReg IntRet
          g.genTypeBody(c)                    # the result type (consumes it)
      while c.hasMore: skip c                 # pragmas, body
  else:
    g.ab.keyword ParamsD
    g.ab.keyword ResultD
  g.ab.tree ClobberD:
    for r in ConvClobbersGpr: g.emReg r

proc emitGlobalInits(g: var CodeGen)

proc genProc(g: var CodeGen; info: ProcInfo) =
  let an = analyseProc(info.decl, g.tvarNames)
  g.varType = initTable[string, string]()     # per-proc (symbol names recycle)
  g.symType = initTable[string, Cursor]()
  # Determine the aggregate return convention BEFORE allocation: a named object
  # ≤16B → x0[:x1]; >16B → x8 indirect result (callee writes through the caller-
  # supplied pointer, which we park in x19 for the proc's lifetime).
  g.retAggrName = ""
  g.retIndirect = false
  g.retIsFloat = false
  g.indirectReg = NoReg
  block:
    var rc = info.decl
    inc rc; inc rc; skip rc                   # head → name → params, skip → return type
    # A named *aggregate* return uses the x0[:x1] / x8 ABI; a named enum/scalar
    # return resolves to a scalar and stays in x0 (handled by the default path).
    if rc.kind == Symbol and slotOf(g.prog, rc).kind == AMem:
      g.retAggrName = symName(rc)
      g.retIndirect = aggrByteSize(g.prog, g.retAggrName) > 16
    elif rc.kind == TagLit and rc.typeKind == FT:
      g.retIsFloat = true                     # float return → v0
      g.retFloatBits = if slotOf(g.prog, rc).size == 4: 32 else: 64
  let preseal = if g.retIndirect: {X19} else: {}
  g.ra = allocateProc(g.buf[], info.decl, an, g.prog, preseal)
  if g.retIndirect:
    g.indirectReg = X19
    g.ra.usedCallee.incl X19                  # saved/restored like any callee reg
  g.initFreeTmp()
  g.computeFrame(an.hasCall)
  g.ab.tree ProcD:
    g.ab.symDef info.asmName                  # "main.0" for the entry
    g.emitSignature(info.decl, isDeclarativeAbi(info.decl))
    g.ab.tree StmtsA64:
      if g.hasFrame: framePush(g)
      # Read incoming stack args while SP still points at the caller's argument
      # area (after the pushes, before locals lower SP).
      g.emitStackParamLoads(info.decl)
      if g.ra.hasStackVars:                   # reserve nifasm-managed slots
        g.ab.tree SubA64: g.emReg SP; g.ab.keyword SsizeX
      if g.retIndirect:                       # park the x8 result pointer
        g.movReg(g.indirectReg, IndirectResultReg)
      g.emitParamMoves(info.decl)
      if info.isEntry: g.emitGlobalInits()    # run module-level var initializers
      var c = info.decl
      c.into:
        inc c; skip c; skip c; skip c         # name, params, return type, pragmas
        if c.stmtKind == StmtsS:
          c.into:
            while c.hasMore: genStmt(g, c)
      # Fallthrough epilogue: a void proc whose body has no explicit `(ret)`
      # must still restore the frame and return (unreachable dead code after an
      # explicit `ret`, harmless).
      if g.ra.hasStackVars:
        g.ab.tree AddA64: g.emReg SP; g.ab.keyword SsizeX
      if g.hasFrame: framePop(g)
      g.ab.keyword RetA64

# ── driver ──────────────────────────────────────────────────────────────────

proc genType(g: var CodeGen; name: string; decl: Cursor) =
  ## Emit `(type :name <translated body>)` — a top-level type definition that
  ## nifasm's stack-slot allocator consults for aggregate field offsets.
  var c = decl
  c.into:                                     # (type SymbolDef TypePragmas body)
    inc c                                     # name
    skip c                                    # TypePragmas (one slot: `.` or (pragmas …))
    g.ab.tree TypeD:
      g.ab.symDef name
      g.genTypeBody(c)

proc genGlobal(g: var CodeGen; name: string; decl: Cursor) =
  ## Emit `(gvar :name <type>)` — a zero-initialized `.bss` global (also used
  ## for `const`; any initializer is run at entry by `emitGlobalInits`).
  var c = decl
  c.into:                                     # (gvar SymbolDef VarPragmas Type Value?)
    inc c                                     # name
    skip c                                    # pragmas
    g.ab.open NifasmDecl.GvarD
    g.ab.symDef name
    g.genTypeBody(c)                          # type
    g.ab.close()
    while c.hasMore: skip c                   # value (initialized at entry)

proc genTvar(g: var CodeGen; name: string; decl: Cursor) =
  ## Emit `(tvar :name <type> <intlit>?)` — a macOS thread-local variable. A
  ## literal initializer is baked into the per-thread template dyld copies on
  ## first access; non-literal initializers are unsupported (a thread-local is
  ## per-thread, so the entry-time `emitGlobalInits` path cannot serve them).
  var c = decl
  c.into:                                     # (tvar SymbolDef VarPragmas Type Value?)
    inc c                                     # name
    skip c                                    # pragmas
    g.ab.open NifasmDecl.TvarD
    g.ab.symDef name
    g.genTypeBody(c)                          # type
    if c.kind == IntLit:
      g.ab.intLit intVal(c)                   # literal initializer → TLV template
    elif c.kind != DotToken:
      raiseAssert "arkham: thread-local initializer must be an integer literal: " & name
    g.ab.close()
    while c.hasMore: skip c

proc emitGlobalInits(g: var CodeGen) =
  ## At program entry, store each global's initializer (if any) into its slot.
  for name, decl in g.globals:
    var c = decl
    c.into:
      inc c; skip c                           # name, pragmas
      let gslot = slotOf(g.prog, c)           # the global's declared type
      let gbits = if gslot.size == 4: 32 else: 64
      skip c                                   # type
      if c.hasMore and c.kind != DotToken:
        if gslot.kind == AFloat:               # float global: store via fstr
          let fv = g.borrowFTmp()
          g.genIntoF(c, fv, gbits)
          let p = g.borrowTmp()
          g.emAdr(p, name)
          g.emFStore(fv, p, gbits)
          g.giveBack p
          g.giveBackF fv
        else:
          let v = g.borrowTmp()
          g.genInto(c, v)                       # evaluate initializer
          let p = g.borrowTmp()
          g.emAdr(p, name)
          g.ab.tree MovA64:
            g.ab.tree MemX: g.emReg p
            g.emReg v
          g.giveBack p
          g.giveBack v
      while c.hasMore: skip c

proc generate*(buf: var TokenBuf; inputPath: string; tags: TagPool): string =
  ## Compile a parsed NIFC module to AArch64 / Darwin asm-NIF text. `inputPath`
  ## and `tags` let the program model load *other* modules on demand to resolve
  ## cross-module symbols (`Foo.0.othermod`).
  var g = CodeGen(ab: initAsmBuf(), buf: addr buf)
  g.prog = collect(buf, inputPath, tags)
  g.callTarget = g.prog.callTarget
  g.globals = g.prog.globals
  g.tvars = g.prog.tvars
  for nm in g.tvars.keys: g.tvarNames.incl nm
  g.ab.tree StmtsA64:
    g.ab.tree ArchD: g.ab.ident "arm64"
    # Thread-local vars resolve their TLV descriptor thunk against libSystem
    # (`__tlv_bootstrap`), so the dylib must be loaded even without extern calls.
    if g.prog.needsLibSystem or g.tvars.len > 0:
      g.ab.tree ImpD: g.ab.str DarwinLibSystem
    for ex in g.prog.externOrder:
      g.ab.tree ExtprocD:
        g.ab.symDef ex.asmName
        g.ab.str ex.extName
    for (name, decl) in g.prog.mainTypeList:
      g.genType(name, decl)
    for name, decl in g.prog.globals:
      g.genGlobal(name, decl)
    for name, decl in g.prog.tvars:
      g.genTvar(name, decl)
    for info in g.prog.procs:
      genProc(g, info)
    # NOTE: foreign types are NOT emitted here. arkham loads other modules only to
    # resolve their layout for *its own* codegen (sizing, field offsets, ABI). The
    # actual cross-module linking is nifasm's job: a module-suffixed symbol like
    # `Foo.0.othermod` makes nifasm auto-import `othermod.s.nif` (which arkham
    # produced when it compiled that module). Emitting the decl inline is ignored.
    for (nm, bytes) in g.rodata:
      g.ab.tree RodataD:
        g.ab.symDef nm
        g.ab.str bytes
  result = g.ab.render()

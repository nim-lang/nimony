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

import std / [assertions, tables]
import nifcore, nifcdecl
import slots, machine, analyser, register_allocator
import asmbuf

const DarwinLibSystem = "/usr/lib/libSystem.B.dylib"

type
  Extern = object
    asmName, extName: string

  CallTarget = object
    asmName: string         ## the asm-NIF symbol to call
    extern: bool            ## true → (extcall), false → (call)

  ProcInfo = object
    asmName: string         ## the proc's asm-NIF name (entry → "main.0")
    decl: Cursor            ## the `(proc …)` declaration
    isEntry: bool

  Program* = object
    externOrder: seq[Extern]               ## extproc decls, in order
    callTarget: Table[string, CallTarget]  ## NIFC proc symbol → how to call it
    procs: seq[ProcInfo]                    ## internal procs to emit (entry first)
    typeDecls: Table[string, Cursor]        ## type name → its `(type …)` decl
    globals: Table[string, Cursor]          ## global var name → its decl cursor
    needsLibSystem: bool

  CodeGen = object
    ab: AsmBuf
    ra: RegAlloc
    buf: ptr TokenBuf
    callTarget: Table[string, CallTarget]
    typeDecls: Table[string, Cursor]        ## type name → its `(type …)` decl
    globals: Table[string, Cursor]          ## global var name → its decl cursor
    freeTmp: set[Reg]                       ## volatile temps free for scratch
    rodata: seq[(string, string)]           ## module-level string literals
    hasFrame: bool                          ## current proc needs a stack frame
    frameRegs: seq[Reg]                     ## callee-saved regs to save (even count)
    labelCount: int                         ## fresh-label counter
    retAggrName: string                     ## current proc's aggregate return type (or "")
    retIndirect: bool                       ## return type is >16B (x8 indirect result)
    indirectReg: Reg                        ## callee-saved reg holding the x8 dest pointer
    varType: Table[string, string]          ## aggregate var/param name → its type name

# ── pass 0: collect procs (entry / internal / external) ─────────────────────

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

proc collect(buf: var TokenBuf): Program =
  result = Program(callTarget: initTable[string, CallTarget](),
                   typeDecls: initTable[string, Cursor](),
                   globals: initTable[string, Cursor]())
  var c = buf.beginRead()
  assert c.stmtKind == StmtsS, "NIFC top level must be (stmts …)"
  c.into:
    while c.hasMore:
      if c.stmtKind in {GvarS, TvarS, ConstS}:
        let gStart = c
        var gc = c
        gc.into:
          result.globals[symName(gc)] = gStart
          while gc.hasMore: skip gc           # drain so `into` stays balanced
        skip c
      elif c.stmtKind == TypeS:
        let typeStart = c
        var tc = c
        tc.into:
          result.typeDecls[symName(tc)] = typeStart
          while tc.hasMore: skip tc           # drain so `into` stays balanced
        skip c
      elif c.stmtKind == ProcS:
        let procStart = c
        var pname, importcN, exportcN = ""
        c.into:
          pname = symName(c); inc c           # name
          skip c                              # params
          skip c                              # return type
          parsePragmas(c, importcN, exportcN)
          skip c                              # body
        if importcN.len > 0:
          let asmN = importcN & ".0"
          result.externOrder.add Extern(asmName: asmN, extName: "_" & importcN)
          result.callTarget[pname] = CallTarget(asmName: asmN, extern: true)
          result.needsLibSystem = true
        else:
          let entry = exportcN.len > 0
          let asmN = if entry: "main.0" else: pname
          result.callTarget[pname] = CallTarget(asmName: asmN, extern: false)
          result.procs.add ProcInfo(asmName: asmN, decl: procStart, isEntry: entry)
      else:
        skip c
  # Emit the entry proc first so it begins the text section.
  for i in 0 ..< result.procs.len:
    if result.procs[i].isEntry and i != 0:
      swap result.procs[0], result.procs[i]
      break

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

proc emPair(g: var CodeGen; op: A64Inst; r1, r2: Reg; off: int) =
  g.ab.tree op: g.emReg r1; g.emReg r2; g.emReg SP; g.ab.intLit off

proc framePush(g: var CodeGen) =
  ## Push fp/lr and the used callee-saved registers as a LIFO stack of pairs.
  g.emPair(StpA64, FP, LR, -16)
  var i = 0
  while i < g.frameRegs.len:
    g.emPair(StpA64, g.frameRegs[i], g.frameRegs[i+1], -16)
    i += 2

proc framePop(g: var CodeGen) =
  ## Restore in reverse (post-index `ldp`), callee-saved first then fp/lr.
  var i = g.frameRegs.len - 2
  while i >= 0:
    g.emPair(LdpA64, g.frameRegs[i], g.frameRegs[i+1], 16)
    i -= 2
  g.emPair(LdpA64, FP, LR, 16)

# ── scratch register pool (volatile temps not held by a local) ──────────────

proc borrowTmp(g: var CodeGen): Reg =
  for r in IntTempRegs:
    if r in g.freeTmp and not g.ra.isSealed(r):
      excl g.freeTmp, r
      return r
  raiseAssert "arkham v1: out of scratch registers"

proc giveBack(g: var CodeGen; r: Reg) =
  if r in IntTempRegs: g.freeTmp.incl r

# ── expressions: target-into-register ───────────────────────────────────────

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg)
proc genCall(g: var CodeGen; c: var Cursor)
proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg)
proc genReg(g: var CodeGen; c: var Cursor): tuple[r: Reg, temp: bool]
proc structToRegs(g: var CodeGen; varName, typeName: string; firstArg: int)
proc regsToStruct(g: var CodeGen; varName, typeName: string; firstArg: int)

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

proc emDotMem(g: var CodeGen; c: var Cursor) =
  ## Consume a NIFC `(dot base field …)` and emit its memory operand.
  var base, field: string
  c.into:
    base = symName(c); inc c
    field = symName(c); inc c
    while c.hasMore: skip c                  # depth / access selector
  g.emAggrFieldMem(base, field)

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
  ## use the `lsl #(64-w); asr|lsr #(64-w)` shift pair (immediate shifts).
  if width <= 0 or width >= 64: return
  let sh = int64(64 - width)
  g.binImm(LslA64, dest, sh)
  g.binImm(if signed: AsrA64 else: LsrA64, dest, sh)

proc genConv(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(conv Type Expr)` — numeric conversion: evaluate, then re-represent in the
  ## target type (truncate + sign/zero-extend). v1: integer/char targets.
  c.into:
    let typeCur = c
    let signed = isSignedType(typeCur)
    let width = intTypeWidth(typeCur)
    skip c                                  # target type
    g.genInto(c, dest)                      # value → dest
    g.extendTo(dest, width, signed)

proc genCast(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `(cast Type Expr)` — reinterpret the bits unchanged: just evaluate.
  c.into:
    skip c                                  # target type
    g.genInto(c, dest)

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg) =
  case c.kind
  of IntLit:
    g.movImm(dest, intVal(c)); inc c
  of StrLit:
    let nm = "msg." & $g.rodata.len
    g.rodata.add (nm, strVal(c))
    g.emAdr(dest, nm); inc c
  of Symbol:
    let nm = symName(c)
    if g.globals.hasKey(nm):                  # global: dest ← [&g]
      let tmp = g.borrowTmp()
      g.emAdr(tmp, nm)
      g.ab.tree MovA64:
        g.emReg dest
        g.ab.tree MemX: g.emReg tmp
      g.giveBack tmp
    else:
      let loc = g.ra.locationOfSym(nm)
      case loc.kind
      of InReg: g.movReg(dest, loc.r)
      else: raiseAssert "arkham v1: symbol not in a register: " & nm
    inc c
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
    of ConvC: g.genConv(c, dest)
    of CastC: g.genCast(c, dest)
    of CallC:
      g.genCall(c)                          # result lands in x0 …
      g.movReg(dest, IntRet)                # … move it to the destination
    of DotC:
      g.ab.tree MovA64:                     # field load: dest ← [base+offset]
        g.emReg dest
        g.emDotMem(c)
    of DerefC:                              # `(deref p)` → dest ← [p]
      var pr = NoReg
      var pt = false
      c.into:
        let (r, t) = g.genReg(c); pr = r; pt = t
        while c.hasMore: skip c             # (cppref)?
      g.ab.tree MovA64:
        g.emReg dest
        g.ab.tree MemX: g.emReg pr
      if pt: g.giveBack pr
    of AddrC:                               # `(addr lvalue)` → dest ← &lvalue
      c.into:
        g.genAddr(c, dest)
        while c.hasMore: skip c             # (cppref)?
    of AtC:                                 # `(at arr idx)` → dest ← arr[idx]
      g.ab.tree MovA64:
        g.emReg dest
        g.ab.tree MemX: g.emAt(c)
    else: raiseAssert "arkham v1: expression not supported: " & $c.exprKind
  else:
    raiseAssert "arkham v1: operand not supported: " & $c.kind

proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `dest ← &lvalue`, with `c` positioned at the lvalue.
  if c.kind == Symbol:
    if g.globals.hasKey(symName(c)):        # &global → adr
      g.emAdr(dest, symName(c))
    else:
      g.ab.tree LeaA64:                     # &stackvar
        g.emReg dest
        g.ab.sym symName(c)
    inc c
  elif c.kind == TagLit:
    case c.exprKind
    of DotC:                                # &obj.field
      var base, field: string
      c.into:
        base = symName(c); inc c
        field = symName(c); inc c
        while c.hasMore: skip c
      g.ab.tree LeaA64:
        g.emReg dest
        g.emAggrDot(base, field)
    of DerefC:                              # &(deref p) == p
      c.into:
        g.genInto(c, dest)
        while c.hasMore: skip c
    of AtC:                                 # &arr[idx]
      g.ab.tree LeaA64:
        g.emReg dest
        g.emAt(c)
    else: raiseAssert "arkham v1: addr of unsupported lvalue: " & $c.exprKind
  else: raiseAssert "arkham v1: addr of non-lvalue"

# ── calls ────────────────────────────────────────────────────────────────────

proc genCall(g: var CodeGen; c: var Cursor) =
  ## `(call f arg…)` — internal `(call)` or external `(extcall)`. Integer/
  ## pointer args go straight into x0,x1,… left-to-right (no nested calls — the
  ## optimizer flattens them), each committed arg register sealed so scratch use
  ## during marshalling can't clobber it. The result lands in x0.
  c.into:
    let fsym = symName(c); inc c
    assert g.callTarget.hasKey(fsym), "arkham v1: unknown call target: " & fsym
    let tgt = g.callTarget[fsym]
    var idx = 0
    var sealedHere: set[Reg] = {}
    while c.hasMore:
      if c.kind == Symbol and g.varType.hasKey(symName(c)):
        let vn = symName(c)
        let tn = g.varType[vn]
        if aggrByteSize(g.typeDecls, tn) > 16:
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
          let nw = aggrWordCount(g.typeDecls, tn)
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
    let nm = symName(c)
    if not g.typeDecls.hasKey(nm):
      raiseAssert "arkham v1: unknown type: " & nm
    var d = g.typeDecls[nm]
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
  let lay = aggrLayout(g.typeDecls, typeName)
  for i in 0 ..< aggrWordCount(g.typeDecls, typeName):
    let fn = fieldAtOffset(lay, i * 8)
    if fn.len == 0: raiseAssert "arkham v1: sub-word-packed aggregate ABI unsupported"
    g.ab.tree MovA64:
      g.emReg IntArgRegs[firstArg + i]
      g.emAggrFieldMem(varName, fn)

proc regsToStruct(g: var CodeGen; varName, typeName: string; firstArg: int) =
  ## x{firstArg+i} → aggregate (one GPR per 8-byte word).
  let lay = aggrLayout(g.typeDecls, typeName)
  for i in 0 ..< aggrWordCount(g.typeDecls, typeName):
    let fn = fieldAtOffset(lay, i * 8)
    if fn.len == 0: raiseAssert "arkham v1: sub-word-packed aggregate ABI unsupported"
    g.ab.tree MovA64:
      g.emAggrFieldMem(varName, fn)
      g.emReg IntArgRegs[firstArg + i]

proc copyStructThroughPtr(g: var CodeGen; srcVar, typeName: string; ptrReg: Reg) =
  ## Field-wise copy of the aggregate `srcVar` to the memory `ptrReg` points at
  ## (any layout — sub-word fields are fine, it copies per field).
  for f in aggrLayout(g.typeDecls, typeName):
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
    skip c                                  # type
    let loc = g.ra.locationOfSym(name)
    case loc.kind
    of NamedStack:
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
        if aggrByteSize(g.typeDecls, typeName) > 16:
          # >16B: hand the callee a pointer to this var via x8; it writes there.
          g.ab.tree LeaA64: g.emReg IndirectResultReg; g.ab.sym name
          g.genCall(c)
        else:
          g.genCall(c)
          g.regsToStruct(name, typeName, 0)
      else: raiseAssert "arkham v1: aggregate initializer must be oconstr/call: " & name
    of InReg:
      if c.kind == DotToken: inc c          # no initializer
      else: g.genInto(c, loc.r)
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

proc emitCmpBranch(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool) =
  ## `c` is a comparison `(op a b)` (NO type child). Emit `cmp a, b` and branch
  ## to `toLabel` when the condition is true/false. Ordering signedness comes
  ## from the first operand's slot (unsigned var → unsigned branch).
  let ek = c.exprKind
  var tag: A64Inst
  c.into:
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
  c.into:
    let condStart = c
    skip c                                  # `c` → first body statement
    g.emLab(lStart)
    var cond = condStart
    g.emitCondJump(cond, lEnd, whenTrue = false)
    while c.hasMore: genStmt(g, c)
    g.emBr(BA64, lStart)
  g.emLab(lEnd)

proc genAsgn(g: var CodeGen; c: var Cursor) =
  ## `(asgn lvalue rvalue)`. The lvalue is a register-resident local or a
  ## `(dot obj field)` field store into a nifasm-managed stack aggregate.
  c.into:
    if c.kind == Symbol:
      let name = symName(c); inc c
      if g.globals.hasKey(name):              # global store: [&g] ← rhs
        let (rr, rt) = g.genReg(c)
        let tmp = g.borrowTmp()
        g.emAdr(tmp, name)
        g.ab.tree MovA64:
          g.ab.tree MemX: g.emReg tmp
          g.emReg rr
        g.giveBack tmp
        if rt: g.giveBack rr
      else:
        let loc = g.ra.locationOfSym(name)
        case loc.kind
        of InReg: g.genInto(c, loc.r)
        else: raiseAssert "arkham v1: asgn to non-register local: " & name
    elif c.kind == TagLit and c.exprKind == DotC:
      let lval = c                          # the `(dot …)` lvalue
      skip c                                # advance to the rvalue
      let (rr, rt) = g.genReg(c)            # evaluate rhs into a register
      var dst = lval
      g.ab.tree MovA64:                     # field store: [base+offset] ← rr
        g.emDotMem(dst)
        g.emReg rr
      if rt: g.giveBack rr
    elif c.kind == TagLit and c.exprKind == DerefC:
      # `(asgn (deref p) rhs)` → [p] ← rhs
      var lval = c
      skip c                                # advance to the rvalue
      let (rr, rt) = g.genReg(c)            # rhs → reg
      var pr = NoReg
      var pt = false
      lval.into:
        let (r, t) = g.genReg(lval); pr = r; pt = t
        while lval.hasMore: skip lval       # (cppref)?
      g.ab.tree MovA64:
        g.ab.tree MemX: g.emReg pr
        g.emReg rr
      if pt: g.giveBack pr
      if rt: g.giveBack rr
    elif c.kind == TagLit and c.exprKind == AtC:
      # `(asgn (at arr idx) rhs)` → arr[idx] ← rhs
      var lval = c
      skip c                                # advance to the rvalue
      let (rr, rt) = g.genReg(c)            # rhs → reg
      g.ab.tree MovA64:
        g.ab.tree MemX: g.emAt(lval)
        g.emReg rr
      if rt: g.giveBack rr
    else:
      raiseAssert "arkham v1: unsupported asgn target"

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
  for name, pos in g.ra.symPos:               # locals occupying a volatile reg
    let loc = g.ra.locs[pos]
    if loc.kind == InReg: g.freeTmp.excl loc.r

proc computeFrame(g: var CodeGen; hasCall: bool) =
  g.frameRegs = @[]
  for r in IntCalleeSaved:
    if r in g.ra.usedCallee: g.frameRegs.add r
  if g.frameRegs.len mod 2 == 1:              # save in pairs → pad to even
    for r in IntCalleeSaved:
      if r notin g.ra.usedCallee: (g.frameRegs.add r; break)
  g.hasFrame = hasCall or g.frameRegs.len > 0

proc emitParamMoves(g: var CodeGen; decl: Cursor) =
  ## Move each parameter from its incoming ABI register to the home the
  ## allocator chose (a callee-saved register for cross-call params; arg regs
  ## stay put for leaf procs). Emitted after the prologue saved the homes.
  var c = decl
  inc c                                       # proc head → name
  inc c                                       # name → params slot
  if c.kind != TagLit: return                 # (params) is `.` → no parameters
  var idx = 0
  c.into:                                     # into (params …)
    while c.hasMore:
      var nm = ""
      var tn = ""
      c.into:                                 # (param :name pragmas type)
        nm = symName(c); inc c
        skip c                                # pragmas
        if c.kind == Symbol and g.typeDecls.hasKey(symName(c)): tn = symName(c)
        while c.hasMore: skip c               # type (+ anything else)
      let loc = g.ra.locationOfSym(nm)
      if tn.len > 0 and loc.kind == NamedStack:
        # ≤16B by-value aggregate: declare its stack home, fill from its GPR(s)
        g.varType[nm] = tn
        g.emStackVar(nm, tn)
        g.regsToStruct(nm, tn, idx)
        idx += aggrWordCount(g.typeDecls, tn)
      elif tn.len > 0 and loc.kind == InReg:
        # >16B by-reference aggregate: a pointer, homed like a scalar; field
        # accesses route through it (recorded in varType).
        g.varType[nm] = tn
        g.movReg(loc.r, IntArgRegs[idx])
        inc idx
      else:
        assert idx < IntArgRegs.len, "arkham v1: >8 integer params (stack TODO)"
        case loc.kind
        of InReg: g.movReg(loc.r, IntArgRegs[idx])
        else: raiseAssert "arkham v1: stack-resident parameter: " & nm
        inc idx

proc emitGlobalInits(g: var CodeGen)

proc genProc(g: var CodeGen; info: ProcInfo) =
  let an = analyseProc(info.decl)
  g.varType = initTable[string, string]()     # per-proc (symbol names recycle)
  # Determine the aggregate return convention BEFORE allocation: a named object
  # ≤16B → x0[:x1]; >16B → x8 indirect result (callee writes through the caller-
  # supplied pointer, which we park in x19 for the proc's lifetime).
  g.retAggrName = ""
  g.retIndirect = false
  g.indirectReg = NoReg
  block:
    var rc = info.decl
    inc rc; inc rc; skip rc                   # head → name → params, skip → return type
    if rc.kind == Symbol and g.typeDecls.hasKey(symName(rc)):
      g.retAggrName = symName(rc)
      g.retIndirect = aggrByteSize(g.typeDecls, g.retAggrName) > 16
  let preseal = if g.retIndirect: {X19} else: {}
  g.ra = allocateProc(g.buf[], info.decl, an, g.typeDecls, preseal)
  if g.retIndirect:
    g.indirectReg = X19
    g.ra.usedCallee.incl X19                  # saved/restored like any callee reg
  g.initFreeTmp()
  g.computeFrame(an.hasCall)
  g.ab.tree ProcD:
    g.ab.symDef info.asmName                  # "main.0" for the entry
    g.ab.keyword ParamsD                      # v1: arkham emits raw registers
    g.ab.keyword ResultD
    g.ab.keyword ClobberD
    g.ab.tree StmtsA64:
      if g.hasFrame: framePush(g)
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
  ## for `tvar`/`const`; any initializer is run at entry by `emitGlobalInits`).
  var c = decl
  c.into:                                     # (gvar SymbolDef VarPragmas Type Value?)
    inc c                                     # name
    skip c                                    # pragmas
    g.ab.open NifasmDecl.GvarD
    g.ab.symDef name
    g.genTypeBody(c)                          # type
    g.ab.close()
    while c.hasMore: skip c                   # value (initialized at entry)

proc emitGlobalInits(g: var CodeGen) =
  ## At program entry, store each global's initializer (if any) into its slot.
  for name, decl in g.globals:
    var c = decl
    c.into:
      inc c; skip c; skip c                   # name, pragmas, type
      if c.hasMore and c.kind != DotToken:
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

proc generate*(buf: var TokenBuf): string =
  ## Compile a parsed NIFC module to AArch64 / Darwin asm-NIF text.
  var prog = collect(buf)
  var g = CodeGen(ab: initAsmBuf(), buf: addr buf, callTarget: prog.callTarget,
                  typeDecls: prog.typeDecls, globals: prog.globals)
  g.ab.tree StmtsA64:
    g.ab.tree ArchD: g.ab.ident "arm64"
    if prog.needsLibSystem:
      g.ab.tree ImpD: g.ab.str DarwinLibSystem
    for ex in prog.externOrder:
      g.ab.tree ExtprocD:
        g.ab.symDef ex.asmName
        g.ab.str ex.extName
    for name, decl in prog.typeDecls:
      g.genType(name, decl)
    for name, decl in prog.globals:
      g.genGlobal(name, decl)
    for info in prog.procs:
      genProc(g, info)
    for (nm, bytes) in g.rodata:
      g.ab.tree RodataD:
        g.ab.symDef nm
        g.ab.str bytes
  result = g.ab.render()

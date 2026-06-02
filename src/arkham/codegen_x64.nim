#
#           Arkham — x86-64 / System V (Linux) code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Pass 3 (x86-64 backend). A first, scalar-integer code generator: it shares the
## front-end (`codegen_common`: the `CodeGen` state, the NIFC type/lvalue
## analysis) and the (arch-neutral) register allocator with the AArch64 backend,
## and emits System V / Linux asm-NIF that `nifasm` assembles+links to an ELF
## executable. Process exit is lowered to the Linux `exit` syscall (rax=60), so
## the produced binaries run without libc.
##
## v0 scope (mirrors arkham's AArch64 v1): integer/pointer scalars held in
## registers, arithmetic / bitops, `while`, comparisons, and a `main` that
## `exit`s. Floats, aggregates, memory lvalues, parameters, `if`/`case`, div/mod
## and shifts `raiseAssert` for now.

import std / [assertions, tables, sets]
import nifcore, nifcdecl
import slots, machinedesc, analyser, register_allocator, programs
import asmbuf, codegen_common, machine_x64

# ── scratch register pool ────────────────────────────────────────────────────

proc emReg(g: var CodeGen; r: Reg) {.inline.} =
  ## A value register operand. If `r` currently hosts a named local, emit the
  ## local's *name* (a typed symbol nifasm type-checks); otherwise the raw `(reg)`
  ## tag (a transient scratch register).
  if g.regLocal.hasKey(r): g.ab.sym g.regLocal[r]
  else: g.ab.reg r

proc initFreeTmp(g: var CodeGen) =
  g.freeTmp = {}
  for r in g.md.intTempRegs: g.freeTmp.incl r
  g.freeFTmp = {}
  for f in g.md.floatTempRegs: g.freeFTmp.incl f
  for name, pos in g.ra.symPos:               # registers held by a local/param
    let loc = g.ra.locs[pos]
    if loc.kind == InReg: g.freeTmp.excl loc.r
    elif loc.kind == InFReg: g.freeFTmp.excl loc.f

proc borrowTmp(g: var CodeGen): Reg =
  for r in g.md.intTempRegs:
    if r in g.freeTmp and not g.ra.isSealed(r):
      excl g.freeTmp, r
      return r
  raiseAssert "arkham x64 v0: out of scratch registers"

proc giveBack(g: var CodeGen; r: Reg) {.inline.} =
  if r != NoReg: g.freeTmp.incl r

# ── SSE / floating-point scratch pool + emit helpers ─────────────────────────
# x86-64 floats live in xmm0..xmm15 (the FReg slots F0..F15). The register operand
# is always `(xmmN)`; the precision is carried by the instruction tag (movss vs
# movsd, addss vs addsd, …), unlike AArch64 where `(sN)`/`(dN)` encode it.

const FloatRet = F0    # xmm0: SysV scalar-float return + first float argument

proc emFReg(g: var CodeGen; f: FReg) {.inline.} = g.ab.xmmReg f

proc borrowFTmp(g: var CodeGen): FReg =
  for f in g.md.floatTempRegs:
    if f in g.freeFTmp:
      excl g.freeFTmp, f
      return f
  raiseAssert "arkham x64 v0: out of SIMD scratch registers"

proc giveBackF(g: var CodeGen; f: FReg) {.inline.} =
  if f in g.md.floatTempRegs: g.freeFTmp.incl f

proc fmovF(g: var CodeGen; d, s: FReg; bits: int) =                # movss/movsd d, s
  if d == s: return
  let op = if bits == 32: MovssX64 else: MovsdX64
  g.ab.tree op: g.emFReg d; g.emFReg s

proc fmovFromGpr(g: var CodeGen; d: FReg; s: Reg; bits: int) =     # movfd/movfq xmm ← gpr
  let op = if bits == 32: MovfdX64 else: MovfqX64
  g.ab.tree op: g.emFReg d; g.emReg s

proc fmovToGpr(g: var CodeGen; d: Reg; s: FReg; bits: int) =       # movfd/movfq gpr ← xmm
  let op = if bits == 32: MovfdX64 else: MovfqX64
  g.ab.tree op: g.emReg d; g.emFReg s

proc fbin(g: var CodeGen; op32, op64: X64Inst; d, s: FReg; bits: int) =  # d = d op s
  let op = if bits == 32: op32 else: op64
  g.ab.tree op: g.emFReg d; g.emFReg s

proc fcvtI2F(g: var CodeGen; d: FReg; s: Reg; bits: int) =         # cvtsi2ss/sd xmm ← gpr
  let op = if bits == 32: Cvtsi2ssX64 else: Cvtsi2sdX64
  g.ab.tree op: g.emFReg d; g.emReg s

proc fcvtF2I(g: var CodeGen; d: Reg; s: FReg; bits: int) =         # cvttss2si/sd2si gpr ← xmm
  let op = if bits == 32: Cvttss2siX64 else: Cvttsd2siX64
  g.ab.tree op: g.emReg d; g.emFReg s

proc emFcvt(g: var CodeGen; d, s: FReg; dstBits, srcBits: int) =   # precision convert
  if dstBits == srcBits: (g.fmovF(d, s, dstBits); return)
  let op = if dstBits == 32: Cvtsd2ssX64 else: Cvtss2sdX64
  g.ab.tree op: g.emFReg d; g.emFReg s

# A spilled float scalar lives in an `(s) (f N)` stack slot (x64 has no callee-
# saved xmm registers, so a float that must survive a call has nowhere else to
# go). It is loaded/stored with movss/movsd against `(mem (rsp) name)`.
proc emFloatStackVar(g: var CodeGen; name: string; bits: int) =
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.floatType(bits)
  g.ab.close()

proc emFloatScalarLoad(g: var CodeGen; dest: FReg; name: string; bits: int) =
  let op = if bits == 32: MovssX64 else: MovsdX64
  g.ab.tree op:
    g.emFReg dest
    g.ab.tree MemX: (g.ab.reg RSP; g.ab.sym name)

proc emFloatScalarStore(g: var CodeGen; name: string; src: FReg; bits: int) =
  let op = if bits == 32: MovssX64 else: MovsdX64
  g.ab.tree op:
    g.ab.tree MemX: (g.ab.reg RSP; g.ab.sym name)
    g.emFReg src

# ── low-level emit helpers ───────────────────────────────────────────────────

proc movImm(g: var CodeGen; d: Reg; v: int64) =
  g.ab.tree MovX64: g.emReg d; g.ab.intLit v

proc movReg(g: var CodeGen; d, s: Reg) =
  if d == s: return
  g.ab.tree MovX64: g.emReg d; g.emReg s

proc binReg(g: var CodeGen; op: X64Inst; d, s: Reg) =      # d op= s
  g.ab.tree op: g.emReg d; g.emReg s

proc binImm(g: var CodeGen; op: X64Inst; d: Reg; v: int64) =  # d op= imm
  g.ab.tree op: g.emReg d; g.ab.intLit v

proc extendTo(g: var CodeGen; dest: Reg; width: int; signed: bool) =
  ## Normalize the low `width` bits of `dest` to its full 64-bit register form
  ## (sign- or zero-extended). No-op for 64-bit. Done with the `shl #(64-w);
  ## sar|shr #(64-w)` shift pair (immediate shifts), matching the A64 backend —
  ## arkham keeps every scalar 64-bit-wide in a register, so widths are normalized
  ## explicitly rather than relying on sized loads.
  if width <= 0 or width >= 64: return
  let sh = int64(64 - width)
  g.binImm(ShlX64, dest, sh)
  g.binImm(if signed: SarX64 else: ShrX64, dest, sh)

proc emLab(g: var CodeGen; name: string) =
  g.ab.tree LabX64: g.ab.symDef name

proc emJmp(g: var CodeGen; name: string) =
  g.ab.tree JmpX64: g.ab.sym name

proc emJcc(g: var CodeGen; tag: X64Inst; name: string) =
  g.ab.tree tag: g.ab.sym name

proc emSyscall(g: var CodeGen) = g.ab.keyword SyscallX64

proc freshLabel(g: var CodeGen): string =
  result = "L" & $g.labelCount & ".0"
  inc g.labelCount

# ── expressions ──────────────────────────────────────────────────────────────

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg)
proc genCall(g: var CodeGen; c: var Cursor)
proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg)
proc emitCondJump(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool)
proc genVal(g: var CodeGen; c: var Cursor): Val
proc forceReg(g: var CodeGen; v: Val): tuple[r: Reg, owns: bool]
proc emitLoad(g: var CodeGen; l: Lvalue; dest: Reg)
proc emitStore(g: var CodeGen; l: Lvalue; src: Reg)
proc genTypeBody(g: var CodeGen; c: var Cursor)
proc emitGlobalInits(g: var CodeGen)
proc framePop(g: var CodeGen)
proc genIntoF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int)
proc emitLoadF(g: var CodeGen; l: Lvalue; dest: FReg; bits: int)

# ── named local variables (nifasm type-checks them; raw scratch stays `(reg)`) ─

proc emRegLocalVar(g: var CodeGen; name: string; r: Reg; typeCur: Cursor) =
  ## Declare `(var :name (reg) type)` and bind `r` to `name` for the rest of its
  ## scope, so subsequent uses emit the typed name instead of `(reg)`.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.reg r                                   # the concrete register (the binding)
  # arkham keeps scalars 64-bit in registers and handles width/signedness via
  # explicit extends, so an int/uint/bool/char local is declared as plain
  # `(i 64)` (a logical `i8`/`u8` would mismatch a 64-bit `mov`, and nifasm also
  # rejects an `i`↔`u` move); a pointer keeps its `(ptr T)` so deref/field typing
  # works. Signed-vs-unsigned comparisons still pick `jb`/`jl` from the slot.
  let rt = resolveType(g.prog, typeCur)
  if isPtrType(rt):
    var tc = typeCur
    g.genTypeBody(tc)
  else: g.ab.intType(64)
  g.ab.close()
  g.regLocal[r] = name
  g.scopeLocals[^1].add (name: name, reg: r)

proc enterScope(g: var CodeGen) = g.scopeLocals.add @[]

proc exitScope(g: var CodeGen) =
  ## `kill` each register local declared in the closing scope so the allocator's
  ## register reuse in a sibling scope rebinds cleanly (nifasm forbids binding a
  ## still-live register).
  for it in g.scopeLocals.pop():
    g.ab.tree KillX64: g.ab.sym it.name
    g.regLocal.del it.reg

# ── stack-slot declarations + memory operands (x86 addressing) ───────────────
# nifasm keeps field names / element types, so a memory operand stays symbolic:
#  * a spilled/address-taken scalar or aggregate is a `(var :name (s) T)` slot,
#    addressed `(mem (rsp) name)` / `(mem (dot (rsp) name field))`;
#  * a pointer in a register is dereferenced `(mem reg)`.
# Storing an immediate to memory is unimplemented in nifasm, so callers
# materialize the value into a register first.

proc emStackVar(g: var CodeGen; name, typeName: string) =
  ## `(var :name (s) typeName)` — a nifasm-managed aggregate stack slot.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.sym typeName
  g.ab.close()

proc emScalarStackVar(g: var CodeGen; name: string) =
  ## `(var :name (s) (i 64))` — a spilled/address-taken scalar's 8-byte slot.
  g.ab.open NifasmDecl.VarD
  g.ab.symDef name
  g.ab.keyword SO
  g.ab.intType(64)
  g.ab.close()

proc emStackMem(g: var CodeGen; name: string) =       # (mem (rsp) name)
  g.ab.tree MemX:
    g.ab.reg RSP
    g.ab.sym name

proc emFieldMem(g: var CodeGen; base, field: string) =   # (mem (dot (rsp) base field))
  for fi in aggrLayout(g.prog, g.varType[base]):  # v0: only full 8-byte fields
    if fi.name == field:
      if fi.size != 8:
        raiseAssert "arkham x64 v0: sub-word field not supported: " & base & "." & field
      break
  g.ab.tree MemX:
    g.ab.tree DotX:
      g.ab.reg RSP
      g.ab.sym base
      g.ab.sym field

proc fieldOffset(g: var CodeGen; base, field: string): int =
  for fi in aggrLayout(g.prog, g.varType[base]):
    if fi.name == field: return fi.off
  raiseAssert "arkham x64: field not found: " & base & "." & field

proc emAccessAddr(g: var CodeGen; n: var Cursor; tmps: var seq[Reg]) =
  ## Recursively re-emit the NIFC lvalue subtree `n` as a nifasm address
  ## expression, letting nifasm collapse the whole chain to `base+offset` from the
  ## declared types. Borrowed temps (deref pointers, computed array indices) are
  ## pushed onto `tmps` for the caller to free after the instruction. A stack var
  ## contributes `(rsp) name`; a pointer in a register contributes the register.
  case n.kind
  of Symbol:
    let nm = symName(n); inc n
    let loc = g.ra.locationOfSym(nm)
    case loc.kind
    of NamedStack: (g.ab.reg RSP; g.ab.sym nm)   # a stack var: rsp + slot offset
    of InReg: g.emReg loc.r                        # a pointer in a register
    else: raiseAssert "arkham x64 v0: unsupported lvalue base: " & nm
  of TagLit:
    case n.exprKind
    of DotC:
      g.ab.tree DotX:
        n.into:
          g.emAccessAddr(n, tmps)                # base (recursive)
          let field = symName(n); inc n          # field name (offset is nifasm's job)
          g.ab.sym field                         # nifasm sizes the access by field type
          while n.hasMore: skip n                # depth selector
    of AtC:
      g.ab.tree AtX:
        n.into:
          g.emAccessAddr(n, tmps)                # array base (recursive)
          if n.kind == IntLit: (g.ab.intLit intVal(n); inc n)
          else:
            let (r, owns) = g.forceReg(g.genVal(n))
            g.emReg r
            if owns: tmps.add r
          while n.hasMore: skip n
    of DerefC:
      n.into:
        let (p, owns) = g.forceReg(g.genVal(n))  # the pointer → a register
        g.emReg p                                # named (bound regs must use the name in mem)
        if owns: tmps.add p
        while n.hasMore: skip n                  # (cppref)?
    else: raiseAssert "arkham x64 v0: not an lvalue: " & $n.exprKind
  else: raiseAssert "arkham x64 v0: not an lvalue: " & $n.kind

proc emMemOperand(g: var CodeGen; l: Lvalue): seq[Reg] =
  ## Emit `(mem <addr>)` for `l` by re-emitting `l.n`; returns temps to free.
  result = @[]
  var nn = l.n
  g.ab.tree MemX:
    g.emAccessAddr(nn, result)

proc emGlobalAddr(g: var CodeGen; dest: Reg; name: string) =
  ## `dest ← &global` — RIP-relative `lea` (nifasm resolves the gvar to a
  ## `.bss`/`.data` address). x86-64 has no typed RIP-relative memory operand, so
  ## a global is always accessed by first materializing its address.
  g.ab.tree LeaX64: (g.emReg dest; g.ab.sym name)

proc emitLoad(g: var CodeGen; l: Lvalue; dest: Reg) =
  ## `dest ← <lvalue>`.
  if l.kind == lvReg:
    g.movReg(dest, l.r)
  elif l.kind == lvTvar:                          # thread-local → FS:[off] mem operand
    g.ab.tree MovX64:                             # nifasm resolves a tvar symbol to FS:[off]
      g.emReg dest
      g.ab.sym l.name
  elif l.kind == lvGlobal:                       # &g into dest, then deref dest
    g.emGlobalAddr(dest, l.name)                 # dest ← &g (no extra scratch needed)
    g.ab.tree MovX64:
      g.emReg dest
      g.ab.tree MemX: g.emReg dest               # dest ← [dest]
  else:                                         # any memory lvalue → recursive operand
    var tmps: seq[Reg]
    g.ab.tree MovX64:
      g.emReg dest
      tmps = g.emMemOperand(l)
    for t in tmps: g.giveBack t

proc emitStore(g: var CodeGen; l: Lvalue; src: Reg) =
  ## `<lvalue> ← src` (src already in a register — imm→mem is unsupported).
  if l.kind == lvReg:
    g.movReg(l.r, src)
  elif l.kind == lvTvar:                          # thread-local → FS:[off] store
    g.ab.tree MovX64:
      g.ab.sym l.name
      g.emReg src
  elif l.kind == lvGlobal:                       # &g into a temp, then store
    let p = g.borrowTmp()
    g.emGlobalAddr(p, l.name)
    g.ab.tree MovX64:
      g.ab.tree MemX: g.emReg p
      g.emReg src
    g.giveBack p
  else:
    var tmps: seq[Reg]
    g.ab.tree MovX64:
      tmps = g.emMemOperand(l)
      g.emReg src
    for t in tmps: g.giveBack t

proc emitAddr(g: var CodeGen; l: Lvalue; dest: Reg) =
  ## `dest ← &<lvalue>`.
  case l.kind
  of lvStackScalar, lvAggrVar:                # (lea dest (rsp) name)
    g.ab.tree LeaX64: (g.emReg dest; g.ab.reg RSP; g.ab.sym l.name)
  of lvField:                                 # &base + field offset
    g.ab.tree LeaX64: (g.emReg dest; g.ab.reg RSP; g.ab.sym l.base)
    let off = g.fieldOffset(l.base, l.field)
    if off != 0: g.binImm(AddX64, dest, off.int64)
  of lvDeref:                                 # &(deref p) == p
    var p = l.cur
    p.into:
      g.genInto(p, dest)
      while p.hasMore: skip p
  of lvGlobal: g.emGlobalAddr(dest, l.name)   # &global → RIP-relative lea
  else: raiseAssert "arkham x64 v0: address-of lvalue kind " & $l.kind

proc binMem(g: var CodeGen; op: X64Inst; dest: Reg; l: Lvalue) =
  ## `dest op= <memory operand>` — x86 folds a memory source into the ALU op.
  var tmps: seq[Reg]
  g.ab.tree op:
    g.emReg dest
    tmps = g.emMemOperand(l)
  for t in tmps: g.giveBack t

proc place(g: var CodeGen; v: Val; dest: Reg) =
  ## Materialize `v` into `dest`, releasing any owned scratch it occupied.
  case v.kind
  of vkImm: g.movImm(dest, v.imm)
  of vkReg:
    g.movReg(dest, v.r)
    if v.ownsR and v.r != dest: g.giveBack v.r
  of vkMem: g.emitLoad(v.mem, dest)
  else: raiseAssert "arkham x64 v0: cannot place a value of kind " & $v.kind

proc forceReg(g: var CodeGen; v: Val): tuple[r: Reg, owns: bool] =
  ## Ensure `v` is in a register (an immediate / memory operand is loaded into a
  ## fresh temp).
  case v.kind
  of vkReg: (v.r, v.ownsR)
  of vkImm:
    let t = g.borrowTmp(); g.movImm(t, v.imm); (t, true)
  of vkMem:
    let t = g.borrowTmp(); g.emitLoad(v.mem, t); (t, true)
  else: raiseAssert "arkham x64 v0: cannot force a value of kind " & $v.kind & " into a register"

proc genVal(g: var CodeGen; c: var Cursor): Val =
  ## The dont-care evaluator: produce `c`'s value wherever it naturally lives — a
  ## literal as an immediate, a register-resident local in place, a memory operand
  ## as a foldable `vkMem` — materializing any *computed* value into a freshly
  ## borrowed scratch register. The counterpart of `genInto(…, dest)`.
  case c.kind
  of IntLit:
    result = Val(kind: vkImm, imm: intVal(c)); inc c
  of Symbol:
    let l = g.asLvalue(c)
    case l.kind
    of lvReg: result = Val(kind: vkReg, r: l.r, ownsR: false)
    of lvStackScalar: result = Val(kind: vkMem, mem: l)
    of lvGlobal, lvTvar:                        # load through its address into a scratch
      let t = g.borrowTmp(); g.emitLoad(l, t)
      result = Val(kind: vkReg, r: t, ownsR: true)
    else: raiseAssert "arkham x64 v0: operand of kind " & $l.kind
  of TagLit:
    case c.exprKind
    of DotC, AtC, DerefC:                       # a memory lvalue used as a value
      result = Val(kind: vkMem, mem: g.asLvalue(c))
    else:
      let t = g.borrowTmp()                    # a computed value → a scratch reg
      g.genInto(c, t)
      result = Val(kind: vkReg, r: t, ownsR: true)
  else:
    let t = g.borrowTmp()
    g.genInto(c, t)
    result = Val(kind: vkReg, r: t, ownsR: true)

proc genBin(g: var CodeGen; c: var Cursor; dest: Reg; op: X64Inst; immOk: bool) =
  ## `dest = a op b` in x86's destructive form: `a` into `dest`, then `dest op= b`.
  ## The right operand is a descriptor, so an immediate folds into `op dest, imm`
  ## and a register is used in place. If `b` lives in `dest`, it is saved before
  ## `a` overwrites it (correct for commutative and non-commutative ops alike).
  c.into:
    skip c                                    # result type
    var bPeek = c                             # peek b before `a` overwrites dest
    skip bPeek
    var b: Val
    if g.operandInReg(bPeek, dest):
      let saved = g.borrowTmp()
      g.movReg(saved, dest)                   # preserve b before `a` clobbers dest
      g.genInto(c, dest)                      # a → dest
      skip c                                  # consume b
      b = Val(kind: vkReg, r: saved, ownsR: true)
    else:
      g.genInto(c, dest)                      # a → dest
      b = g.genVal(c)                         # b stays where it naturally lives
    if immOk and b.kind == vkImm and b.imm >= 0 and b.imm <= 0xFFFF:
      g.binImm(op, dest, b.imm)
    elif b.kind == vkMem:
      g.binMem(op, dest, b.mem)               # fold the memory operand: op dest, [mem]
    else:
      let (br, owns) = g.forceReg(b)
      g.binReg(op, dest, br)
      if owns: g.giveBack br

proc materializeCond(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## A comparison/logic used as a 0/1 value: assume true, jump over the reset.
  let lEnd = g.freshLabel()
  g.movImm(dest, 1)
  g.emitCondJump(c, lEnd, whenTrue = true)
  g.movImm(dest, 0)
  g.emLab(lEnd)

proc genDivMod(g: var CodeGen; c: var Cursor; dest: Reg; signed, wantRemainder: bool) =
  ## x86 division: dividend in RAX, divisor in a register; quotient → RAX,
  ## remainder → RDX. nifasm's `(idiv|div (rdx)(rax) src)` emits the cqo / xor-rdx
  ## itself. RAX/RDX are never live locals (not in the allocator's pool), so
  ## clobbering them is safe; the divisor lives in a borrowed temp (idiv has no
  ## immediate form).
  c.into:
    skip c                                    # result type
    g.genInto(c, RAX)                          # dividend → rax
    g.ra.seal RAX                              # protect it while materializing the divisor
    let (br, owns) = g.forceReg(g.genVal(c))   # divisor → a register
    g.ra.unseal {RAX}
    g.ab.tree (if signed: IdivX64 else: DivX64):
      g.ab.reg RDX                             # (rdx): high half of the dividend
      g.ab.reg RAX                             # (rax): low half
      g.emReg br
    if owns: g.giveBack br
  g.movReg(dest, if wantRemainder: RDX else: RAX)

# ── floating-point expressions (single + double precision) ──────────────────
# `bits` (32/64) is the value's precision, threaded top-down: it selects movss vs
# movsd / addss vs addsd, etc. A bare literal has no inherent width, so it adopts
# the contextual `bits`.

proc genFReg(g: var CodeGen; c: var Cursor; bits: int): tuple[f: FReg, owns: bool] =
  ## A float operand in an xmm register: a float local stays in place; anything
  ## else is materialized into a borrowed SIMD temp.
  if c.kind == Symbol:
    let loc = g.ra.locationOfSym(symName(c))
    if loc.kind == InFReg:
      inc c
      return (loc.f, false)
  let f = g.borrowFTmp()
  g.genIntoF(c, f, bits)
  (f, true)

proc genFBin(g: var CodeGen; c: var Cursor; dest: FReg; op32, op64: X64Inst; bits: int) =
  ## `(op (f N) a b)` → `dest = a op b` (addss/sd, subss/sd, mulss/sd, divss/sd).
  c.into:
    skip c                                    # result float type
    g.genIntoF(c, dest, bits)                  # a → dest
    let (fr, ft) = g.genFReg(c, bits)          # b → fp temp (or in place)
    g.fbin(op32, op64, dest, fr, bits)
    if ft: g.giveBackF fr

proc genConvToF(g: var CodeGen; c: var Cursor; dest: FReg; bits: int) =
  ## `(conv (f N) Expr)` — int→float (cvtsi2ss/sd) or float→float (cvt precision).
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
      g.fcvtI2F(dest, tmp, bits)                 # cvtsi2ss/sd (signed; u64 edge: TODO)
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
    of AddC: g.genFBin(c, dest, AddssX64, AddsdX64, bits)
    of SubC: g.genFBin(c, dest, SubssX64, SubsdX64, bits)
    of MulC: g.genFBin(c, dest, MulssX64, MulsdX64, bits)
    of DivC: g.genFBin(c, dest, DivssX64, DivsdX64, bits)
    of NegC:
      # No scalar SSE negate; flip the sign bit by subtracting from +0.0.
      c.into:
        skip c                                # result type
        let (sf, st) = g.genFReg(c, bits)
        let zero = g.borrowFTmp()
        let z = g.borrowTmp(); g.movImm(z, 0)
        g.fmovFromGpr(zero, z, bits); g.giveBack z   # zero ← +0.0
        g.fbin(SubssX64, SubsdX64, zero, sf, bits)   # 0 - x
        g.fmovF(dest, zero, bits)
        g.giveBackF zero
        if st: g.giveBackF sf
        while c.hasMore: skip c
    of ConvC: g.genConvToF(c, dest, bits)
    of CastC: g.genCastToF(c, dest, bits)
    of CallC:
      g.genCall(c)                            # float result lands in xmm0 …
      g.fmovF(dest, FloatRet, bits)           # … move it to the destination
    of DotC, AtC, DerefC:                      # float field / element / deref: dest ← [mem]
      let l = g.asLvalue(c); g.emitLoadF(l, dest, bits)
    else: raiseAssert "arkham x64 v0: float expression not supported: " & $c.exprKind
  else:
    raiseAssert "arkham x64 v0: float operand not supported: " & $c.kind

proc emitLoadF(g: var CodeGen; l: Lvalue; dest: FReg; bits: int) =
  ## `dest ← <lvalue>` (float). v0: register-resident float locals only; memory
  ## float lvalues (stack spill / global / field / element / deref) are TODO.
  case l.kind
  of lvFReg: g.fmovF(dest, l.f, bits)
  of lvStackScalar: g.emFloatScalarLoad(dest, l.name, bits)
  of lvField, lvElem, lvDeref:                # movss/movsd xmm ← (mem <recursive addr>)
    let op = if bits == 32: MovssX64 else: MovsdX64
    var tmps: seq[Reg]
    g.ab.tree op:
      g.emFReg dest
      tmps = g.emMemOperand(l)
    for t in tmps: g.giveBack t
  else: raiseAssert "arkham x64 v0: float load from lvalue kind " & $l.kind

proc emitStoreF(g: var CodeGen; l: Lvalue; src: FReg; bits: int) =
  ## `<lvalue> ← src` (float). v0: register / spilled locals + memory lvalues.
  case l.kind
  of lvFReg: g.fmovF(l.f, src, bits)
  of lvStackScalar: g.emFloatScalarStore(l.name, src, bits)
  of lvField, lvElem, lvDeref:                # movss/movsd (mem <recursive addr>) ← xmm
    let op = if bits == 32: MovssX64 else: MovsdX64
    var tmps: seq[Reg]
    g.ab.tree op:
      tmps = g.emMemOperand(l)
      g.emFReg src
    for t in tmps: g.giveBack t
  else: raiseAssert "arkham x64 v0: float store to lvalue kind " & $l.kind

proc genCoerce(g: var CodeGen; c: var Cursor; dest: Reg; isCast: bool) =
  ## `(conv Type Expr)` / `(cast Type Expr)`: evaluate `Expr` into `dest`, then
  ## re-represent it in `Type`'s 64-bit register form. Widening extends from the
  ## *source* width (conv follows the source signedness; cast zero-extends the
  ## bits); narrowing/equal truncates to the target width and extends per the
  ## *target*; an int→ptr target zero-extends a narrower source. Integer/char/
  ## bool/pointer only (no floats in x64 v0).
  c.into:
    let tc = resolveType(g.prog, c)           # resolve named types / enums
    let targetSigned = isSignedType(tc)
    let targetW = intTypeWidth(tc)
    let targetPtr = isPtrType(tc)
    skip c                                    # target type
    if g.isFloatExpr(c):
      # float source → integer/pointer target (`dest` is a GPR).
      let fbits = g.floatBits(c)
      let (sf, st) = g.genFReg(c, fbits)
      if isCast:
        g.fmovToGpr(dest, sf, fbits)          # reinterpret the float's bits
      else:
        g.fcvtF2I(dest, sf, fbits)            # cvtt* (truncate toward zero)
        if targetW < 64 and not targetPtr:
          g.extendTo(dest, targetW, signed = targetSigned)
      if st: g.giveBackF sf
      while c.hasMore: skip c
      return
    let (srcW, srcSigned) = g.srcWidthSigned(c)
    g.genInto(c, dest)                        # value → dest
    if targetPtr:
      if srcW < 64: g.extendTo(dest, srcW, signed = false)   # int→ptr: zero-extend
    elif srcW < targetW:                      # widening int→int
      g.extendTo(dest, srcW, signed = (not isCast) and srcSigned)
    else:                                     # narrowing or equal width
      g.extendTo(dest, targetW, signed = targetSigned)
    while c.hasMore: skip c

proc genInto(g: var CodeGen; c: var Cursor; dest: Reg) =
  case c.kind
  of IntLit, Symbol:
    g.place(g.genVal(c), dest)               # literal / register-resident local
  of StrLit:                                 # string literal → rodata + RIP-relative lea
    let nm = "msg." & $g.rodata.len
    g.rodata.add (nm, strVal(c)); inc c
    g.ab.tree LeaX64:
      g.emReg dest
      g.ab.sym nm                            # `(lea dest msg.N)` → nifasm RIP-relative
  of TagLit:
    case c.exprKind
    of AddC: g.genBin(c, dest, AddX64, immOk = true)
    of SubC: g.genBin(c, dest, SubX64, immOk = true)
    of MulC: g.genBin(c, dest, ImulX64, immOk = false)
    of BitandC: g.genBin(c, dest, AndX64, immOk = true)
    of BitorC: g.genBin(c, dest, OrX64, immOk = true)
    of BitxorC: g.genBin(c, dest, XorX64, immOk = true)
    of DivC, ModC:
      let wantRemainder = c.exprKind == ModC
      var tc = c; inc tc                      # the result-type child
      g.genDivMod(c, dest, signed = isSignedType(tc), wantRemainder = wantRemainder)
    of ShlC:
      g.genBin(c, dest, ShlX64, immOk = true)
    of ShrC:                                  # arithmetic for signed, logical for unsigned
      var tc = c; inc tc
      g.genBin(c, dest, (if isSignedType(tc): SarX64 else: ShrX64), immOk = true)
    of NegC:
      c.into:
        skip c                                # type
        g.genInto(c, dest)
        g.ab.tree NegX64: g.emReg dest
    of BitnotC:
      c.into:
        skip c                                # type
        g.genInto(c, dest)
        g.ab.tree NotX64: g.emReg dest        # x86 has a real `not`
    of NotC:                                  # boolean not: a ∈ {0,1} → a xor 1
      c.into:
        g.genInto(c, dest)
        g.binImm(XorX64, dest, 1)
    of EqC, NeqC, LtC, LeC, AndC, OrC:
      g.materializeCond(c, dest)
    of ConvC: g.genCoerce(c, dest, isCast = false)
    of CastC: g.genCoerce(c, dest, isCast = true)
    of DotC, AtC, DerefC:                     # field / element / pointer load: dest ← [mem]
      g.place(g.genVal(c), dest)
    of AddrC:                                 # (addr lvalue) → dest ← &lvalue
      c.into:
        g.genAddr(c, dest)
        while c.hasMore: skip c               # (cppref)?
    of CallC:
      g.genCall(c)
      g.movReg(dest, RAX)
    else: raiseAssert "arkham x64 v0: expression not supported: " & $c.exprKind
  else: raiseAssert "arkham x64 v0: operand not supported: " & $c.kind

proc genAddr(g: var CodeGen; c: var Cursor; dest: Reg) =
  ## `dest ← &lvalue`. Parse the addressing mode once, then form the address.
  let l = g.asLvalue(c)
  g.emitAddr(l, dest)

# ── conditions / branches ────────────────────────────────────────────────────

proc emitCmpBranch(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool) =
  ## `c` is a comparison `(op a b)` (NO type child): `cmp a, b` then a `jcc` to
  ## `toLabel` when the condition is true/false. Ordering signedness comes from
  ## the first operand's slot (an unsigned operand → an unsigned condition).
  let ek = c.exprKind
  var tag: X64Inst
  c.into:
    if g.isFloatExpr(c):
      # `comisd a, b` sets CF/ZF like an unsigned compare, so ordered </<= map to
      # the below/below-or-equal conditions (NaN makes them spuriously true, but
      # NIFC's compares assume non-NaN, matching the A64 backend).
      let fbits = g.floatBits(c)
      tag =
        case ek
        of EqC:  (if whenTrue: JeX64 else: JneX64)
        of NeqC: (if whenTrue: JneX64 else: JeX64)
        of LtC:  (if whenTrue: JbX64 else: JaeX64)
        of LeC:  (if whenTrue: JbeX64 else: JaX64)
        else: raiseAssert "arkham x64 v0: float condition not supported: " & $ek
      let (fa, fat) = g.genFReg(c, fbits)
      let (fb, fbt) = g.genFReg(c, fbits)
      let op = if fbits == 32: ComissX64 else: ComisdX64
      g.ab.tree op: g.emFReg fa; g.emFReg fb
      if fbt: g.giveBackF fb
      if fat: g.giveBackF fa
    else:
      var signed = true
      if c.kind == Symbol and g.ra.locationOfSym(symName(c)).typ.kind == AUInt:
        signed = false
      tag =
        case ek
        of EqC:  (if whenTrue: JeX64 else: JneX64)
        of NeqC: (if whenTrue: JneX64 else: JeX64)
        of LtC:  (if whenTrue: (if signed: JlX64 else: JbX64)
                  else:        (if signed: JgeX64 else: JaeX64))
        of LeC:  (if whenTrue: (if signed: JleX64 else: JbeX64)
                  else:        (if signed: JgX64 else: JaX64))
        else: raiseAssert "arkham x64 v0: condition not supported: " & $ek
      let (ar, aOwns) = g.forceReg(g.genVal(c))   # a must be in a register for cmp
      let bv = g.genVal(c)
      var br = NoReg
      var bOwns = false
      var bImm = false
      var bImmV = 0'i64
      if bv.kind == vkImm and bv.imm >= 0 and bv.imm <= 0xFFFF:
        bImm = true; bImmV = bv.imm
      else:
        (br, bOwns) = g.forceReg(bv)
      g.ab.tree CmpX64:
        g.emReg ar
        if bImm: g.ab.intLit bImmV else: g.emReg br
      if bOwns: g.giveBack br
      if aOwns: g.giveBack ar
  g.emJcc(tag, toLabel)

proc emitCondJump(g: var CodeGen; c: var Cursor; toLabel: string; whenTrue: bool) =
  ## Short-circuit conditional jump (and/or/not, comparisons, plain bool value).
  if c.kind == TagLit:
    case c.exprKind
    of AndC:
      c.into:
        if whenTrue:
          let lSkip = g.freshLabel()
          g.emitCondJump(c, lSkip, false)
          g.emitCondJump(c, toLabel, true)
          g.emLab(lSkip)
        else:
          g.emitCondJump(c, toLabel, false)
          g.emitCondJump(c, toLabel, false)
      return
    of OrC:
      c.into:
        if whenTrue:
          g.emitCondJump(c, toLabel, true)
          g.emitCondJump(c, toLabel, true)
        else:
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
  # plain boolean value: branch on `v != 0` / `v == 0`
  let (r, t) = g.forceReg(g.genVal(c))
  g.ab.tree CmpX64: (g.emReg r; g.ab.intLit 0)
  g.emJcc(if whenTrue: JneX64 else: JeX64, toLabel)
  if t: g.giveBack r

# ── calls ─────────────────────────────────────────────────────────────────────

proc externCName(g: var CodeGen; asmName: string): string =
  for ex in g.prog.externOrder:
    if ex.asmName == asmName:
      result = ex.extName
      if result.len > 0 and result[0] == '_': result = result[1 .. ^1]  # strip Darwin '_'
      return
  result = ""

# libc functions arkham lowers to raw Linux/x86-64 syscalls — nifasm's ELF backend
# is static (no dynamic linker / PLT), so an `importc`'d libc call is served by the
# kernel directly. The C ABI arg registers (rdi, rsi, rdx, …) already match the
# syscall ABI for ≤3 args; the 4th syscall arg is r10 (not rcx), handled by the
# dedicated arg-register list in `genCall`.
const LinuxSyscalls = {
  "read": 0, "write": 1, "open": 2, "close": 3, "exit": 60, "exit_group": 231}

proc linuxSyscallNr(name: string): int =
  for (n, nr) in LinuxSyscalls:
    if n == name: return nr
  result = -1

# ── atomic builtins (GCC `__atomic_*` → x86 lock-prefixed instructions) ──────
# x86-64 has a strong memory model: a plain aligned `mov` is already an atomic
# load/store, `xchg` with memory is implicitly locked, and an RMW that returns the
# old value uses `lock xadd` / a `lock cmpxchg` retry loop. The `memorder` arg is
# ignored (all sequences are at least acquire/release), matching the A64 backend.
# Inside a sequence there are no calls, so RAX/RCX/RDX (not in the allocator pool)
# are free scratch; the result lands in RAX (the integer return register).

proc genReg(g: var CodeGen; c: var Cursor): tuple[r: Reg, owns: bool] =
  g.forceReg(g.genVal(c))

proc emMemAt(g: var CodeGen; p: Reg) =        # `(mem p)` — dereference the pointer in p
  g.ab.tree MemX: g.emReg p

proc genAtomicXadd(g: var CodeGen; pReg, val: Reg; returnNew, sub: bool) =
  ## `lock xadd [p], val` (val ← old). For `sub`, negate val first so memory is
  ## decremented. `returnNew` recomputes old±delta into rax; otherwise returns old.
  if returnNew: g.movReg(RDX, val)            # save the original delta (non-pool scratch)
  if sub:
    g.ab.tree NegX64: g.emReg val             # val ← -val
  g.ab.tree LockX64:
    g.ab.tree XaddX64:
      g.emMemAt pReg
      g.emReg val                              # val ← old; [p] += val
  if returnNew:
    let op = if sub: SubX64 else: AddX64
    g.ab.tree op: g.emReg val; g.emReg RDX     # new = old ± delta
  g.movReg(RAX, val)

proc genAtomicLoopRmw(g: var CodeGen; pReg, val: Reg; op: X64Inst) =
  ## `rax = [p]; loop: rdx = rax op val; lock cmpxchg [p], rdx; jne loop`. There
  ## is no lock-fetch form for and/or/xor that yields the old value, so spin on
  ## cmpxchg. Result (old) ends up in rax.
  let lab = g.freshLabel()
  g.ab.tree MovX64: (g.emReg RAX; g.emMemAt pReg)   # rax = [p]
  g.emLab(lab)
  g.movReg(RDX, RAX)
  g.ab.tree op: g.emReg RDX; g.emReg val             # rdx = rax op val (the new value)
  g.ab.tree LockX64:
    g.ab.tree CmpxchgX64:
      g.emMemAt pReg
      g.emReg RDX                                     # if [p]==rax: [p]=rdx else rax=[p]
  g.emJcc(JneX64, lab)                                # retry until cmpxchg succeeds

proc genAtomic(g: var CodeGen; c: var Cursor; builtin: string) =
  ## Lower one `__atomic_*` builtin; `c` is at the first argument. Result → rax.
  case builtin
  of "__atomic_load_n":                        # (ptr, memorder) → *ptr
    let (p, pT) = g.genReg(c); skip c
    g.ab.tree MovX64: (g.emReg RAX; g.emMemAt p)
    if pT: g.giveBack p
  of "__atomic_store_n":                        # (ptr, val, memorder) → void
    let (p, pT) = g.genReg(c)
    let (v, vT) = g.genReg(c); skip c
    g.ab.tree MovX64: (g.emMemAt p; g.emReg v)
    if vT: g.giveBack v
    if pT: g.giveBack p
  of "__atomic_clear":                          # (ptr, memorder) → void; *ptr = 0
    let (p, pT) = g.genReg(c); skip c
    g.movImm(RDX, 0)
    g.ab.tree MovX64: (g.emMemAt p; g.emReg RDX)
    if pT: g.giveBack p
  of "__atomic_thread_fence":                   # (memorder) → void
    skip c
    g.ab.keyword MfenceX64
  of "__atomic_signal_fence":                   # (memorder) → void; compiler barrier only
    skip c
  of "__atomic_exchange_n":                     # (ptr, val, memorder) → old
    let (p, pT) = g.genReg(c)
    let (v, vT) = g.genReg(c); skip c
    g.ab.tree XchgX64: (g.emMemAt p; g.emReg v)  # v ↔ [p] (implicitly locked); v ← old
    g.movReg(RAX, v)
    if vT: g.giveBack v
    if pT: g.giveBack p
  of "__atomic_fetch_add", "__atomic_fetch_sub",
     "__atomic_add_fetch", "__atomic_sub_fetch",
     "__atomic_fetch_and", "__atomic_fetch_or", "__atomic_fetch_xor":
    let (p, pT) = g.genReg(c)
    let (v, vT) = g.genReg(c); skip c
    case builtin
    of "__atomic_fetch_add": g.genAtomicXadd(p, v, returnNew = false, sub = false)
    of "__atomic_fetch_sub": g.genAtomicXadd(p, v, returnNew = false, sub = true)
    of "__atomic_add_fetch": g.genAtomicXadd(p, v, returnNew = true, sub = false)
    of "__atomic_sub_fetch": g.genAtomicXadd(p, v, returnNew = true, sub = true)
    of "__atomic_fetch_and": g.genAtomicLoopRmw(p, v, AndX64)
    of "__atomic_fetch_or":  g.genAtomicLoopRmw(p, v, OrX64)
    of "__atomic_fetch_xor": g.genAtomicLoopRmw(p, v, XorX64)
    else: discard
    if vT: g.giveBack v
    if pT: g.giveBack p
  of "__atomic_test_and_set":                   # (ptr, memorder) → bool (old != 0)
    let (p, pT) = g.genReg(c); skip c
    g.movImm(RDX, 1)
    g.ab.tree XchgX64: (g.emMemAt p; g.emReg RDX)   # rdx ← old; [p] = 1
    let lSkip = g.freshLabel()
    g.movImm(RAX, 0)
    g.ab.tree CmpX64: (g.emReg RDX; g.ab.intLit 0)
    g.emJcc(JeX64, lSkip)
    g.movImm(RAX, 1)
    g.emLab(lSkip)
    if pT: g.giveBack p
  of "__atomic_compare_exchange_n":             # (ptr, exp_ptr, des, weak, succ, fail) → bool
    let (p, pT) = g.genReg(c)
    let (ep, eT) = g.genReg(c)
    g.genInto(c, RCX)                            # desired → rcx (non-pool scratch)
    skip c; skip c; skip c                       # weak, success order, failure order
    g.ab.tree MovX64: (g.emReg RAX; g.emMemAt ep)   # rax = *exp (the comparand)
    g.ab.tree LockX64:
      g.ab.tree CmpxchgX64:
        g.emMemAt p
        g.emReg RCX                              # if [p]==rax: [p]=rcx,ZF=1 else rax=[p],ZF=0
    let lFail = g.freshLabel()
    let lDone = g.freshLabel()
    g.emJcc(JneX64, lFail)
    g.movImm(RAX, 1); g.emJmp(lDone)             # success → 1
    g.emLab(lFail)
    g.ab.tree MovX64: (g.emMemAt ep; g.emReg RAX)   # *exp = actual old value (rax)
    g.movImm(RAX, 0)                             # failure → 0
    g.emLab(lDone)
    if eT: g.giveBack ep
    if pT: g.giveBack p
  else:
    raiseAssert "arkham x64 v0: unsupported atomic builtin: " & builtin

proc genCall(g: var CodeGen; c: var Cursor) =
  ## `(call f arg…)`. The C `exit` extern lowers to the Linux exit syscall; a
  ## declarative user proc uses the SysV register ABI via a `(prepare …)` block
  ## (args → rdi/rsi/…, result ← rax). Each committed arg register is sealed so
  ## marshalling can't clobber it.
  c.into:
    let fsym = symName(c); inc c
    assert g.callTarget.hasKey(fsym), "arkham x64 v0: unknown call target: " & fsym
    let tgt = g.callTarget[fsym]
    let sysNr = if tgt.extern: linuxSyscallNr(g.externCName(tgt.asmName)) else: -1
    if tgt.atomic.len > 0:                     # GCC `__atomic_*` builtin → inline
      g.genAtomic(c, tgt.atomic)               # consumes the args; result in rax
    elif sysNr >= 0:
      # Lower to a raw Linux syscall: args in the syscall ABI registers, number
      # in rax, `syscall`. The result lands in rax (used by the `CallC` path in
      # `genInto`; discarded at statement level).
      const SyscallArgRegs = [RDI, RSI, RDX, R10, R8, R9]
      var idx = 0
      while c.hasMore:
        if idx >= SyscallArgRegs.len:
          raiseAssert "arkham x64 v0: syscall with more than 6 arguments"
        g.genInto(c, SyscallArgRegs[idx]); inc idx
      g.movImm(RAX, sysNr.int64)
      g.emSyscall()
    elif tgt.declarative and not tgt.extern:
      var sealedHere: set[Reg] = {}
      var argCurs: seq[Cursor] = @[]           # one cursor per argument expression
      while c.hasMore: (argCurs.add c; skip c)
      let nReg = min(argCurs.len, g.md.intArgRegs.len)
      let nStack = argCurs.len - nReg          # 7th+ args go through the stack
      g.ab.tree PrepareX64:
        g.ab.sym tgt.asmName
        # Phase 1 — register args → rdi…r9, each bound to the callee's param and
        # sealed so later marshalling can't clobber it. Evaluated before rsp moves.
        for idx in 0 ..< nReg:
          var a = argCurs[idx]
          if g.isFloatExpr(a): raiseAssert "arkham x64 v0: float call argument"
          let ar = g.md.intArgRegs[idx]
          g.genInto(a, ar)
          g.ab.tree MovX64:                    # bind it to the callee's param p.idx
            g.ab.tree ArgX: g.ab.sym paramName(idx)
            g.emReg ar
          g.ra.seal ar; sealedHere.incl ar
        # Phase 2 — reserve the outgoing stack area (`csize` keeps rsp 16-aligned),
        # then evaluate each stack arg into a reused temp and store it; nifasm
        # resolves `(arg p.k)` to its byte offset. x64 has only two scratch GPRs,
        # so args are materialized one at a time rather than all held at once —
        # hence the evaluation happens after the `sub rsp`, which is fine for the
        # literals/register locals the flattener leaves (an rsp-relative stack
        # local would read at the wrong offset and is rejected below).
        if nStack > 0:
          g.ab.tree SubX64: g.ab.reg RSP; g.ab.keyword CsizeX
          for k in 0 ..< nStack:
            let idx = nReg + k
            var a = argCurs[idx]
            if g.isFloatExpr(a): raiseAssert "arkham x64 v0: float call argument"
            if a.kind == Symbol and
               g.ra.locationOfSym(symName(a)).kind in {NamedStack, OnStack}:
              raiseAssert "arkham x64 v0: rsp-relative local as a stack call argument"
            let t = g.borrowTmp()
            g.genInto(a, t)
            g.ab.tree MovX64:
              g.ab.tree MemX:
                g.ab.reg RSP
                g.ab.tree ArgX: g.ab.sym paramName(idx)
              g.emReg t
            g.giveBack t
        g.ab.keyword CallX64
        if nStack > 0:
          g.ab.tree AddX64: g.ab.reg RSP; g.ab.keyword CsizeX
        if not retIsVoid(tgt.retType):
          g.ab.tree MovX64:
            g.emReg RAX
            g.ab.tree ResX: g.ab.sym "ret.0"
      g.ra.unseal sealedHere
    else:
      raiseAssert "arkham x64 v0: unsupported call target: " & fsym

# ── statements ─────────────────────────────────────────────────────────────────

proc genStmt(g: var CodeGen; c: var Cursor)

proc genVarDecl(g: var CodeGen; c: var Cursor) =
  c.into:
    let name = symName(c); inc c
    skip c                                    # pragmas
    g.symType[name] = c                       # record the type for getType
    let typeCur = c
    skip c                                    # type
    let loc = g.ra.locationOfSym(name)
    case loc.kind
    of InReg:
      g.emRegLocalVar(name, loc.r, typeCur)   # (var :name (reg) type) — typed for nifasm
      if c.hasMore and c.kind != DotToken: g.genInto(c, loc.r)
    of InFReg:                                # float local in an xmm register
      if c.hasMore and c.kind != DotToken:
        g.genIntoF(c, loc.f, loc.typ.size * 8)
    of NamedStack:
      if loc.typ.kind == AFloat:              # a spilled float scalar
        let bits = loc.typ.size * 8
        g.emFloatStackVar(name, bits)
        if c.hasMore and c.kind != DotToken:
          let f = g.borrowFTmp()
          g.genIntoF(c, f, bits)
          g.emFloatScalarStore(name, f, bits)
          g.giveBackF f
      elif loc.typ.kind == AMem:              # an aggregate stack object
        g.ab.open NifasmDecl.VarD             # (var :name (s) <type>)
        g.ab.symDef name
        g.ab.keyword SO
        var tc = typeCur                      # named type ref, or inline structural type
        if tc.kind == Symbol: g.ab.sym symName(tc)
        else: g.genTypeBody(tc)               # e.g. an inline `(array (i N) len)`
        g.ab.close()
        if tc.kind == Symbol: g.varType[name] = symName(tc)  # object field-offset lookups
        if c.hasMore and c.kind != DotToken:
          if c.kind == TagLit and c.exprKind == OconstrC:
            c.into:                           # (oconstr Type (kv Field Value)*)
              skip c                          # the constructed type
              while c.hasMore:
                assert c.substructureKind == KvU, "arkham x64 v0: oconstr expects (kv …)"
                c.into:
                  let field = symName(c); inc c
                  let (r, owns) = g.forceReg(g.genVal(c))
                  g.ab.tree MovX64:           # store directly (synthetic field, no cursor)
                    g.emFieldMem(name, field)
                    g.emReg r
                  if owns: g.giveBack r
                  while c.hasMore: skip c      # optional inherited-depth INTLIT
          else:                               # aggregate copy-init → byte copy (TODO)
            raiseAssert "arkham x64 v0: aggregate copy-initializer not supported: " & name
      else:                                   # a spilled / address-taken scalar
        g.emScalarStackVar(name)              # (var :name (s) (i 64))
        if c.hasMore and c.kind != DotToken:
          let (r, owns) = g.forceReg(g.genVal(c))
          g.ab.tree MovX64:                   # store directly (synthetic slot, no cursor)
            g.emStackMem(name)
            g.emReg r
          if owns: g.giveBack r
    else: raiseAssert "arkham x64 v0: local '" & name & "' has location " & $loc.kind
    while c.hasMore: skip c

proc genAsgn(g: var CodeGen; c: var Cursor) =
  c.into:
    let l = g.asLvalue(c)                      # consumes the lvalue
    if l.slot.kind == AFloat:                  # float target (register or spilled)
      let bits = l.slot.size * 8
      if l.kind == lvFReg:
        g.genIntoF(c, l.f, bits)
      else:
        let f = g.borrowFTmp()
        g.genIntoF(c, f, bits)
        g.emitStoreF(l, f, bits)
        g.giveBackF f
    else:
      case l.kind
      of lvReg: g.genInto(c, l.r)
      of lvAggrVar:
        raiseAssert "arkham x64 v0: assignment target of kind " & $l.kind
      else:                                    # memory target: RHS → reg → store
        let (r, owns) = g.forceReg(g.genVal(c))  # (imm→mem isn't supported by nifasm)
        g.emitStore(l, r)
        if owns: g.giveBack r
    while c.hasMore: skip c

proc genWhile(g: var CodeGen; c: var Cursor) =
  let lStart = g.freshLabel()
  let lEnd = g.freshLabel()
  g.loopEnds.add lEnd
  c.into:
    let condStart = c
    skip c                                    # `c` → first body statement
    g.emLab(lStart)
    var cond = condStart
    g.emitCondJump(cond, lEnd, whenTrue = false)
    while c.hasMore: genStmt(g, c)
    g.emJmp(lStart)
  g.emLab(lEnd)
  discard g.loopEnds.pop()

proc genBreak(g: var CodeGen; c: var Cursor) =
  assert g.loopEnds.len > 0, "arkham x64 v0: `break` outside a loop"
  g.emJmp(g.loopEnds[^1])
  skip c

# ── if / case ────────────────────────────────────────────────────────────────

proc genActionStmts(g: var CodeGen; c: var Cursor) =
  ## The body of an `(elif … body)` / `(else body)` / case branch — a `(stmts …)`
  ## list (not a fresh scope) or a single statement.
  if c.stmtKind == StmtsS:
    c.into:
      while c.hasMore: genStmt(g, c)
  else:
    genStmt(g, c)

proc emitChain(g: var CodeGen; c: var Cursor; lEnd: string) =
  if not c.hasMore: return
  case c.substructureKind
  of ElifU:
    var branch = c
    skip c                                    # `c` → the rest of the chain
    let lNext = g.freshLabel()
    branch.into:
      g.emitCondJump(branch, lNext, whenTrue = false)
      g.genActionStmts(branch)
      g.emJmp(lEnd)
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

proc branchImm(c: var Cursor): int64 =
  ## A NIFC `BranchValue` (Number | CharLiteral | (true) | (false)); advance past it.
  case c.kind
  of IntLit:  result = intVal(c); inc c
  of UIntLit: result = cast[int64](uintVal(c)); inc c
  of CharLit: result = int64(ord(charLit(c))); inc c
  of TagLit:
    case c.exprKind
    of TrueC:  result = 1; skip c
    of FalseC: result = 0; skip c
    else: raiseAssert "arkham x64 v0: unsupported case branch value: " & $c.exprKind
  else: raiseAssert "arkham x64 v0: unsupported case branch value kind: " & $c.kind

proc cmpImm(g: var CodeGen; selReg: Reg; v: int64) =
  ## `cmp selReg, v` — immediate when small, else via a scratch register.
  if v >= 0 and v <= 0xFFFF:
    g.ab.tree CmpX64: (g.emReg selReg; g.ab.intLit v)
  else:
    let tmp = g.borrowTmp()
    g.movImm(tmp, v)
    g.ab.tree CmpX64: (g.emReg selReg; g.emReg tmp)
    g.giveBack tmp

proc emitRangeTest(g: var CodeGen; selReg: Reg; c: var Cursor;
                   lBody: string; signed: bool) =
  ## One `BranchRange` against `selReg`; jump to `lBody` on a match.
  if c.kind == TagLit and c.substructureKind == RangeU:
    c.into:
      let lo = branchImm(c)
      let hi = branchImm(c)
      let lSkip = g.freshLabel()              # match iff lo <= sel <= hi
      g.cmpImm(selReg, lo)
      g.emJcc(if signed: JlX64 else: JbX64, lSkip)
      g.cmpImm(selReg, hi)
      g.emJcc(if signed: JgX64 else: JaX64, lSkip)
      g.emJmp(lBody)
      g.emLab(lSkip)
  else:
    g.cmpImm(selReg, branchImm(c))
    g.emJcc(JeX64, lBody)

proc genCase(g: var CodeGen; c: var Cursor) =
  ## `(case Expr (of (ranges BranchRange+) StmtList)* (else StmtList)?)`. Selector
  ## → a register; each branch's tests jump to its body; a non-match falls through
  ## to `else` (or the end). NIFC `case` has no fall-through, so bodies end in a jmp.
  let lEnd = g.freshLabel()
  c.into:
    var signed = true
    if c.kind == Symbol and g.ra.locationOfSym(symName(c)).typ.kind == AUInt:
      signed = false
    let (selReg, selTemp) = g.forceReg(g.genVal(c))   # selector, live across all tests
    var bodies: seq[(string, Cursor)] = @[]
    var elseBody = c
    var hasElse = false
    while c.hasMore:
      case c.substructureKind
      of OfU:
        let lBody = g.freshLabel()
        var branch = c
        skip c
        branch.into:                          # branch → (ranges …) then StmtList
          assert branch.substructureKind == RangesU, "arkham x64: case `of` needs `ranges`"
          branch.into:
            while branch.hasMore:
              g.emitRangeTest(selReg, branch, lBody, signed)
          bodies.add (lBody, branch)
          skip branch
      of ElseU:
        elseBody = c; hasElse = true; skip c
      else: skip c
    if selTemp: g.giveBack selReg
    if hasElse:
      elseBody.into:
        g.genActionStmts(elseBody)
    g.emJmp(lEnd)
    for (lBody, bc) in bodies:
      g.emLab(lBody)
      var body = bc
      g.genActionStmts(body)
      g.emJmp(lEnd)
  g.emLab(lEnd)

proc genStmt(g: var CodeGen; c: var Cursor) =
  case c.stmtKind
  of ScopeS:                                  # only `scope` is a fresh scope
    g.enterScope()                            # register locals here `kill` at close
    c.into:
      while c.hasMore: genStmt(g, c)
    g.exitScope()
  of StmtsS:                                  # a statement list — not a fresh scope
    c.into:
      while c.hasMore: genStmt(g, c)
  of VarS, GvarS, TvarS, ConstS:
    genVarDecl(g, c)
  of CallS:
    genCall(g, c)
  of AsgnS:
    genAsgn(g, c)
  of WhileS:
    genWhile(g, c)
  of IfS:
    genIf(g, c)
  of CaseS:
    genCase(g, c)
  of BreakS:
    genBreak(g, c)
  of RetS:
    # The Linux entry must terminate the process; a normal proc returns in rax.
    if g.isEntryProc:
      c.into:
        if c.hasMore and c.kind != DotToken: g.genInto(c, RDI)  # exit code → rdi
        else: g.movImm(RDI, 0)
      g.movImm(RAX, 60)
      g.emSyscall()
    else:
      c.into:
        if c.hasMore and c.kind != DotToken: g.genInto(c, RAX)
      g.framePop()                            # restore callee-saved before returning
      g.ab.keyword RetX64
  else:
    raiseAssert "arkham x64 v0: statement not supported: " & $c.stmtKind

# ── type + proc + module emission ───────────────────────────────────────────
# genTypeBody/genType emit nifasm `NifasmType` tags (arch-neutral). TODO: share
# with codegen_a64 by lifting these into codegen_common.

proc genTypeBody(g: var CodeGen; c: var Cursor) =
  ## Translate a NIFC type at `c` into asm-NIF, advancing past it. Named types
  ## are inlined; object field pragmas are dropped. v0: int/uint/bool/ptr + objects.
  case c.kind
  of Symbol:
    var d = lookupType(g.prog, symName(c))
    d.into:
      inc d; skip d                           # name, type-pragmas
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
        c.into: g.genTypeBody(c)              # pointee
    of ArrayT:
      c.into:
        g.ab.arrayType:
          g.genTypeBody(c)
          if c.kind == IntLit: (g.ab.intLit intVal(c); inc c)
          else: raiseAssert "arkham x64 v0: array length must be a literal"
    of ObjectT:
      c.into:
        skip c                                # inheritance (`.`)
        g.ab.objectType:
          while c.hasMore:
            c.into:                           # (fld :name pragmas type)
              let fn = symName(c); inc c
              skip c                          # field pragmas (dropped)
              g.ab.fldDef(fn):
                g.genTypeBody(c)
    of EnumT:                                 # an enum is just its base integer type
      c.into:
        g.genTypeBody(c)                      # (enum <base> (efld …)…) → <base>
        while c.hasMore: skip c               # efld declarations (dropped)
    else:
      raiseAssert "arkham x64 v0: type not supported: " & $c.typeKind
  else:
    raiseAssert "arkham x64 v0: malformed type"

proc genType(g: var CodeGen; name: string; decl: Cursor) =
  ## `(type :name <body>)` — nifasm's stack-slot allocator consults it for field
  ## offsets.
  var c = decl
  c.into:
    inc c                                     # name
    skip c                                    # type-pragmas
    g.ab.tree TypeD:
      g.ab.symDef name
      g.genTypeBody(c)

proc emitSignature(g: var CodeGen; decl: Cursor; declarative: bool) =
  ## `(params)/(result)/(clobber)`. Declarative procs state the SysV register ABI
  ## — positional `p.i` params in rdi/rsi/… and an rax result — so nifasm
  ## cross-checks every call site; the clobber set is always the convention's.
  var numParams = 0
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
                  g.ab.symDef paramName(idx)
                  if idx < g.md.intArgRegs.len:
                    g.ab.reg g.md.intArgRegs[idx]   # rdi,rsi,rdx,rcx,r8,r9
                  else:
                    g.ab.keyword SO            # 7th+ → stack-passed `(s)` (caller marshals)
                  g.genTypeBody(c)            # the param type (consumes it)
                while c.hasMore: skip c
              inc idx
          numParams = idx
        else:
          skip c                              # no params slot
      g.ab.tree ResultD:                      # c now at the return type
        if retIsVoid(c):
          skip c
        else:
          g.ab.symDef "ret.0"
          g.ab.reg RAX
          g.genTypeBody(c)                    # the result type (consumes it)
      while c.hasMore: skip c                 # pragmas, body
  else:
    g.ab.keyword ParamsD
    g.ab.keyword ResultD
  # The clobber set excludes the parameter registers: those hold live params on
  # entry, and nifasm treats a declared-clobbered register as clobbered there, so
  # listing them would make the body unable to read its own params. The caller
  # already accounts for the arg/result registers via the ABI.
  var paramRegs: set[Reg] = {}
  for i in 0 ..< min(numParams, g.md.intArgRegs.len): paramRegs.incl g.md.intArgRegs[i]
  g.ab.tree ClobberD:
    for r in x64ClobbersGpr:
      if r notin paramRegs: g.ab.reg r

proc emitParamMoves(g: var CodeGen; decl: Cursor) =
  ## Settle each register-passed parameter into its allocated home. A param the
  ## allocator left in its incoming arg register becomes the named local `p.i`
  ## (x64 refers to a bound register by name); one the allocator relocated to a
  ## callee-saved register (because it lives across a call) is `mov`'d there as a
  ## *raw* register — never a named local, so the epilogue can `pop` that
  ## callee-saved reg without a "kill it first" binding conflict. Stack-passed
  ## params (7th+) are handled by `emitStackParamLoadsX64`.
  var c = decl
  inc c                                       # proc head → name
  inc c                                       # name → params slot
  if c.kind != TagLit: return                 # no parameters
  var idx = 0
  c.into:
    while c.hasMore:
      var nm = ""
      c.into:                                 # (param :name pragmas type)
        nm = symName(c); inc c
        skip c                                # pragmas
        g.symType[nm] = c                     # record the param's type for getType
        if c.kind == Symbol and slotOf(g.prog, c).kind == AMem:
          raiseAssert "arkham x64 v0: aggregate parameter not supported: " & nm
        while c.hasMore: skip c
      if idx < g.md.intArgRegs.len:           # register-passed parameter
        let argReg = g.md.intArgRegs[idx]
        let loc = g.ra.locationOfSym(nm)
        if loc.kind == InReg and loc.r == argReg:
          g.regLocal[argReg] = paramName(idx)   # stays in its arg reg → named local
        elif loc.kind == InReg:
          g.movReg(loc.r, argReg)               # relocated to a callee-saved home (raw)
        else:
          raiseAssert "arkham x64 v0: spilled / float parameter: " & nm
      # else: stack-passed (7th+) — loaded by emitStackParamLoadsX64.
      inc idx

# ── stack frame: callee-saved save/restore + incoming stack parameters ───────
# x86-64 has no pair store, so each used callee-saved GPR is a single `push`/`pop`.
# Frames are needed when the proc uses a callee-saved register (for a cross-call
# local or a stack-param home). Saved registers stay RAW (`(rbx)`), never named
# locals, so the epilogue can pop them without nifasm's bound-register guard.

proc computeFrameX64(g: var CodeGen; isEntry, hasCall: bool) =
  g.frameRegs = @[]
  for r in g.md.intCalleeSaved:
    if r in g.ra.usedCallee: g.frameRegs.add r
  # SysV requires rsp ≡ 0 (mod 16) at a `call`. The kernel enters the entry with
  # rsp ≡ 0; a normal callee is entered with rsp ≡ 8 (the caller's pushed return
  # address). Each saved reg is 8 bytes, so after the pushes the parity may be
  # wrong — pad with an extra 8 when this proc itself makes a call.
  g.framePad = 0
  if hasCall:
    let entryBias = if isEntry: 0 else: 8
    if (entryBias + 8 * g.frameRegs.len) mod 16 != 0: g.framePad = 8
  g.hasFrame = g.frameRegs.len > 0 or g.framePad > 0

proc framePushBytesX64(g: CodeGen): int =
  ## Bytes between the current rsp (after the callee-saved pushes, before the pad)
  ## and the caller's first stack argument: the return address (8) plus each saved
  ## register (8). Used to address incoming stack params.
  8 + 8 * g.frameRegs.len

proc framePush(g: var CodeGen) =
  for r in g.frameRegs:
    g.ab.tree PushX64: g.ab.reg r                          # raw push

proc framePop(g: var CodeGen) =
  if g.framePad > 0: g.binImm(AddX64, RSP, g.framePad.int64)
  for i in countdown(g.frameRegs.high, 0):
    g.ab.tree PopX64: g.ab.reg g.frameRegs[i]             # raw pop, reverse order

proc emitStackParamLoadsX64(g: var CodeGen; decl: Cursor) =
  ## Load the 7th+ integer/pointer parameters from the caller's outgoing argument
  ## area into their allocated (callee-saved) register homes. Emitted right after
  ## `framePush` and before the alignment pad, so each arg sits at the statically
  ## known offset `framePushBytes + k*8` from the current rsp.
  var c = decl
  inc c; inc c                                # → params slot
  if c.kind != TagLit: return
  let base = g.framePushBytesX64()
  var idx = 0
  var stackOrd = 0
  c.into:
    while c.hasMore:
      var nm = ""
      c.into:                                 # (param :name pragmas type)
        nm = symName(c); inc c
        skip c
        while c.hasMore: skip c
      if idx >= g.md.intArgRegs.len:
        let loc = g.ra.locationOfSym(nm)
        assert loc.kind == InReg,
          "arkham x64 v0: stack parameter without a register home: " & nm
        g.ab.tree MovX64:                     # home ← [rsp + base + stackOrd*8]
          g.emReg loc.r                       # raw callee-saved reg
          g.ab.tree MemX:
            g.ab.reg RSP
            g.ab.intLit (base + stackOrd * 8)
        inc stackOrd
      inc idx

# ── thread-local storage ─────────────────────────────────────────────────────
# nifasm accesses an x86-64 thread-local as `FS:[off]` (it resolves a tvar symbol
# to a displacement-only FS-segment memory operand). The kernel zeroes the FS base
# at process entry, so arkham points it at a static `.bss` block via
# `arch_prctl(ARCH_SET_FS, &block)` in the entry prologue. Single-threaded, so
# per-thread == per-process. Non-zero literal initializers are stored at entry
# (the block is zero-filled); nifasm bakes no x64 TLS init template.

const TlsBlockName = "arkham.tls.0"  # distinct basename so nifasm's scope keys it uniquely
const ArchSetFs = 0x1002             # arch_prctl(2) subfunction
const ArchPrctlNr = 158              # Linux x86-64 syscall number

proc tlsBlockSize(g: var CodeGen): int =
  ## Total bytes the FS block must cover: the sum of each tvar's aligned size,
  ## matching nifasm's sequential `tlsOffset += alignedSize` allocation (summing
  ## over *all* tvars is an upper bound on its reachable subset).
  result = 0
  for name, decl in g.tvars:
    var c = decl
    c.into:
      inc c; skip c                              # name, pragmas
      let s = slotOf(g.prog, c)
      let a = max(s.align, 1)
      result += (s.size + a - 1) and not (a - 1)
      while c.hasMore: skip c
  if result < 16: result = 16                     # keep the block 16-byte sized/aligned

proc genTvar(g: var CodeGen; name: string; decl: Cursor) =
  ## Emit `(tvar :name <type> <intlit>?)`. nifasm allocates the FS offset; the
  ## optional literal is carried (parsed but unused on x64 — `emitTlsSetup` stores
  ## non-zero initializers at runtime since `.bss` defaults to zero).
  var c = decl
  c.into:                                         # (tvar SymbolDef VarPragmas Type Value?)
    inc c; skip c                                 # name, pragmas
    g.ab.open NifasmDecl.TvarD
    g.ab.symDef name
    g.genTypeBody(c)                              # type
    if c.kind == IntLit:
      g.ab.intLit intVal(c)
    elif c.kind != DotToken:
      raiseAssert "arkham x64: thread-local initializer must be an integer literal: " & name
    g.ab.close()
    while c.hasMore: skip c

proc emitTlsSetup(g: var CodeGen) =
  ## Entry prologue: point FS at the TLS block, then run literal initializers.
  g.emGlobalAddr(RSI, TlsBlockName)               # rsi ← &block
  g.movImm(RDI, ArchSetFs)
  g.movImm(RAX, ArchPrctlNr)
  g.emSyscall()                                   # arch_prctl(ARCH_SET_FS, &block)
  for name, decl in g.tvars:
    var c = decl
    c.into:
      inc c; skip c; skip c                       # name, pragmas, type
      if c.hasMore and c.kind == IntLit:
        let v = intVal(c)
        if v != 0:
          let r = g.borrowTmp()
          g.movImm(r, v)
          g.ab.tree MovX64:                       # FS:[off] ← r  (tvar symbol resolves to mem)
            g.ab.sym name
            g.emReg r
          g.giveBack r
      while c.hasMore: skip c

proc genProc(g: var CodeGen; info: ProcInfo) =
  # Unlike A64 (where a thread-local goes through a TLV-descriptor thunk call), x64
  # reads/writes a tvar directly as an FS-segment operand — no call — so tvar
  # accesses must NOT mark the proc non-leaf. Hence the empty tvar set here.
  let an = analyseProc(info.decl)
  g.varType = initTable[string, string]()
  g.symType = initTable[string, Cursor]()
  g.retAggrName = ""; g.retIndirect = false; g.retIsFloat = false
  g.indirectReg = NoReg
  g.isEntryProc = info.isEntry
  g.regLocal = initTable[Reg, string]()       # per-proc named-local bindings
  g.scopeLocals = @[]
  g.ra = allocateProc(g.buf[], info.decl, an, g.prog, x64Machine)
  g.initFreeTmp()
  g.computeFrameX64(info.isEntry, an.hasCall)
  g.ab.tree ProcD:
    g.ab.symDef info.asmName
    g.emitSignature(info.decl, isDeclarativeAbi(g.prog, info.decl))
    g.ab.tree StmtsX64:
      g.enterScope()                          # the proc body's scope
      g.framePush()                           # save the used callee-saved GPRs
      g.emitStackParamLoadsX64(info.decl)      # incoming 7th+ args → their reg homes
      if g.framePad > 0:                      # 16-byte alignment for outgoing calls
        g.binImm(SubX64, RSP, g.framePad.int64)
      g.emitParamMoves(info.decl)             # settle register params
      if info.isEntry and g.tvars.len > 0:
        g.emitTlsSetup()                      # set FS base + thread-local initializers
      if info.isEntry: g.emitGlobalInits()    # run module-level var initializers
      var c = info.decl
      c.into:
        inc c; skip c; skip c; skip c         # name, params, return type, pragmas
        if c.stmtKind == StmtsS:
          genStmt(g, c)
      g.exitScope()                           # `kill` the proc's register locals
      # Fallthrough terminator: the entry exits the process; a normal proc
      # restores its frame and returns.
      if info.isEntry:
        g.movImm(RAX, 60); g.movImm(RDI, 0); g.emSyscall()
      else:
        g.framePop()
        g.ab.keyword RetX64

proc genGlobal(g: var CodeGen; name: string; decl: Cursor) =
  ## `(gvar :name <type>)` — a zero-initialized `.bss` global (also `const`); any
  ## initializer is run at program entry by `emitGlobalInits`.
  var c = decl
  c.into:                                       # (gvar SymbolDef VarPragmas Type Value?)
    inc c                                       # name
    skip c                                      # pragmas
    g.ab.open NifasmDecl.GvarD
    g.ab.symDef name
    g.genTypeBody(c)                            # type
    g.ab.close()
    while c.hasMore: skip c                      # value (initialized at entry)

proc emitGlobalInits(g: var CodeGen) =
  ## At program entry, store each global's initializer (if any) into its slot.
  for name, decl in g.globals:
    var c = decl
    c.into:
      inc c; skip c                             # name, pragmas
      let gslot = slotOf(g.prog, c)             # the global's declared type
      skip c                                    # type
      if c.hasMore and c.kind != DotToken:
        if gslot.kind == AFloat:                 # float global → movss/movsd [&g], xmm
          let gbits = if gslot.size == 4: 32 else: 64
          let fv = g.borrowFTmp()
          g.genIntoF(c, fv, gbits)
          let p = g.borrowTmp()
          g.emGlobalAddr(p, name)
          let op = if gbits == 32: MovssX64 else: MovsdX64
          g.ab.tree op:
            g.ab.tree MemX: g.emReg p
            g.emFReg fv
          g.giveBack p
          g.giveBackF fv
        else:
          let v = g.borrowTmp()
          g.genInto(c, v)                          # evaluate the initializer
          let p = g.borrowTmp()
          g.emGlobalAddr(p, name)
          g.ab.tree MovX64:
            g.ab.tree MemX: g.emReg p
            g.emReg v
          g.giveBack p
          g.giveBack v
      while c.hasMore: skip c

proc generateX64*(buf: var TokenBuf; inputPath: string; tags: TagPool): string =
  ## Compile a parsed NIFC module to x86-64 / Linux asm-NIF text.
  var g = CodeGen(ab: initAsmBuf(), buf: addr buf, md: x64Machine)
  g.ab.renderReg = x64RegName                 # render register slots as x86 names
  g.prog = collect(buf, inputPath, tags)
  g.callTarget = g.prog.callTarget
  g.globals = g.prog.globals
  g.tvars = g.prog.tvars
  for nm in g.tvars.keys: g.tvarNames.incl nm
  g.ab.tree StmtsX64:
    g.ab.tree ArchD: g.ab.ident "x64"
    for (name, decl) in g.prog.mainTypeList:
      g.genType(name, decl)
    for name, decl in g.prog.globals:
      g.genGlobal(name, decl)
    if g.tvars.len > 0:
      # The static FS-segment block backing all thread-locals (see `emitTlsSetup`).
      g.ab.open NifasmDecl.GvarD
      g.ab.symDef TlsBlockName
      g.ab.arrayType:
        g.ab.uintType(8)
        g.ab.intLit g.tlsBlockSize()
      g.ab.close()
    for name, decl in g.prog.tvars:
      g.genTvar(name, decl)
    for info in g.prog.procs:
      genProc(g, info)
    for (nm, bytes) in g.rodata:
      g.ab.tree RodataD:
        g.ab.symDef nm
        g.ab.str bytes
  result = g.ab.render()

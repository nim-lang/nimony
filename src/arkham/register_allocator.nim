#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Pass 2: register allocation.
##
## Every value-producing position in a proc body gets a `Location`, looked up
## by `cursorToPosition` (the `SymId→position` mapping for locals is kept
## *inside* the allocator — that is its job). This module assigns the
## long-lived storage: parameters (ABI pre-coloring) and locals (scope-based).
## Per-expression temporaries are filled into the same `locs` table by the
## code generator as it walks the tree (it borrows registers via `borrowReg`
## / `giveBack`); see codegen. There is no Sethi–Ullman numbering — registers
## are handed out in the natural traversal order.
##
## Strategy for locals (after `analyser`):
##  * address-taken / aggregate / float (v1)        → stack slot
##  * confined to a call-free scope (`AllRegs`)      → a volatile temp register
##  * otherwise (may be live across a call)          → a callee-saved register
##  * out of registers                               → stack slot
## A register is returned to its pool when its scope closes (sibling scopes
## reuse it); the union of callee-saved registers ever used drives the
## prologue/epilogue. Weight-based stealing is a later refinement.

import std / [tables, assertions]
import nifcore, nifcdecl, slots, machine, analyser, programs

type
  RegAlloc* = object
    locs*: seq[Location]              ## indexed by cursorToPosition
    symPos*: Table[string, int]       ## local/param name → its def position
    usedCallee*: set[Reg]             ## callee-saved GPRs to save in prologue
    usedCalleeF*: set[FReg]           ## callee-saved SIMD regs (v8–v15) to save in prologue
    frameSize*: int                   ## bytes of stack frame for spilled slots
    hasStackVars*: bool               ## proc has nifasm-managed `(s)` aggregate vars
    sealed*: set[Reg]                 ## registers pinned to an in-flight ABI
                                      ## call (args being marshalled, x8 result,
                                      ## values live through the call): never
                                      ## allocate to or steal from these

  Builder = object
    ra: RegAlloc
    buf: ptr TokenBuf
    an: ptr ProcAnalysis
    prog: ptr Program                 ## program (for cross-module type resolution / sizing)
    freeVol, freeCallee: set[Reg]
    freeVolF: set[FReg]               ## caller-saved SIMD/FP scratch pool (v16–v31)
    freeCalleeF: set[FReg]            ## callee-saved SIMD pool (v8–v15)
    scopeVars: seq[seq[string]]       ## register-eligible locals per open scope
                                      ## (steal candidates; freed by current loc)

proc posOf(b: Builder; c: Cursor): int {.inline.} =
  cursorToPosition(b.buf[], c)

# ── physical register pools ────────────────────────────────────────────────

proc takeReg(b: var Builder; pool: var set[Reg]; cands: openArray[Reg]): Reg =
  ## Take the first free, non-sealed register from `cands`. A sealed register
  ## is committed to an in-flight ABI call and must never be h(re)allocated.
  for r in cands:
    if r in pool and r notin b.ra.sealed:
      excl pool, r
      return r
  result = NoReg

proc takeFReg(b: var Builder; pool: var set[FReg]; cands: openArray[FReg]): FReg =
  ## Take the first free SIMD register from `cands` out of `pool`.
  for f in cands:
    if f in pool:
      excl pool, f
      return f
  result = NoFReg

proc spill(b: var Builder; slot: AsmSlot): Location =
  b.ra.frameSize += align(max(slot.size, 1), 8)
  result = stackLoc(-b.ra.frameSize, slot)

proc allocStorage(b: var Builder; slot: AsmSlot; props: VarProps): Location =
  ## Decide where one local/param lives. Records reg use for scope freeing.
  if slot.isFloat:
    # Floats in a call-free region use caller-saved scratch (v16–v31); those
    # live across a call use callee-saved (v8–v15), saved in the prologue.
    # Address-taken floats fall back to a (codegen-unsupported) slot.
    if AddrTaken in props: return b.spill(slot)
    var f: FReg
    if AllRegs in props:
      f = b.takeFReg(b.freeVolF, FloatTempRegs)
      if f == NoFReg: f = b.takeFReg(b.freeCalleeF, FloatCalleeSaved)
    else:
      f = b.takeFReg(b.freeCalleeF, FloatCalleeSaved)
    if f == NoFReg: return b.spill(slot)
    if f in {V8..V15}: b.ra.usedCalleeF.incl f
    return fregLoc(f, slot)
  if AddrTaken in props or not slot.inRegClass:
    return b.spill(slot)
  var r: Reg
  if AllRegs in props:
    r = b.takeReg(b.freeVol, IntTempRegs)
    if r == NoReg: r = b.takeReg(b.freeCallee, IntCalleeSaved)
  else:
    # may be live across a call → must be callee-saved (or stack)
    r = b.takeReg(b.freeCallee, IntCalleeSaved)
  if r == NoReg: return b.spill(slot)
  if r in {X19..X28}: b.ra.usedCallee.incl r
  result = regLoc(r, slot)

proc giveBack(b: var Builder; r: Reg) {.inline.} =
  if r in {X19..X28}: b.freeCallee.incl r
  elif r != NoReg: b.freeVol.incl r

proc giveBackF(b: var Builder; f: FReg) {.inline.} =
  if f in {V8..V15}: b.freeCalleeF.incl f
  elif f != NoFReg: b.freeVolF.incl f

proc weightOf(b: Builder; name: string): int {.inline.} =
  b.an.vars.getOrDefault(name).weight

# ── scope-based walk that allocates locals ──────────────────────────────────

proc openScope(b: var Builder) = b.scopeVars.add @[]
proc closeScope(b: var Builder) =
  ## Return registers to the pool, keyed by each var's *current* location —
  ## so a var that was evicted to the stack (its reg stolen by a hotter one)
  ## frees nothing, and the thief frees the register when its own scope ends.
  for v in b.scopeVars.pop():
    let loc = b.ra.locs[b.ra.symPos[v]]
    if loc.kind == InReg: b.giveBack loc.r
    elif loc.kind == InFReg: b.giveBackF loc.f

proc record(b: var Builder; pos: int; name: string; loc: Location) =
  b.ra.symPos[name] = pos
  b.ra.locs[pos] = loc

proc trySteal(b: var Builder; curName: string; curSlot: AsmSlot;
              curProps: VarProps; fallback: Location): Location =
  ## `curName` wanted a register but the pool was empty (`fallback` is a stack
  ## slot). Evict the lowest-weight live local that holds a usable register, if
  ## it is strictly colder than `curName`; that local moves to `fallback` and
  ## `curName` takes its register. Returns `curName`'s chosen location.
  let curW = b.weightOf(curName)
  let calleeOnly = AllRegs notin curProps   # cross-call var needs callee-saved
  var bestV = ""
  var bestW = curW
  var bestReg = NoReg
  for scope in b.scopeVars:
    for v in scope:
      let vloc = b.ra.locs[b.ra.symPos[v]]
      if vloc.kind != InReg: continue
      if vloc.r in b.ra.sealed: continue        # pinned to an in-flight ABI call
      if calleeOnly and vloc.r notin {X19..X28}: continue
      let vw = b.weightOf(v)
      if vw < bestW:
        bestW = vw; bestV = v; bestReg = vloc.r
  if bestReg == NoReg: return fallback      # nothing colder to steal from
  # evict the victim to the (current's) stack slot; current takes its register
  let vpos = b.ra.symPos[bestV]
  # The victim moves to a nifasm-managed `(s)` slot, addressed by its own name
  # (offsets are nifasm's job). `fallback`'s numeric offset is irrelevant now.
  b.ra.locs[vpos] = namedStackLoc(bestV, b.ra.locs[vpos].typ)
  b.ra.hasStackVars = true
  if bestReg in {X19..X28}: b.ra.usedCallee.incl bestReg
  result = regLoc(bestReg, curSlot)

proc allocVarDecl(b: var Builder; n: var Cursor) =
  n.into:
    let pos = b.posOf(n)
    assert n.kind == SymbolDef
    let name = symName(n); inc n
    skip n                                   # pragmas
    let slot = slotOf(b.prog[], n); skip n  # type (resolves named types)
    if n.hasMore: skip n                     # value (analysed in pass 1)
    if slot.kind == AMem:
      # an aggregate (object/array/named type): a nifasm-managed `(s)` stack
      # var, addressed by name — arkham does not register-allocate it. (No
      # early `return` here: that would skip the `into` epilogue and desync.)
      b.record(pos, name, namedStackLoc(name, slot))
      b.ra.hasStackVars = true
    else:
      let props = b.an.vars.getOrDefault(name).props
      var loc = b.allocStorage(slot, props)
      if loc.kind == OnStack and AddrTaken notin props and
         slot.inRegClass and not slot.isFloat:
        loc = b.trySteal(name, slot, props, loc)  # hot var evicts a colder one
      if loc.kind == OnStack:
        # A spilled (or address-taken) scalar — integer/pointer (`(s) (i 64)`) or
        # float (`(s) (f N)`) — lives in a nifasm-managed slot addressed by name,
        # same frame as aggregates, so nifasm computes the offset. (Floats don't
        # participate in `trySteal` eviction yet; they spill directly here.)
        loc = namedStackLoc(name, slot)
        b.ra.hasStackVars = true
      b.record(pos, name, loc)
      b.scopeVars[^1].add name

proc walk(b: var Builder; n: var Cursor) =
  case n.stmtKind
  of ProcS, TypeS:
    skip n                                   # nested decls allocate separately
  of VarS, GvarS, TvarS, ConstS:
    allocVarDecl(b, n)
  of StmtsS, ScopeS:
    openScope(b)
    n.into:
      while n.hasMore: walk(b, n)
    closeScope(b)
  else:
    if n.kind == TagLit:
      n.into:
        while n.hasMore: walk(b, n)          # recurse (var decls may nest)
    else:
      inc n

proc allocParams(b: var Builder; params: var Cursor; hasCall: bool) =
  if params.kind != TagLit: return
  var intIdx = 0
  var fidx = 0
  params.into:
    while params.hasMore:
      params.into:
        let pos = b.posOf(params)
        assert params.kind == SymbolDef
        let name = symName(params); inc params
        skip params                          # pragmas
        let slot = slotOf(b.prog[], params); skip params  # type (resolves named)
        # Classify an aggregate param: ≤16B by-value (a `(s)` stack home filled
        # from its GPR(s)) vs >16B by-reference (a pointer, like a scalar).
        var aggrSmall = false
        var aggrByRef = false
        var aggrWords = 0
        if slot.kind == AMem:
          let sz = slot.size                  # filled by slotOf (named or inline)
          if sz >= 1 and sz <= 16: (aggrSmall = true; aggrWords = (sz + 7) div 8)
          else: aggrByRef = true
        if aggrSmall:
          # (No early `continue`/`return`: that skips the `into` epilogue.)
          b.record(pos, name, namedStackLoc(name, slot))
          b.ra.hasStackVars = true
          intIdx += aggrWords
        else:
          # `effSlot` is the in-register value: the scalar itself, or (by-ref) a
          # pointer to the aggregate copy.
          let effSlot = if aggrByRef: AsmSlot(kind: AUInt, size: 8, align: 8) else: slot
          let props = b.an.vars.getOrDefault(name).props
          var loc: Location
          if effSlot.isFloat:
            # A float parameter arrives in v{fidx}. In a leaf proc it stays
            # there; if the proc makes calls it moves to a callee-saved register
            # (v8–v15) so it survives them; if address-taken it spills to a slot.
            # Either way the incoming register is consumed, so `fidx` advances in
            # lockstep with emitParamMoves (the >8-float case is still TODO).
            if fidx < FloatArgRegs.len:
              if AddrTaken in props:
                loc = b.spill(effSlot)           # address taken → must be on the stack
              elif hasCall:
                let f = b.takeFReg(b.freeCalleeF, FloatCalleeSaved)
                if f != NoFReg:
                  b.ra.usedCalleeF.incl f
                  loc = fregLoc(f, effSlot)
                else:
                  loc = b.spill(effSlot)
              else:
                loc = fregLoc(FloatArgRegs[fidx], effSlot)
              inc fidx
            else:
              loc = b.spill(effSlot)             # >8 float args: stack-passed (TODO)
          elif not effSlot.inRegClass:
            loc = b.spill(effSlot)
          elif intIdx < IntArgRegs.len:
            let arg = IntArgRegs[intIdx]
            if AddrTaken in props and not aggrByRef:
              loc = b.spill(effSlot)           # address taken → must be on the stack
            elif hasCall or aggrByRef:
              # Live across a call (the incoming arg reg is volatile), or a by-ref
              # pointer that must survive repeated field loads in the body: give
              # it a callee-saved home so the prologue can `mov home, argReg`.
              let r = b.takeReg(b.freeCallee, IntCalleeSaved)
              if r != NoReg:
                b.ra.usedCallee.incl r
                loc = regLoc(r, effSlot)
              else:
                loc = b.spill(effSlot)
            else:
              loc = regLoc(arg, effSlot)       # leaf proc: stay in the arg reg
            inc intIdx
          else:
            # The 9th integer/pointer parameter onward arrives on the caller's
            # stack (AAPCS64). arkham gives it a callee-saved register home that
            # the prologue loads from the incoming arg slot before SP is lowered
            # for locals, so it survives the whole proc. Address-taken or
            # out-of-register stack params aren't supported yet.
            if AddrTaken in props:
              raiseAssert "arkham v1: address-taken >8th parameter"
            let r = b.takeReg(b.freeCallee, IntCalleeSaved)
            if r == NoReg:
              raiseAssert "arkham v1: out of callee-saved registers for >8th parameter"
            b.ra.usedCallee.incl r
            loc = regLoc(r, effSlot)
            inc intIdx
          if loc.kind == OnStack and slot.kind != AMem:
            # An address-taken scalar param (integer or float): a nifasm `(s)`
            # slot the prologue fills from the incoming arg register (int or SIMD;
            # see emitParamMoves).
            loc = namedStackLoc(name, effSlot)
            b.ra.hasStackVars = true
          b.record(pos, name, loc)

proc allocateProc*(buf: var TokenBuf; procDecl: Cursor; an: ProcAnalysis;
                   prog: var Program;
                   presealed: set[Reg] = {}): RegAlloc =
  ## Allocate storage for the params and locals of `procDecl`. `presealed`
  ## registers are reserved for the whole proc (never allocated/stolen). `prog`
  ## is taken by `var` because resolving a cross-module type may load a module.
  var b = Builder(buf: addr buf, an: addr an, prog: addr prog)
  b.ra.locs = newSeq[Location](buf.len)
  b.ra.symPos = initTable[string, int]()
  b.ra.sealed = presealed
  for r in IntTempRegs: b.freeVol.incl r
  for r in IntCalleeSaved: b.freeCallee.incl r
  for f in FloatTempRegs: b.freeVolF.incl f
  for f in FloatCalleeSaved: b.freeCalleeF.incl f
  var n = procDecl
  assert n.stmtKind == ProcS
  b.openScope()
  n.into:
    inc n                                    # name
    allocParams(b, n, an.hasCall)            # params
    skip n                                   # return type
    skip n                                   # pragmas
    walk(b, n)                               # body
  b.closeScope()
  result = ensureMove b.ra

# ── lookup API (used by codegen) ────────────────────────────────────────────

proc locationOfSym*(ra: RegAlloc; name: string): Location {.inline.} =
  ## Storage of a local/param by name; `Undef` if unknown.
  let p = ra.symPos.getOrDefault(name, -1)
  if p >= 0: ra.locs[p] else: dontCare

# ── sealing (driven by codegen during ABI call marshalling) ─────────────────
# Before codegen places an argument in `xN` (or `x8` for an indirect result),
# it seals that register so any scratch borrow or steal during the rest of the
# call setup cannot clobber the committed value; it unseals after the call.

proc seal*(ra: var RegAlloc; r: Reg) {.inline.} = ra.sealed.incl r
proc unseal*(ra: var RegAlloc; r: Reg) {.inline.} = ra.sealed.excl r
proc seal*(ra: var RegAlloc; regs: set[Reg]) {.inline.} = ra.sealed = ra.sealed + regs
proc unseal*(ra: var RegAlloc; regs: set[Reg]) {.inline.} = ra.sealed = ra.sealed - regs
proc isSealed*(ra: RegAlloc; r: Reg): bool {.inline.} = r in ra.sealed

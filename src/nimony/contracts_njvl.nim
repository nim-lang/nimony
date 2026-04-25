#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[
Contract analysis using NJVL (No-Jump Versioned Locations) IR.

Tries to prove or disprove `.requires` and `.ensures` annotations.
Uses the structured ite/loop constructs from NJVL instead of goto-based
control flow graphs.

The analysis is performed on NJVL IR which has:
- `(ite cond then else)` for branching
- `(loop pre cond body)` for loops
- `(store value dest)` for assignments
- `(v symId version)` for versioned variables
- `(join symId newV old1 old2)` for merge points

In order to not be too annoying in the case of a contract violation, the
compiler emits a warning (that can be suppressed or turned into an error).
]##

import std / [assertions, tables, hashes, sets, strutils, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / models / tags
import ".." / lib / symparser
import ".." / njvl / [njvl_model, vl]
import nimony_model, programs, decls, typenav, sembasics, reporters,
  renderer, typeprops, inferle, xints, builtintypes
import implications

type
  BorrowableCheck = enum
    IsBorrowable       ## simple path: symbols, dots, array access
    IsBorrowableFromConst
    IsBorrowableFromGlobal
    HasAddr            ## path contains explicit `addr` — unsafe escape hatch
    NotBorrowable      ## deref in middle of path or function call

  BorrowInfo = object
    borrower: SymId   ## variable holding the borrow; upon `(kill borrower)` the borrow ends
    mode: BorrowableCheck
    path: seq[SymId]  ## root :: field1 :: field2 :: ...
    info: PackedLineInfo
  
  AnumNarrowInfo = object
    # rules:
    # allowuse = candidates.len > 0 otherwise ambigious
    # ObjConstr => candidates = possibleCandidates
    # of branch => candidates = @[branch]
    # if branch => restrict candidates
    narrower: SymId      # variable symbol the narrowing is bound to (e.g. `obj.0`)
    discriminator: SymId # discriminator field of the case-object (e.g. `kind.0`)
    candidates: HashSet[SymId]
    # TODO: maybe adding possibleCandidates will cheaper
    info: PackedLineInfo

  NjvlContext = object
    facts: Facts           # From inferle.nim - tracks le/notnil facts
    typeCache: TypeCache
    directlyInitialized: seq[HashSet[SymId]]
    errors: TokenBuf
    procCanRaise: bool
    moduleSuffix: string
    nestedProcs: int
    knownCfVars: HashSet[SymId]
    inlineVars: Table[SymId, Cursor] # var -> to its init expression
    impls: Implications                # flow-sensitive init implications;
                                        # subsumes both "writesTo" and
                                        # "knownTrueCfVars" — a jtrue'd cfvar
                                        # is just `Always cfvar` in `impls`.
    nextCondId: int                    # counter for minting synthetic cond
                                        # syms for complex ite conditions
    falseCfvars: seq[SymId]            # cond syms (cfvars or synthetics)
                                        # currently known false (from
                                        # `(ite (not cf) ...)` nesting and
                                        # from leaving-path asymmetries)
    trueCfvars: seq[SymId]             # cond syms currently known true
                                        # (from `(ite cf ...)` nesting and
                                        # leaving-path asymmetries)
    resultSym: SymId                   # symId of the `result` local for the current proc, or NoSymId
    activeBorrows: seq[BorrowInfo]
    anumNarrows: HashSet[AnumNarrowInfo]
    verbose: bool                      # --verbose: dump NJ IR on init/contract
                                       # failures for easier debugging
    currentProcStart: Cursor           # cursor at the start of the proc whose
                                       # body we are currently analysing (used
                                       # for the --verbose dump)
    aliasOf: Table[SymId, (SymId, SymId)]   # map `let cs = obj.kind` to (narrower, discriminator)

proc hash(a: AnumNarrowInfo): Hash {.inline.} =
  result = hash(a.narrower) !& hash(a.discriminator)
  result = !$result

proc `==`(a, b: AnumNarrowInfo): bool {.inline.} =
  a.narrower == b.narrower and a.discriminator == b.discriminator

proc dumpCurrentProc(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  ## Dump the NJ IR of the proc currently under analysis to stderr. Used
  ## by `--verbose` so the user can see the lowered form that caused
  ## a contract/init failure. Gated on `c.verbose` — callers still invoke
  ## it unconditionally; this proc is the single decision point.
  if not c.verbose: return
  if cursorIsNil(c.currentProcStart): return
  stderr.writeLine "--- NJ IR (--verbose) for: " & msg
  stderr.writeLine "--- at " & infoToStr(info) & ":"
  stderr.writeLine toString(c.currentProcStart, false)
  stderr.writeLine "--- end NJ IR dump ---"

proc buildErr(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  dumpCurrentProc(c, info, msg)
  var hintedMsg = msg
  if not c.verbose:
    hintedMsg.add " [pass --verbose for the NJ IR]"
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.add strToken(pool.strings.getOrIncl(hintedMsg), info)

proc contractViolation(c: var NjvlContext; orig: Cursor; fact: LeXplusC; report: bool) =
  if report:
    echo "known facts in this context: "
    for i in 0 ..< c.facts.len:
      echo $c.facts[i]
    echo "canonical fact: ", $fact
  error "contract violation: ", orig

# Forward declarations
proc traverseStmt(c: var NjvlContext; n: var Cursor)
proc traverseExpr(c: var NjvlContext; pc: var Cursor)
proc analyseCall(c: var NjvlContext; n: var Cursor)

proc extractSymId(n: Cursor): SymId {.inline.} =
  var n = n
  if n.exprKind in {HaddrX, HderefX}: inc n

  if n.kind == Symbol:
    result = n.symId
  elif n.kind == ParLe and n.tagEnum == VTagId:
    result = n.firstSon.symId
  else:
    result = NoSymId

proc extractSymIdForStore(n: Cursor): SymId =
  # idea both (etupat result.0 +0) and (etupat result.0 +1) create
  # a full store to `result.0`.
  var n = n
  if n.njvlKind == EtupatV:
    inc n
  result = extractSymId(n)

proc skipSymbol(r: var Cursor): SymId {.inline.} =
  ## Consume a bare Symbol or (v sym version) node and return its SymId.
  ## Returns NoSymId (without advancing) if r is neither.
  var n = r
  while n.exprKind in {HconvX, ConvX, BaseobjX}:
    inc n
    skip n # type
  result = extractSymId(n)
  if result != NoSymId:
    skip r

# --- Borrow checking ---

proc extractBorrowPath(c: var NjvlContext; n: Cursor; result: var BorrowInfo; followInlineVars=true) =
  ## Extract a path (root :: field1 :: field2 :: ...) from an expression,
  ## expanding inline variables.
  if n.kind == ParLe:
    let ek = n.exprKind
    if ek in {DotX, DdotX}:
      if ek == DdotX and result.mode != HasAddr:
        result.mode = NotBorrowable
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
      skip r # skip object subtree
      if r.kind == Symbol:
        result.path.add r.symId
    elif ek == AddrX:
      result.mode = HasAddr
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek == DerefX:
      if result.mode != HasAddr:
        result.mode = NotBorrowable
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {HaddrX, HderefX}:
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {TupatX, ArratX, AtX, PatX}:
      # Array/tuple access: recurse into container, don't distinguish indices
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in ConvKinds:
      var r = n
      inc r
      skip r # type
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek == BaseobjX:
      var r = n
      inc r
      skip r # type
      skip r # intlit
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in CallKinds:
      # we borrow from the first argument of the call:
      var r = n
      inc r
      skip r # fn
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {AconstrX, SetconstrX, TupconstrX, OconstrX, NilX, TrueX, FalseX}:
      result.mode = IsBorrowableFromConst
    elif n.njvlKind == EtupatV:
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif n.njvlKind == VV:
      extractBorrowPath(c, n.firstSon, result, followInlineVars)
  elif n.kind in {IntLit, UIntLit, CharLit, FloatLit, StringLit}:
    result.mode = IsBorrowableFromConst
  elif n.kind == Symbol:
    let s = n.symId
    if (followInlineVars or getType(c.typeCache, n).typeKind in {MutT, OutT, LentT}) and s in c.inlineVars:
      extractBorrowPath(c, c.inlineVars.getOrQuit(s), result, followInlineVars)
    else:
      if result.mode != HasAddr:
        result.mode = IsBorrowable
        let res = tryLoadSym(s)
        if res.status == LacksNothing:
          let local = asLocal(res.decl)
          if local.kind in {GvarY, TvarY}:
            result.mode = IsBorrowableFromGlobal
      result.path.add s

proc extractPath(c: var NjvlContext; n: Cursor; followInlineVars=true): BorrowInfo =
  result = BorrowInfo(path: @[], mode: NotBorrowable, info: n.info)
  extractBorrowPath(c, n, result, followInlineVars)

proc `$`(b: BorrowInfo): string =
  result = "BorrowInfo(mode: " & $b.mode & ", path: "
  for i in 0 ..< b.path.len:
    result.add " :: " & pool.syms[b.path[i]]
  result.add ")"

proc pathsOverlap(a, b: BorrowInfo): bool =
  ## Two paths overlap if one is a prefix of the other (or they are equal).
  ## Disjoint siblings (e.g. a.b vs a.c) do not overlap.
  if a.path.len == 0 or b.path.len == 0: return false
  let minLen = min(a.path.len, b.path.len)
  for i in 0 ..< minLen:
    if a.path[i] != b.path[i]:
      return false
  result = true

proc checkBorrowConflict(c: var NjvlContext; mutPath: BorrowInfo; info: PackedLineInfo) =
  for b in c.activeBorrows:
    if pathsOverlap(mutPath, b):
      buildErr c, info, "'" & pool.syms[mutPath.path[0]] & "' is borrowed and cannot be mutated"
      return

proc endBorrow(c: var NjvlContext; sym: SymId) =
  var i = 0
  while i < c.activeBorrows.len:
    if c.activeBorrows[i].borrower == sym:
      # order of active borrows is irrelevant, so swap-delete is fine
      c.activeBorrows.del(i)
    else:
      inc i

proc conditionCond(c: var NjvlContext; n: Cursor): PolarizedSym =
  ## Classify an ite condition into a cond-sym with polarity.
  ## When the condition is a cfvar (``(v cf …)`` or ``(not cf)``) the
  ## cfvar itself is used as the cond sym, preserving transitive reasoning
  ## across chained cfvar implications. For anything else (complex
  ## expressions, tuple-pattern checks, …) a fresh synthetic sym is
  ## minted so `combine` can still lift branch-local `Always` facts via
  ## cond-polarity. Synthetic conds are never ``jtrue``'d, so they don't
  ## appear in `c.knownCfVars`; they interact with the analysis purely
  ## through the cond-polarity lift and the `c.falseCfvars` set.
  if n.exprKind == NotX:
    var r = n
    inc r
    let s = extractSymId(r)
    if s != NoSymId and s in c.knownCfVars:
      return PolarizedSym(sym: s, negated: true)
  let d = extractSymId(n)
  if d != NoSymId and d in c.knownCfVars:
    return PolarizedSym(sym: d, negated: false)
  inc c.nextCondId
  let synth = pool.syms.getOrIncl("´cond." & $c.nextCondId & "." & c.moduleSuffix)
  result = PolarizedSym(sym: synth, negated: false)

proc cfCondKnownValue(c: NjvlContext; n: Cursor): int =
  ## Returns +1 if the condition is a mflag known to be true,
  ## -1 if it is `(not cf)` where cf is known true, 0 otherwise.
  ## After vl.nim, cfvars appear as `(v sym N)` so we use extractSymId.
  ## A cfvar is known true ⟺ it is `Always` written in `c.impls`.
  let s = extractSymId(n)
  if s != NoSymId and c.impls.isAlwaysInit(s):
    result = 1
  elif n.exprKind == NotX:
    var inner = n
    inc inner
    let s2 = extractSymId(inner)
    if s2 != NoSymId and c.impls.isAlwaysInit(s2):
      result = -1
    else:
      result = 0
  else:
    result = 0

template getVarId(c: var NjvlContext; symId: SymId): VarId = VarId(symId)

# --- Fact extraction from conditions ---

proc rightHandSide(c: var NjvlContext; pc: var Cursor; fact: var LeXplusC): bool =
  result = false
  if pc.exprKind in {AddX, SubX}:
    inc pc
    skip pc # type
    let symId2 = skipSymbol(pc)
    if symId2 != NoSymId:
      fact.b = getVarId(c, symId2)
      if pc.kind == IntLit:
        fact.c = fact.c + createXint(pool.integers[pc.intId])
        result = true
        inc pc
      elif pc.kind == UIntLit:
        fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
        result = true
        inc pc
      else:
        traverseExpr c, pc
    else:
      traverseExpr c, pc
      traverseExpr c, pc
    skipParRi pc
  elif (let symId2 = skipSymbol(pc); symId2 != NoSymId):
    fact.b = getVarId(c, symId2)
    result = true
  elif pc.kind == IntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.integers[pc.intId])
    result = true
    inc pc
  elif pc.kind == UIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
    result = true
    inc pc
  elif pc.exprKind == NilX:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(0'i32)
    result = true
    skip pc
  else:
    traverseExpr c, pc

proc translateCond(c: var NjvlContext; pc: var Cursor; wasEquality: var bool): LeXplusC =
  var r = pc
  result = LeXplusC(a: InvalidVarId, b: VarId(0), c: createXint(0'i32))

  var negations = 0
  while r.exprKind == NotX:
    inc negations
    inc r

  let xk = r.exprKind
  if xk in {LeX, LtX}:
    inc r
    skip r # skip type
  elif xk == EqX:
    wasEquality = negations == 0  # negated equality is inequality, not equality
    inc r
    skip r # skip type
  else:
    # Check for a bare symbol/hderef/haddr (truthy ref check: `if x:` means `x != nil`)
    let sa = extractSymId(r)
    if sa != NoSymId:
      result = isNotNil(getVarId(c, sa))
      skip r
      while negations > 0:
        negateFact(result)
        dec negations
        skipParRi r
      pc = r
    else:
      traverseExpr c, pc
    return result

  if r.kind == IntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.uintegers[r.uintId])
    inc r
  elif (let sa = skipSymbol(r); sa != NoSymId):
    result.a = getVarId(c, sa)
  elif r.exprKind == NilX:
    result.a = VarId(0)
    skip r
  else:
    traverseExpr c, pc
    return result
  if r.exprKind == NilX:
    wasEquality = false
  if not rightHandSide(c, r, result):
    result.a = InvalidVarId
  # a < b  --> a <= b - 1:
  if xk == LtX:
    result.c = result.c - createXint(1'i32)
  skipParRi r

  while negations > 0:
    negateFact(result)
    dec negations
    skipParRi r

  pc = r

proc analyseCondition(c: var NjvlContext; pc: var Cursor): int =
  ## Returns number of facts added
  var wasEquality = false
  let fact = translateCond(c, pc, wasEquality)
  if fact.isValid:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
      result = 2
    else:
      result = 1
  else:
    result = 0

# --- Not-nil checking ---

proc markedAs(t: Cursor; mark: NimonyOther): bool =
  result = false
  case t.typeKind
  of PtrT, RefT:
    var e = t.firstSon
    skip e # base type
    if e.kind != ParRi and e.substructureKind == mark:
      result = true
  of CstringT, PointerT:
    let e = t.firstSon
    # no base type
    if e.kind != ParRi and e.substructureKind == mark:
      result = true
  of ProctypeT:
    # New layout: `(proctype <NilTag> (params) RetType <Pragmas>)`. The
    # nilability marker is at slot 0.
    let e = t.firstSon
    if e.substructureKind == mark:
      result = true
  else:
    discard

proc analysableRoot(c: var NjvlContext; n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, ArratX, HderefX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    of BaseobjX:
      inc n
      skip n # type part
      skip n # skip intlit
    else:
      break
  let s = extractSymId(n)
  if s != NoSymId:
    result = s
    let x = getLocalInfo(c.typeCache, result)
    if x.kind == GvarY:
      # assume sharing of global variables between threads
      result = NoSymId
  else:
    result = NoSymId

proc isNonNilExpr(c: var NjvlContext; n: Cursor): bool =
  ## Check if an expression is trivially non-nil without needing dataflow analysis.
  case n.exprKind
  of AddrX:
    result = true
  of ConvKinds:
    # e.g. cstring("abc") — a conversion from a non-nil value is non-nil
    var inner = n
    inc inner
    skip inner # skip type part
    result = isNonNilExpr(c, inner)
  of SufX:
    # suffixed literal, e.g. (suf "abc" "R") — still a literal value
    result = true
  else:
    if n.kind == StringLit:
      result = true
    else:
      let s = extractSymId(n)
      if s != NoSymId:
        let sk = fetchSymKind(c.typeCache, s)
        result = isRoutine(sk)
      else:
        result = false

proc wantNotNil(c: var NjvlContext; n: Cursor) =
  case n.exprKind
  of NilX:
    buildErr(c, n.info, "expected non-nil value")
  of AddrX:
    discard "fine, addresses are not nil"
  else:
    let t = getType(c.typeCache, n)
    if markedAs(t, NotnilU):
      discard "fine, per type we know it is not nil"
    elif isNonNilExpr(c, n):
      discard "fine, expression is trivially not nil"
    elif t.typeKind in RoutineTypes and not markedAs(t, NilU):
      discard "fine, proc values are not nil unless explicitly marked nil"
    else:
      let r = analysableRoot(c, n)
      if r == NoSymId:
        # account for the fact that NJ already introduced tuples for the error handling:
        var n = n
        if n.exprKind == TupconstrX:
          inc n
          skip n # skip type
          if n.kind == Symbol and pool.syms[n.symId] == ("Success.0." & SystemModuleSuffix):
            inc n
        if n.exprKind == NewobjX and c.procCanRaise:
          discard "fine, nil value is mapped to OOM by the compiler"
        else:
          buildErr c, n.info, "cannot analyze expression is not nil: " & asNimCode(n)
      else:
        let fact = inferle.isNotNil(VarId r)
        if implies(c.facts, fact):
          discard "fine, did prove access correct"
        else:
          buildErr c, n.info, "cannot prove expression is not nil: " & asNimCode(n)

proc checkNilMatch(c: var NjvlContext; n: Cursor; expected: Cursor) =
  if markedAs(expected, NotnilU):
    wantNotNil c, n

proc wantNotNilDeref(c: var NjvlContext; n: Cursor) =
  let e = getType(c.typeCache, n)
  if markedAs(e, NilU):
    wantNotNil c, n

# --- .requires checking ---

type
  ProofRes = enum
    Unprovable, Disproven, Proven

proc `and`(a, b: ProofRes): ProofRes =
  if a == Proven and b == Proven:
    Proven
  elif a == Disproven or b == Disproven:
    Disproven
  else:
    Unprovable

proc `or`(a, b: ProofRes): ProofRes =
  if a == Proven or b == Proven:
    Proven
  elif a == Disproven and b == Disproven:
    Disproven
  else:
    Unprovable

proc `not`(a: ProofRes): ProofRes =
  if a == Unprovable:
    Unprovable
  elif a == Proven:
    Disproven
  else:
    Proven

proc argAt(call: Cursor; pos: int): Cursor =
  result = call
  inc result
  for i in 0 ..< pos: skip result

proc mapSymbol(c: var NjvlContext; paramMap: Table[SymId, int]; call: Cursor; symId: SymId): VarId =
  result = VarId(0)
  let pos = paramMap.getOrDefault(symId)
  if pos > 0:
    let arg = call.argAt(pos)
    let sid = extractSymId(arg)
    if sid != NoSymId:
      result = getVarId(c, sid)

proc compileCmp(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): LeXplusC =
  var r = req
  var a = InvalidVarId
  var b = InvalidVarId
  var cnst = createXint(0'i32)
  let sid = extractSymId(r)
  if sid != NoSymId:
    a = mapSymbol(c, paramMap, call, sid)
    inc r
  let rid = extractSymId(r)
  if rid != NoSymId:
    b = mapSymbol(c, paramMap, call, rid)
    inc r
  elif r.kind == IntLit:
    b = VarId(0)
    cnst = createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    b = VarId(0)
    cnst = createXint(pool.uintegers[r.uintId])
    inc r
  elif (let op = r.exprKind; op in {AddX, SubX}):
    inc r
    skip r # type
    let cid = extractSymId(r)
    if cid != NoSymId:
      b = mapSymbol(c, paramMap, call, cid)
      inc r
      if r.kind == IntLit:
        cnst = createXint(pool.integers[r.intId])
      elif r.kind == UIntLit:
        cnst = createXint(pool.uintegers[r.uintId])
      else:
        error "expected integer literal but got: ", r
    else:
      error "expected symbol but got: ", r
    skipParRi r
  result = query(a, b, cnst)

proc checkReq(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
  case req.exprKind
  of AndX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a and b
  of OrX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a or b
  of NotX:
    var r = req
    inc r
    result = not checkReq(c, paramMap, r, call)
  of EqX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    let cm2 = cm.geXplusC
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm) and implies(c.facts, cm2):
      result = Proven
    else:
      result = Disproven
  of LeX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm):
      result = Proven
    else:
      result = Disproven
  of LtX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm.ltXplusC):
      result = Proven
    else:
      result = Disproven
  of ExprX:
    var r = req
    while r.exprKind == ExprX:
      inc r
      while r.kind != ParRi and not isLastSon(r): skip r
    result = checkReq(c, paramMap, r, call)
  else:
    result = Unprovable

# --- Expression analysis ---

proc buildAnumInfos(objType: Cursor; narrower: SymId;
                    info: PackedLineInfo): seq[AnumNarrowInfo] =
  result = @[]
  var t = objType
  while t.kind == Symbol:
    let r = tryLoadSym(t.symId)
    if r.status != LacksNothing: return
    t = asTypeDecl(r.decl).body
  if t.typeKind != ObjectT: return
  inc t
  skip t
  while t.kind != ParRi:
    if t.substructureKind == CaseU:
      inc t
      var entry = AnumNarrowInfo(narrower: narrower, info: info)
      if t.substructureKind == FldU:
        entry.discriminator = asLocal(t).name.symId
        skip t
      while t.substructureKind == OfU:
        inc t
        if t.substructureKind == RangesU:
          inc t
          while t.kind != ParRi:
            if t.kind == Symbol: entry.candidates.incl t.symId
            skip t
          inc t
        skip t # (stmts ...)
        inc t # )
      result.add entry
    skip t

proc extractEqNarrowing(c: NjvlContext; cond: Cursor): (SymId, SymId, SymId) =
  ## Process `(eq T <alias> Tag)`
  result = (NoSymId, NoSymId, NoSymId)
  if cond.kind != ParLe or cond.exprKind != EqX:
    return
  var r = cond
  inc r
  skip r
  let lhs = extractSymId(r)
  skip r
  let tag =
    if r.kind == Symbol: r.symId
    else: NoSymId
  if lhs == NoSymId or tag == NoSymId or lhs notin c.aliasOf:
    return
  let (narrower, discriminator) = c.aliasOf[lhs]
  result = (narrower, discriminator, tag)

proc narrowTo(c: var NjvlContext; narrower, discriminator: SymId; cands: HashSet[SymId]; info: PackedLineInfo) =
  # erase old narrow for narrower and discriminator
  c.anumNarrows.excl AnumNarrowInfo(narrower: narrower, discriminator: discriminator)
  c.anumNarrows.incl AnumNarrowInfo(narrower: narrower, discriminator: discriminator,
                                    candidates: cands, info: info)

proc candidatesOf(c: var NjvlContext; narrower, discriminator: SymId;
                  info: PackedLineInfo): HashSet[SymId] =
  # get candidate tags considering current narrowing info,
  # if narrowing info not recorded, we don't have narrowings i.e all candidates possible
  let key = AnumNarrowInfo(narrower: narrower, discriminator: discriminator)
  for e in c.anumNarrows:
    if discriminator == NoSymId:
      if e.narrower == narrower:
        return e.candidates
    elif e == key:
      return e.candidates
  let objType = lookupSymbol(c.typeCache, narrower)
  if not cursorIsNil(objType):
    for entry in buildAnumInfos(objType, narrower, info):
      if discriminator == NoSymId or entry.discriminator == discriminator:
        return entry.candidates
  result = initHashSet[SymId]()

proc findBranch(objType: Cursor; fld: SymId): SymId =
  ## Find tag with field, used for better error message
  # basicly modified copy of findBranchFields
  result = NoSymId
  var body = objType
  while body.kind == Symbol:
    let r = tryLoadSym(body.symId)
    if r.status != LacksNothing: return
    body = asTypeDecl(r.decl).body
  if body.typeKind != ObjectT: return
  var n = asObjectDecl(body).firstField
  while n.substructureKind == FldU:
    skip n
  if n.substructureKind != CaseU: return
  inc n
  skip n     # skip discriminator fld
  while n.kind != ParRi:
    if n.substructureKind == OfU:
      inc n
      var tag = NoSymId
      if n.substructureKind == RangesU:
        var scan = n
        inc scan
        if scan.kind == Symbol:
          tag = scan.symId
      skip n   # skip ranges
      if n.substructureKind == StmtsU:
        var s = n
        inc s
        while s.kind != ParRi:
          if s.substructureKind == FldU and asLocal(s).name.symId == fld:
            return tag
          skip s
      skip n   # skip stmts
      inc n    # close `of`
    else:
      skip n

proc checkSumtypeFieldAccess(c: var NjvlContext; obj: Cursor; fld: SymId; info: PackedLineInfo) =
  let narrower = extractSymId(obj)
  if narrower == NoSymId: return
  let objType = lookupSymbol(c.typeCache, narrower)
  if cursorIsNil(objType): return
  let requiredTag = findBranch(objType, fld)
  if requiredTag == NoSymId: return  # common field / discriminator / not case-object
  let candidates = candidatesOf(c, narrower, NoSymId, info)

  var tagsStr = ""
  for t in candidates:
    if tagsStr.len > 0: tagsStr.add ", "
    tagsStr.add pool.syms[t]

  let suffix = if candidates.len == 0: "" else: " (currently in {" & tagsStr & "})"

  if candidates.len == 1 and requiredTag in candidates:
    discard "ok"
  elif requiredTag notin candidates:
    buildErr c, info,
      "cannot access field `" & pool.syms[fld] & "` of `" &
      pool.syms[narrower] & "`, active branch is not `" &
      pool.syms[requiredTag] & "`" & suffix
  else:
    assert candidates.len > 1
    buildErr c, info,
      "cannot access field `" & pool.syms[fld] & "` of `" &
      pool.syms[narrower] & "`: discriminator is ambiguous: {" &
      tagsStr & "}"

proc analyseOconstr(c: var NjvlContext; n: var Cursor) =
  inc n
  let objType = n
  skip n # type
  while n.kind != ParRi:
    assert n.substructureKind == KvU
    inc n
    assert n.kind == Symbol
    let expected = lookupField(c.typeCache, objType, n.symId)
    assert not cursorIsNil(expected), "could not lookup type for " & pool.syms[n.symId]
    skip n # field name
    checkNilMatch c, n, expected
    skip n # value
    if n.kind != ParRi:
      # optional inheritance
      skip n
    skipParRi n
  skipParRi n

proc analyseArrayConstr(c: var NjvlContext; n: var Cursor) =
  inc n
  let expected = n.firstSon # element type of the array
  skip n # type
  while n.kind != ParRi:
    checkNilMatch c, n, expected
    skip n
  skipParRi n

proc analyseTupConstr(c: var NjvlContext; n: var Cursor) =
  inc n
  var expected = n.firstSon # type of the first field
  skip n # type
  while n.kind != ParRi:
    assert expected.kind != ParRi
    let fieldType = getTupleFieldType(expected)
    var val = n
    if val.substructureKind == KvU:
      inc val # skip kv tag
      skip val # skip field name
    checkNilMatch c, val, fieldType
    skip n
    skip expected # type of the next field
  skipParRi n

proc isDirectlyInitialized(c: var NjvlContext; symId: SymId): bool =
  for s in mitems c.directlyInitialized:
    if symId in s:
      return true
  return false

proc isEffectivelyInitialized(c: var NjvlContext; symId: SymId): bool =
  ## True if symId is known initialized (directly, always on every path via
  ## `c.impls`, or via cond-based implication when a cond is known true or
  ## known false at the current program point).
  if isDirectlyInitialized(c, symId): return true
  if c.impls.isAlwaysInit(symId): return true
  for cf in c.falseCfvars:
    if c.impls.isInitIfCondFalse(symId, cf): return true
  for cf in c.trueCfvars:
    if c.impls.isInitIfCondTrue(symId, cf): return true
  return false

proc traverseExpr(c: var NjvlContext; pc: var Cursor) =
  var nested = 0
  while true:
    case pc.kind
    of Symbol:
      let symId = pc.symId
      let x = getLocalInfo(c.typeCache, symId)
      if x.kind in {VarY, LetY, CursorY, PatternvarY, ResultY}:
        if not isEffectivelyInitialized(c, symId):
          buildErr(c, pc.info, "cannot prove that " & pool.syms[symId] & " has been initialized")
          # don't report the same symbol twice from later references
          c.impls.add always(symId)
      inc pc
    of SymbolDef:
      bug "symbol definition in expression"
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit, UnknownToken:
      inc pc
    of ParRi:
      assert nested > 0
      dec nested
      inc pc
    of ParLe:
      case pc.exprKind
      of CallKinds:
        analyseCall c, pc
      of DotX:
        inc pc
        let obj = pc
        let info = pc.info
        traverseExpr c, pc # object
        if pc.kind == Symbol:
          checkSumtypeFieldAccess(c, obj, pc.symId, info)
        skip pc # field name
        if pc.kind != ParRi: skip pc # inheritance depth
        if pc.kind != ParRi: skip pc # optional access-token string lit
        skipParRi pc
      of DdotX:
        inc pc
        wantNotNilDeref c, pc
        let obj = pc
        let info = pc.info
        traverseExpr c, pc # object
        if pc.kind == Symbol:
          checkSumtypeFieldAccess(c, obj, pc.symId, info)
        skip pc # field name
        if pc.kind != ParRi: skip pc # inheritance depth
        if pc.kind != ParRi: skip pc # optional access-token string lit
        skipParRi pc
      of DerefX:
        inc pc
        wantNotNilDeref c, pc
        traverseExpr c, pc
        skipParRi pc
      of OconstrX, NewobjX:
        analyseOconstr c, pc
      of AconstrX:
        analyseArrayConstr c, pc
      of TupconstrX:
        analyseTupConstr c, pc
      of CastX, ConvX, HconvX:
        inc pc
        skip pc # skips type
        traverseExpr c, pc
        skipParRi pc
      else:
        inc nested
        inc pc
    if nested == 0: break

proc borrowCheckForCall(c: var NjvlContext; args: Cursor) =
  var mutPaths: seq[BorrowInfo] = @[]
  var immPaths: seq[BorrowInfo] = @[]
  var n = args
  while n.kind != ParRi:
    let isMut = n.exprKind == HaddrX
    # Validate borrowable path for haddr arguments (call-scoped borrows)
    var inner = n
    if isMut:
      inner = n.firstSon
      let m = extractPath(c, inner)
      if m.mode == NotBorrowable:
        buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
          "': path is not borrowable; use 'addr' to override or a temporary move"
      else:
        mutPaths.add m
    else:
      let m = extractPath(c, n, followInlineVars = false)
      if m.mode in {IsBorrowable, IsBorrowableFromGlobal}:
        immPaths.add m

    skip n
  # Check aliasing: a mutable argument must not overlap with any other argument:
  for i in 0 ..< mutPaths.len:
    for j in 0 ..< immPaths.len:
      if pathsOverlap(mutPaths[i], immPaths[j]):
        when false:
          echo "mutPaths[i]: ", mutPaths[i]
          echo "immPaths[j]: ", immPaths[j]
        buildErr c, mutPaths[i].info, "mutable argument aliases with immutable parameter"
        break
  # Mutable argument must not overlap with any other mutable argument:
  for i in 0 ..< mutPaths.len:
    for j in 0 ..< mutPaths.len:
      if i != j and pathsOverlap(mutPaths[i], mutPaths[j]):
        when false:
          echo "mutPaths[i]: ", mutPaths[i]
          echo "mutPaths[j]: ", mutPaths[j]
        buildErr c, mutPaths[i].info, "mutable argument aliases with mutable parameter"
        break

proc analyseCallArgs(c: var NjvlContext; n: var Cursor) =
  let callCursor = n
  let tt = getType(c.typeCache, n)
  let calleeKind = tt.stmtKind
  var fnType = skipProcTypeToParams(tt)
  var fnPragmas = fnType
  skip fnPragmas # params
  skip fnPragmas # return type
  let effect = whichEffect(calleeKind, fnPragmas)
  traverseExpr c, n # the `fn` itself
  assert fnType.isParamsTag
  inc fnType
  var paramMap = initTable[SymId, int]()
  # Collect argument paths for aliasing check
  let args = n
  var needsBorrowCheck = false
  while n.kind != ParRi:
    if fnType.kind == ParRi:
      # All formal params consumed but args remain (e.g. varargs that were
      # consumed without a matching VarargsT param, or similar edge cases).
      # Traverse remaining args for their side effects.
      while n.kind != ParRi:
        traverseExpr c, n
      break
    let previousFormalParam = fnType
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
    let pk = param.typ.typeKind
    # Save arg info before traverseExpr advances n
    let isMut = n.exprKind == HaddrX
    # Validate borrowable path for haddr arguments (call-scoped borrows)
    if isMut:
      var inner = n
      inc inner # skip haddr tag
      needsBorrowCheck = true
    if pk == OutT:
      let s = extractSymId(n)
      if s != NoSymId:
        c.impls.add always(s)
    elif pk == VarargsT:
      fnType = previousFormalParam
    checkNilMatch c, n, param.typ
    traverseExpr c, n
  if needsBorrowCheck:
    borrowCheckForCall c, args
  while fnType.kind != ParRi: skip fnType
  inc fnType # skip ParRi
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    let res = checkReq(c, paramMap, req, callCursor)
    when isMainModule:
      if res != Proven:
        error "contract violation: ", req

proc analyseCall(c: var NjvlContext; n: var Cursor) =
  inc n # skip call instruction
  analyseCallArgs(c, n)
  skipParRi n

# --- Assignment fact tracking ---

proc addAsgnFact(c: var NjvlContext; fact: LeXplusC) =
  if fact.isValid:
    c.facts.add fact
    c.facts.add fact.geXplusC

proc cannotBeNil(c: var NjvlContext; n: Cursor): bool {.inline.} =
  let t = getType(c.typeCache, n)
  result = markedAs(t, NotnilU) or isNonNilExpr(c, n)

# --- NJVL-specific traversal ---

proc traverseStore(c: var NjvlContext; n: var Cursor) =
  ## Handle (store value dest) - note reversed order from asgn
  inc n # skip store tag

  # First analyze the value (source)
  let valueStart = n
  traverseExpr c, n

  # Check borrow conflicts for the destination
  let destMutPath = extractPath(c, n)
  if destMutPath.mode in {IsBorrowable, IsBorrowableFromGlobal}:
    checkBorrowConflict(c, destMutPath, n.info)

  # if narrower is touched by destMutPath
  # we lost narrow info since
  # we no longer know which branch is active.
  # it mean that obj = Bar() drop narrow info
  if destMutPath.path.len > 0:
    var stale: seq[AnumNarrowInfo] = @[]
    for e in c.anumNarrows:
      if e.narrower == destMutPath.path[0]:
        stale.add e
    for e in stale:
      c.anumNarrows.excl e

  # Now handle the destination (Symbol or NJVL versioned variable (v symId version))
  let destSymId = extractSymIdForStore(n)
  if destSymId != NoSymId:
    let symId = destSymId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if isDirectlyInitialized(c, symId) or c.impls.isAlwaysInit(symId):
        c.buildErr n.info, "invalid reassignment to `let` variable"

    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    c.impls.add always(symId)

    # Check for not-nil type match
    let expected = getType(c.typeCache, n)
    checkNilMatch c, valueStart, expected

    # Try to extract facts from the value
    var valueForFact = valueStart
    if rightHandSide(c, valueForFact, fact):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        invalidateFactsAbout(c.facts, fact.a)
        addAsgnFact c, fact
    else:
      invalidateFactsAbout(c.facts, fact.a)

    # Check if the rhs is known to be not nil
    if (valueStart.exprKind == NewobjX and c.procCanRaise) or cannotBeNil(c, valueStart):
      c.facts.add isNotNil(fact.a)
    else:
      # Also check: the destination type might have notnil (e.g. proctype)
      if markedAs(expected, NotnilU):
        # The nil-match check already passed, so the value IS non-nil
        c.facts.add isNotNil(fact.a)

    skip n
  else:
    traverseExpr c, n

  skipParRi n

proc traverseIte(c: var NjvlContext; n: var Cursor) =
  ## Handle (ite cond then else [join])
  inc n # skip ite/itec tag

  # Fast path: if the condition's truth value is known from mflag state,
  # only traverse the live branch and skip the dead one.
  let knownVal = cfCondKnownValue(c, n)
  if knownVal == 1:
    # condition is a mflag known to be true: only then-branch runs
    skip n  # skip condition
    traverseStmt c, n  # then branch
    skip n  # skip else
    if n.kind == ParLe and n.stmtKind == StmtsS: skip n  # skip join
    skipParRi n
    return
  elif knownVal == -1:
    # condition is (not cf) with cf known true: only else-branch runs
    skip n  # skip condition
    skip n  # skip then branch
    if n.kind != DotToken: traverseStmt c, n else: inc n  # else
    if n.kind == ParLe and n.stmtKind == StmtsS: skip n  # skip join
    skipParRi n
    return

  # Classify the condition into a cond sym + polarity. Cfvars reuse their
  # own sym; complex conditions get a freshly minted synthetic.
  let cond = conditionCond(c, n)

  # Analyze condition and extract facts.
  let savedFacts = save(c.facts)
  let savedBorrowsLen = c.activeBorrows.len
  let savedNarrows = c.anumNarrows
  let implsCp = c.impls.checkpoint()
  let condStart = n
  let condFacts = analyseCondition(c, n)

  # Copy condition facts for else-branch negation (only single fact can be negated)
  var condFactsList: seq[LeXplusC] = @[]
  if condFacts == 1:
    condFactsList.add c.facts[c.facts.len - 1]

  # Then branch: `(not cf)` makes cf known false inside; a direct `(v cf …)`
  # makes cf known true inside.
  if cond.sym != NoSymId:
    if cond.negated: c.falseCfvars.add cond.sym
    else:            c.trueCfvars.add cond.sym

  let (narrower, discriminator, tag) = extractEqNarrowing(c, condStart)
  if narrower != NoSymId:
    var s = toHashSet([tag])
    narrowTo(c, narrower, discriminator, s, condStart.info)
  traverseStmt c, n
  if cond.sym != NoSymId:
    if cond.negated: discard c.falseCfvars.pop()
    else:            discard c.trueCfvars.pop()
  let thenFacts = c.facts
  let thenImpls = c.impls.take(implsCp)
  c.activeBorrows.setLen(savedBorrowsLen)

  # Restore facts for else branch. Inside else, the cond's polarity flips.
  restore(c.facts, savedFacts)
  c.anumNarrows = savedNarrows
  for f in condFactsList:
    var negated = f
    negateFact(negated)
    c.facts.add negated

  if cond.sym != NoSymId:
    if cond.negated: c.trueCfvars.add cond.sym
    else:            c.falseCfvars.add cond.sym
  if narrower != NoSymId:
    var elseCandidates = candidatesOf(c, narrower, discriminator, condStart.info)
    elseCandidates.excl tag
    narrowTo(c, narrower, discriminator, elseCandidates, condStart.info)
  if n.kind == DotToken:
    inc n
  else:
    traverseStmt c, n
  if cond.sym != NoSymId:
    if cond.negated: discard c.trueCfvars.pop()
    else:            discard c.falseCfvars.pop()
  c.activeBorrows.setLen(savedBorrowsLen)
  c.anumNarrows = savedNarrows
  let elseImpls = c.impls.take(implsCp)

  # Merge branch implications into the outer scope. Pass the ambient
  # `falseCfvars` set so `combine` can promote branch-local `IfFalse cf s`
  # facts to `Always s` when `cf` is already known false in the outer scope.
  # `combine` returns any cond syms the leaving-path asymmetry proves are
  # known-false / -true in sequential code past this ite.
  c.facts = merge(thenFacts, 0, c.facts, false)
  var knownFalse = initHashSet[SymId]()
  for cf in c.falseCfvars: knownFalse.incl cf
  var nowKnownFalse, nowKnownTrue: seq[SymId] = @[]
  combine(c.impls, nowKnownFalse, nowKnownTrue, thenImpls, elseImpls, cond, c.knownCfVars, knownFalse)
  for cf in nowKnownFalse:
    if cf notin c.falseCfvars: c.falseCfvars.add cf
  for cf in nowKnownTrue:
    if cf notin c.trueCfvars: c.trueCfvars.add cf

  # Skip optional join information emitted by vl.nim.
  if n.kind == ParLe and n.stmtKind == StmtsS:
    skip n

  skipParRi n

proc traverseLoop(c: var NjvlContext; n: var Cursor) =
  ## Handle (loop pre cond body)
  inc n # skip loop tag

  # Pre-condition statements
  traverseStmt c, n

  # Analyze loop condition
  let savedBorrowsLen = c.activeBorrows.len
  let savedFacts = save(c.facts)
  var condCursor = n
  var wasEquality = false
  let condFact = translateCond(c, condCursor, wasEquality)
  skip n # skip condition expression

  # Add condition fact so body is analyzed knowing condition is true
  if condFact.isValid:
    c.facts.add condFact
    if wasEquality:
      c.facts.add condFact.geXplusC

  # Loop body: the loop may execute 0 times, so `Always` facts don't survive.
  # However, cfvars are monotonic (only `jtrue` writes them), so an
  # `IfTrue cf s` fact produced by any iteration stays valid outside the
  # loop — if `cf=true` after the loop, the iteration that set it also
  # wrote `s`. `IfFalse cf s` doesn't survive: `cf=false` after the loop is
  # consistent with 0 iterations, where nothing was written.
  let loopCp = c.impls.checkpoint()
  traverseStmt c, n
  let bodyImpls = c.impls.take(loopCp)
  for imp in bodyImpls:
    if imp.kind == IfTrue: c.impls.add imp

  # After loop, we know the condition is false (if we exited normally)
  c.activeBorrows.setLen(savedBorrowsLen)
  restore(c.facts, savedFacts)
  if condFact.isValid:
    var negated = condFact
    negateFact(negated)
    c.facts.add negated

  skipParRi n

proc traverseLocal(c: var NjvlContext; n: var Cursor) =
  let kind = n.symKind
  inc n
  let name = n.symId
  skip n # name
  skip n # export marker
  let skipInitCheck = hasPragma(n, NoinitP)
  let isInline = hasPragma(n, InlineP)
  skip n # pragmas
  c.typeCache.registerLocal(name, kind, n)
  let localType = n
  skip n # type
  if n.kind != DotToken or skipInitCheck:
    c.directlyInitialized[^1].incl name
  if kind == ResultY:
    c.resultSym = name
  if isInline:
    c.inlineVars[name] = n
  # Detect borrow: (haddr X) as init expression starts a borrow.
  # Validate that the path is borrowable (no deref in the middle, no calls).
  # Explicit `addr` in the path is an escape hatch ("unchecked").
  if n.kind == ParLe and n.exprKind == HaddrX:
    var inner = n
    inc inner # skip haddr tag
    var path = extractPath(c, inner)
    if path.mode in {IsBorrowable, IsBorrowableFromGlobal}:
      path.borrower = name
      c.activeBorrows.add path
    elif path.mode == NotBorrowable:
      buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
        "': path is not borrowable; use 'addr' to override or a temporary move"
  if n.kind != DotToken and localType.typeKind in {PtrT, RefT, CstringT, PointerT, ProctypeT}:
    checkNilMatch c, n, localType
  # Process `let cs = obj.kind`
  # It place for aliasOf and
  # another narrow point btw
  if n.kind == ParLe and n.exprKind == DotX:
    var d = n
    inc d
    let narrower = extractSymId(d)
    skip d
    if d.kind == Symbol and narrower != NoSymId:
      let objType = lookupSymbol(c.typeCache, narrower)
      if not cursorIsNil(objType):
        for entry in buildAnumInfos(objType, narrower, n.info):
          if entry.discriminator == d.symId:
            c.aliasOf[name] = (narrower, entry.discriminator)
            break
  traverseExpr c, n
  skipParRi n

proc traverseAssume(c: var NjvlContext; n: var Cursor) =
  inc n
  var wasEquality = false
  let fact = translateCond(c, n, wasEquality)
  if not fact.isValid:
    error "invalid assume: ", n
  else:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
  skipParRi n

proc traverseAssert(c: var NjvlContext; n: var Cursor) =
  let orig = n
  inc n
  var report = false
  var shouldError = false
  if n.pragmaKind == ReportP:
    report = true
    inc n
    skipParRi n
  if n.pragmaKind == ErrorP:
    shouldError = true
    inc n
    skipParRi n

  var wasEquality = false
  let fact = translateCond(c, n, wasEquality)
  if not fact.isValid:
    error "invalid assert: ", orig
  elif implies(c.facts, fact):
    if shouldError:
      contractViolation(c, orig, fact, report)
    elif wasEquality:
      if implies(c.facts, fact.geXplusC):
        if report: echo "OK ", $fact
      else:
        if shouldError:
          if report: echo "OK (could indeed not prove) ", $fact
        else:
          contractViolation(c, orig, fact, report)
    else:
      if report: echo "OK ", $fact
  else:
    if shouldError:
      if report: echo "OK (could indeed not prove) ", $fact
    else:
      contractViolation(c, orig, fact, report)
  skipParRi n

proc isInitializedAtProcEnd(c: var NjvlContext; symId: SymId): bool =
  ## At the natural proc exit every cfvar Nimony emitted is a leaving-path
  ## marker, so reaching here implies each such cfvar is still false
  ## (otherwise we would already be on a raise/return path). We extend the
  ## ambient `falseCfvars` with every `knownCfVars` entry for the duration
  ## of this query and run the normal sound init check.
  let savedLen = c.falseCfvars.len
  for cf in c.knownCfVars:
    if cf notin c.falseCfvars: c.falseCfvars.add cf
  result = isEffectivelyInitialized(c, symId)
  c.falseCfvars.setLen(savedLen)

proc traverseProc(c: var NjvlContext; n: var Cursor) =
  let decl = n
  c.facts = createFacts()
  c.directlyInitialized.add initHashSet[SymId]()
  c.procCanRaise = false
  let savedImplsScope = c.impls.pushScope()
  let oldFalseCfvars = move c.falseCfvars
  let oldTrueCfvars = move c.trueCfvars
  let oldKnownCfVars = move c.knownCfVars
  let oldResultSym = c.resultSym
  let oldInlineVars = move c.inlineVars
  let oldBorrows = move c.activeBorrows
  let oldProcStart = c.currentProcStart
  c.currentProcStart = decl
  let oldAliasOf = move c.aliasOf
  c.resultSym = NoSymId
  inc n
  let symId = n.symId
  var isGeneric = false
  var isExternProc = false
  var outParams: seq[SymId] = @[]
  for i in 0 ..< BodyPos:
    if i == ProcPragmasPos:
      c.procCanRaise = hasPragma(n, RaisesP)
      isExternProc = hasPragma(n, ImportcP) or hasPragma(n, ImportcppP)
    elif i == TypevarsPos:
      isGeneric = n.substructureKind == TypevarsU
    elif i == ParamsPos:
      if n.kind == ParLe:
        var p = n
        inc p
        while p.kind != ParRi:
          let r = takeLocal(p, SkipFinalParRi)
          c.typeCache.registerLocal(r.name.symId, ParamY, r.typ)
          if r.typ.typeKind == OutT and not hasPragma(r.pragmas, NoinitP):
            outParams.add r.name.symId
      c.typeCache.registerLocal(symId, ProcY, decl)
    skip n

  # Analyze body
  if not isGeneric:
    traverseStmt c, n
    let info = decl.info
    # Check result / out-param init at proc end. In-body writes guarded by
    # leaving-path cfvars still land in `c.impls` as conditional facts; if
    # combine folded complementary conditionals into `Always`, we're fine.
    # Skip importc/importcpp procs: they have no Nim body and satisfy the
    # contract at the C level.
    if not isExternProc:
      if c.resultSym != NoSymId and not isInitializedAtProcEnd(c, c.resultSym):
        buildErr c, info, "cannot prove that " & pool.syms[c.resultSym] & " has been initialized"
      for sym in outParams:
        if not isInitializedAtProcEnd(c, sym):
          buildErr c, info, "cannot prove that " & pool.syms[sym] & " has been initialized"
  else:
    skip n
  skipParRi n
  c.impls.popScope(savedImplsScope)
  c.falseCfvars = oldFalseCfvars
  c.trueCfvars = oldTrueCfvars
  c.knownCfVars = oldKnownCfVars
  c.resultSym = oldResultSym
  c.inlineVars = ensureMove oldInlineVars
  c.activeBorrows = ensureMove oldBorrows
  c.currentProcStart = oldProcStart
  c.aliasOf = ensureMove oldAliasOf
  discard c.directlyInitialized.pop()

proc traverseStmt(c: var NjvlContext; n: var Cursor) =
  case n.njvlKind
  of IteV, ItecV:
    traverseIte c, n
  of LoopV:
    traverseLoop c, n
  of StoreV:
    traverseStore c, n
  of AssumeV:
    traverseAssume c, n
  of AssertV:
    traverseAssert c, n
  of MflagV, VflagV:
    # Control flow variable declaration
    inc n
    c.knownCfVars.incl n.symId
    skip n # symdef
    skipParRi n
  of JtrueV:
    # (jtrue cf1 cf2 ...) - cfvars listed here are now known true on this path.
    # NJ emits jtrue after noreturn calls and leaving paths (return/break/raise).
    # The mflag information is used at join points to determine which branches are
    # leaving paths, enabling the writeSets implication mechanism.
    inc n
    while n.kind != ParRi:
      assert n.kind == Symbol
      c.impls.add always(n.symId)
      inc n
    inc n  # ParRi
  of KillV:
    # Variable going out of scope - end any active borrows
    inc n
    while n.kind != ParRi:
      let s = extractSymId(n)
      if s != NoSymId:
        endBorrow(c, s)
      skip n
    inc n # ParRi
  of UnknownV:
    # Unknown instruction - variable's contents become unknown after a call.
    # Check borrow conflicts: passing a borrowed path to a var param is a mutation.
    inc n
    let unknownPath = extractPath(c, n)
    if unknownPath.mode in {IsBorrowable, IsBorrowableFromGlobal}:
      checkBorrowConflict(c, unknownPath, n.info)
    skip n # the unknown location
    skipParRi n
  of ContinueV:
    # Continue in loop - skip
    skip n
  of VV:
    # Versioned variable reference - should not appear as statement
    skip n
  of EtupatV:
    traverseExpr c, n
  of NoVTag:
    case n.stmtKind
    of StmtsS, ScopeS, BlockS:
      inc n
      while n.kind != ParRi:
        traverseStmt c, n
      skipParRi n
    of LocalDecls:
      traverseLocal c, n
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS:
      # Nested routine - analyze and advance past it
      c.typeCache.openScope()
      inc c.nestedProcs
      traverseProc c, n
      dec c.nestedProcs
      c.typeCache.closeScope()
    of TemplateS, TypeS, CommentS, PragmasS, RetS:
      skip n
    of CallKindsS:
      analyseCall c, n
    of DiscardS, YldS:
      inc n
      traverseExpr c, n
      skipParRi n
    of EmitS, InclS, ExclS:
      skip n
    of PragmaxS:
      inc n
      skip n # pragmas
      while n.kind != ParRi:
        traverseStmt c, n
      skipParRi n
    of NoStmt:
      if n.exprKind in CallKinds:
        analyseCall c, n
      elif n.exprKind == PragmaxX:
        inc n
        skip n
        traverseStmt c, n
        skipParRi n
      elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX, TraceX}:
        inc n
        traverseExpr c, n
        while n.kind != ParRi:
          traverseExpr c, n
        skipParRi n
      else:
        traverseExpr c, n
    else:
      # Unknown statement - try to traverse children
      inc n
      var nested = 1
      while nested > 0:
        case n.kind
        of ParLe:
          inc nested
          inc n
        of ParRi:
          dec nested
          inc n
        else:
          inc n

proc traverseToplevel(c: var NjvlContext; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    inc n
    while n.kind != ParRi:
      traverseToplevel c, n
    skipParRi n
  of PragmaxS:
    inc n
    skip n
    traverseToplevel c, n
    skipParRi n
  of ProcS, FuncS, IteratorS, ConverterS, MethodS:
    inc c.nestedProcs
    traverseProc c, n
    dec c.nestedProcs
  of MacroS, TemplateS, TypeS, CommentS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS,
     ExportS,
     IncludeS, ImportS, FromimportS, ImportexceptS:
    skip n
  else:
    # Toplevel statements - analyze them
    traverseStmt c, n

proc analyzeContractsNjvl*(input: var TokenBuf; moduleSuffix: string; verbose = false): TokenBuf =
  ## Main entry point: converts input to NJVL and analyzes contracts.
  ## When `verbose` is true, every contract/init failure dumps the enclosing
  ## proc's NJ IR to stderr to aid debugging.
  var n = beginRead(input)

  # Convert to NJVL first
  var njvlBuf = toNjvl(n, moduleSuffix)
  endRead input

  # Now analyze the NJVL IR
  var c = NjvlContext(
    typeCache: createTypeCache(),
    moduleSuffix: moduleSuffix,
    directlyInitialized: @[initHashSet[SymId]()],
    impls: createImplications(),
    verbose: verbose
  )
  c.typeCache.openScope()

  var njvl = beginRead(njvlBuf)
  traverseToplevel c, njvl
  endRead njvlBuf

  c.typeCache.closeScope()
  result = ensureMove c.errors

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parseFromFile(infile)
    discard analyzeContractsNjvl(input, "main")

  main(paramStr(1))

#
#
#        Condition elimination (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## CSE for CONDITIONS: fold a condition whose truth is already established on
## every path that reaches it. The payoff is nested assertions — an inlined
## accessor re-materializes the same `c.p != nil and 0 < c.rem` guard and the
## same `kind == TagLit` check over and over, each dragging a ~200-token panic
## block along. The first guard's fall-through PROVES the condition; every
## later re-test folds to a literal and `branchprune` deletes its panic arm.
##
## ## Facts
##
## A fact is "this pure boolean expression is true/false here", keyed by a
## CANONICAL SERIALIZATION of the expression:
##
##   - local symbols are substituted by (the serialization of) their defining
##     expression, so the assert temps `\`x.0h379` and `\`x.0h386` — distinct
##     names for the same computation — produce the same key;
##   - the diamond xelim lowers `A and B` / `A or B` to —
##     `(if (elif C (asgn b E)) (else (asgn b (false))))` — serializes `b` as
##     `(and C E)` (dually `(or …)` for an else-true diamond);
##   - every non-substituted symbol is stamped with a VERSION, bumped on each
##     assignment to it, and every memory read (deref/pat/at, or any lvalue
##     rooted in an address-taken local) is stamped with a memory EPOCH,
##     bumped by calls and stores through pointers. Stale facts therefore
##     simply stop matching — no kill lists, no dataflow lattice;
##   - an expression containing a call is unkeyable.
##
## Facts are learned from the fall-through of a DIVERGING guard: after
## `(if (elif C B))` with no other branches and `B` ending in a call to a
## `.noreturn` proc (carried into Leng as `(attr "noreturn")` on the decl —
## see `lengcgen`), the surviving path has `C` false. Learned facts decompose:
## `(not X)` false ⇒ `X` true; `(and A B)` true ⇒ both true; `(or A B)` false
## ⇒ both false; `(eq …)`/`(neq …)` know each other as duals.
##
## ## Folding
##
## An `(elif COND …)` whose serialized COND (or its negation) is a known fact
## is rewritten to the literal `(true)`/`(false)`; a boolean `(asgn b E)`
## whose E is a known fact likewise gets a literal RHS. The pass only plants
## literals — the actual deletion of decided branches is `branchprune`'s job,
## which runs right after (see `optdriver`).
##
## ## Merges, loops, labels — kept deliberately dumb
##
## Facts learned inside an `if` branch stay in that branch (the sibling did
## not establish them); the code after a fully-falling-through `if` keeps the
## facts from before it (branch assignments bumped versions, which neutralizes
## anything they invalidated). A `(loop …)` body starts with no facts and
## contributes none (its assignments still bump versions); a `(lab …)` that is
## actually jumped to clears the fact set — but the splicer's residual unused
## labels, which sit between every inlined body, are transparent, which is
## what lets facts flow from one spliced assert cluster to the next.

import std / [assertions, tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # parse/serialize; re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags         # TagEnum ordinals
import ".." / nifmodules                     # MainModule: foreign decl lookup
import cse                                   # FunctionSummary(-Table), readSummaryPragma

when defined(condelimStats):
  import std / syncio
  var ceFolds*: int
  var ceGuardsSeen*: int
  var ceGuardsLearned*: int

const
  MaxKeyLen = 2048     ## substitution-expanded keys beyond this are unkeyable
  Unkeyable = ""       ## sentinel serialization: never stored, never matches

type
  CondElimShared* = object
    ## Callee classifications survive across the per-body runs of one module:
    ## deriving readonly-ness walks callee bodies transitively, and paying
    ## that per BODY multiplied the work by the proc count (measured: 400+
    ## re-derivations of the same callee on one sem module).
    noreturnCache*: Table[SymId, bool]
    roCache*: Table[SymId, bool]

  CondElimCtx = object
    m: ptr MainModule                 ## nil in self-tests
    summaries: ptr FunctionSummaryTable  ## own-module summaries (may be nil)
    forcedNoreturn: HashSet[string]   ## test hook: callee basenames
    forcedReadonly: HashSet[string]   ## test hook: callee basenames
    noreturnCache: Table[SymId, bool]
    roCache: Table[SymId, bool]       ## callee → readonly (summary-derived)
    addrTaken: HashSet[SymId]
    assignCount: Table[SymId, int]    ## assignments to each sym in this body
    labelUses: Table[SymId, int]
    version: Table[SymId, int]
    memEpoch: int
    defs: Table[SymId, string]        ## local -> serialized defining value
                                      ## (single-assignment locals only)
    lastAsgnKey: Table[SymId, string] ## local -> key of the value most recently
                                      ## assigned to it, whatever its assignment
                                      ## count; the diamond join reads it to
                                      ## compose `(and C E)` for a temp that is
                                      ## (by construction) assigned in both arms
    facts: Table[string, bool]
    assignedLog: seq[SymId]           ## every sym assigned during the walk; a
                                      ## region (if/loop) records its start and
                                      ## re-bumps the syms logged inside it at
                                      ## the join, so no branch-local `defs`
                                      ## entry survives a merge
    changed: bool

# ---- noreturn callees ------------------------------------------------------

proc extractBasename(s: string): string =
  result = s
  let dot = result.find('.')
  if dot >= 0: result.setLen dot

proc declHasNoreturnAttr(decl: Cursor): bool =
  ## `(proc :name PARAMS RET (pragmas … (attr "noreturn") …) BODY)`?
  var d = decl
  let pd = takeProcDecl(d)
  if pd.pragmas.kind != TagLit: return false
  result = false
  var p = pd.pragmas
  p.into:
    while p.hasMore:
      if p.kind == TagLit and p.pragmaKind == AttrP:
        var a = p
        inc a                              # into the attr: at the string
        if a.kind == StrLit and strVal(a) == "noreturn": result = true
      skip p

proc isNoreturnCallee(c: var CondElimCtx; s: SymId; name: string): bool =
  if c.noreturnCache.hasKey(s): return c.noreturnCache.getOrDefault(s)
  var r = false
  if extractBasename(name) in c.forcedNoreturn:
    r = true
  elif c.m != nil:
    let d = c.m[].getDeclOrNil(s)
    if d != nil and d.kind == ProcY:
      r = declHasNoreturnAttr(d.pos)
  c.noreturnCache[s] = r
  r

proc isReadOnlySummary(s: FunctionSummary): bool =
  if s.writesGlobal or s.callsUnknown: return false
  for p in s.params:
    if p.writes or p.slotWritten: return false
  true

const MaxRoDepth = 5

proc isReadonlyCallee(c: var CondElimCtx; s: SymId; name: string;
                      depth = 0): bool
proc divergesTail(c: var CondElimCtx; body: Cursor): bool

proc hasJmp(n: Cursor): bool =
  if n.kind != TagLit: return false
  if n.stmtKind == JmpS: return true
  result = false
  var it = n
  it.into:
    while it.hasMore:
      if hasJmp(it): result = true
      skip it

proc exprReadonly(c: var CondElimCtx; n: Cursor; locals: HashSet[SymId];
                  depth: int): bool =
  ## No calls except to readonly-on-return callees. Reads (including through
  ## pointers) are always fine — the epoch stamps them.
  case n.kind
  of TagLit:
    if n.exprKind == InstrC:
      return false                         # may be an atomic RMW: writes
    if n.exprKind == CallC or n.stmtKind == CallS:
      var callee = n
      inc callee
      if callee.kind != Symbol: return false
      if not c.isReadonlyCallee(callee.symId, symName(callee), depth + 1):
        return false
    result = true
    var it = n
    it.into:
      while it.hasMore:
        if not c.exprReadonly(it, locals, depth): result = false
        skip it
  else:
    result = true

proc storeIsLocal(lhs: Cursor; locals: HashSet[SymId]): bool =
  ## The store target stays within a body-local (dot/at chains over a local
  ## are fine; any deref/pat hop leaves the frame).
  var n = lhs
  while n.kind == TagLit:
    case n.exprKind
    of DotC, AtC:
      inc n
    of DerefC, PatC:
      return false
    else:
      return false
  n.kind == Symbol and n.symId in locals

proc stmtsReadonly(c: var CondElimCtx; list: var Cursor;
                   locals: var HashSet[SymId]; depth: int): bool

proc oneStmtReadonly(c: var CondElimCtx; n: var Cursor;
                     locals: var HashSet[SymId]; depth: int): bool =
  case n.kind
  of TagLit:
    case n.stmtKind
    of StmtsS, ScopeS:
      var ok = true
      n.into:
        while n.hasMore:
          if not c.oneStmtReadonly(n, locals, depth): ok = false
      return ok
    of VarS:
      var probe = n
      inc probe
      if probe.kind == SymbolDef: locals.incl probe.symId
      let r = c.exprReadonly(n, locals, depth)
      skip n
      return r
    of AsgnS, StoreS:
      var probe = n
      inc probe
      let lhsCur = probe
      skip probe
      var ok = storeIsLocal(lhsCur, locals)
      if ok and probe.hasMore:
        if not c.exprReadonly(probe, locals, depth): ok = false
      skip n
      return ok
    of CallS:
      var callee = n
      inc callee
      var ok = false
      if callee.kind == Symbol:
        if c.isNoreturnCallee(callee.symId, symName(callee)):
          ok = true                        # never returns: a returning run
        else:                              # cannot have executed it
          ok = c.isReadonlyCallee(callee.symId, symName(callee), depth + 1) and
               c.exprReadonly(n, locals, depth)
      skip n
      return ok
    of IfS, CaseS:
      var ok = true
      n.into:
        while n.hasMore:
          if n.kind == TagLit and n.substructureKind in {ElifU, ElseU, OfU}:
            # a diverging, jump-free arm never returns: exempt from purity
            var b = n
            var bodyCur = default(Cursor)
            b.into:
              if n.substructureKind in {ElifU, OfU} and b.hasMore: skip b
              bodyCur = b
              while b.hasMore: skip b
            if c.divergesTail(bodyCur) and not hasJmp(bodyCur):
              skip n
            else:
              var arm = n
              arm.into:
                if n.substructureKind in {ElifU, OfU} and arm.hasMore:
                  # the leading condition / (ranges …) is an EXPRESSION
                  if not c.exprReadonly(arm, locals, depth): ok = false
                  skip arm
                while arm.hasMore:
                  if not c.oneStmtReadonly(arm, locals, depth): ok = false
              skip n
          else:
            if not c.oneStmtReadonly(n, locals, depth): ok = false
      return ok
    of LoopS, WhileS:
      var ok = true
      n.into:
        while n.hasMore:
          if not c.oneStmtReadonly(n, locals, depth): ok = false
      return ok
    of JmpS, LabS:
      skip n
      return true
    of RetS:
      let r = c.exprReadonly(n, locals, depth)
      skip n
      return r
    else:
      # unknown statement kind: conservative
      skip n
      return false
  else:
    inc n
    return true

proc stmtsReadonly(c: var CondElimCtx; list: var Cursor;
                   locals: var HashSet[SymId]; depth: int): bool =
  c.oneStmtReadonly(list, locals, depth)

proc bodyReadonlyOnReturn(c: var CondElimCtx; decl: Cursor; depth: int): bool =
  ## Writes and impure calls confined to diverging (noreturn-tailed, jump-free)
  ## arms don't count: a RETURNING execution never entered them. This is
  ## exactly the accessor shape — pure reads plus assert arms that
  ## write-stderr-and-quit — which hexer's summaries flag as callsUnknown.
  var d = decl
  let pd = takeProcDecl(d)
  if pd.body.kind != TagLit: return false
  # importc etc.: the body is a placeholder, judge only by summary
  if pd.pragmas.kind == TagLit:
    var pr = pd.pragmas
    var isImport = false
    pr.into:
      while pr.hasMore:
        if pr.kind == TagLit and pr.pragmaKind in {ImportcP, ImportcppP}:
          isImport = true
        skip pr
    if isImport: return false
  var locals = initHashSet[SymId]()
  if pd.params.kind == TagLit:
    var p = pd.params
    p.into:
      while p.hasMore:
        if p.kind == TagLit and p.substructureKind == ParamU:
          var q = p
          inc q
          if q.kind == SymbolDef: locals.incl q.symId
        skip p
  var body = pd.body
  result = c.stmtsReadonly(body, locals, depth)

proc isReadonlyCallee(c: var CondElimCtx; s: SymId; name: string;
                      depth = 0): bool =
  ## Readonly on every RETURNING path — first per the `(smry …)` summary,
  ## then (because hexer's summaries mark every cross-module caller
  ## `callsUnknown`) inferred from the callee's own body, transitively with a
  ## depth cap. Such a call is a pure function of its arguments and the
  ## memory epoch, which makes it KEYABLE — the sem modules' assert operands
  ## are almost all calls to `kind`/`substructureKind`-style readers the
  ## inliner declined, so without this the pass is blind exactly where the
  ## nested assertions live.
  if c.roCache.hasKey(s): return c.roCache.getOrDefault(s)
  if depth > MaxRoDepth: return false      # not cached: a shallower query may
                                           # still succeed
  c.roCache[s] = false                     # cycle-breaker
  var r = false
  if extractBasename(name) in c.forcedReadonly:
    r = true
  elif c.m != nil:
    # Judged by the BODY alone. The `(smry …)` summaries cannot be trusted
    # here: an importc decl's placeholder body makes them vacuously clean
    # (cWriteErr — the write(2) wrapper — carried a spotless summary), and
    # they are blind to `(instr …)`-level writes (atomic RMWs).
    let d = c.m[].getDeclOrNil(s)
    if d != nil and d.kind == ProcY:
      r = c.bodyReadonlyOnReturn(d.pos, depth)
  c.roCache[s] = r
  r

proc divergesTail(c: var CondElimCtx; body: Cursor): bool =
  ## Does the branch body END in a call to a noreturn proc? Peels trailing
  ## stmts/scope wrappers and looks at the last statement.
  var last = default(Cursor)
  var found = false
  var n = body
  if n.kind != TagLit: return false
  if n.stmtKind notin {StmtsS, ScopeS}:
    last = n; found = true
  else:
    while true:
      var inner = n
      var lst = default(Cursor)
      var any = false
      inner.into:
        while inner.hasMore:
          lst = inner
          any = true
          skip inner
      if not any: return false
      if lst.kind == TagLit and lst.stmtKind in {StmtsS, ScopeS}:
        n = lst
      else:
        last = lst; found = true
        break
  if not found or last.kind != TagLit: return false
  if last.stmtKind != CallS: return false
  var cc = last
  inc cc                                   # into the call: at the callee
  if cc.kind != Symbol: return false
  result = c.isNoreturnCallee(cc.symId, symName(cc))

# ---- canonical serialization ----------------------------------------------

proc verOf(c: var CondElimCtx; s: SymId): int =
  c.version.getOrDefault(s, 0)

proc ser(c: var CondElimCtx; n: Cursor; depth: int): string

proc serChildren(c: var CondElimCtx; n: Cursor; depth: int; res: var string): bool =
  var it = n
  var ok = true
  it.into:
    while it.hasMore:
      let s = c.ser(it, depth)
      if s == Unkeyable: ok = false
      res.add ' '
      res.add s
      skip it
  ok

proc ser(c: var CondElimCtx; n: Cursor; depth: int): string =
  ## Canonical value-serialization of a pure expression; `Unkeyable` when the
  ## expression contains a call or grows past `MaxKeyLen`.
  if depth > 24: return Unkeyable
  case n.kind
  of Symbol:
    let s = n.symId
    let nm = symName(n)
    if s in c.addrTaken:
      # An ADDRESS-TAKEN local is memory: epoch-stamped, and NEVER substituted
      # by its recorded definition — a callee holding `&x` may have stored
      # through it, so the initializer no longer describes the value. (Checking
      # `defs` first made a `(lt 0 f.rem)` guard fold to `(true)` across a
      # `sigmatchLoop(m, &f, …)` call that advances `f`.)
      return nm & "@@m" & $c.memEpoch
    if c.defs.hasKey(s):
      return c.defs.getOrDefault(s)
    return nm & "@" & $c.verOf(s)
  of IntLit: return $intVal(n)
  of UIntLit: return $uintVal(n) & "u"
  of FloatLit: return "f" & $cast[uint64](floatVal(n))   # exact bits as key
  of CharLit: return "c" & $int(charLit(n))
  of StrLit: return "s" & strVal(n)
  of DotToken: return "."
  of TagLit:
    case n.exprKind
    of CallC:
      # A READONLY call is a pure function of its arguments plus the memory
      # epoch: keyable. Anything else is opaque.
      var callee = n
      inc callee
      if callee.kind != Symbol: return Unkeyable
      if not c.isReadonlyCallee(callee.symId, symName(callee)):
        return Unkeyable
      var r = "(c " & symName(callee) & "@m" & $c.memEpoch
      var it = n
      var ok = true
      it.into:
        skip it                            # past the callee
        while it.hasMore:
          let s = c.ser(it, depth + 1)
          if s == Unkeyable: ok = false
          r.add ' '
          r.add s
          skip it
      if not ok: return Unkeyable
      r.add ')'
      if r.len > MaxKeyLen: return Unkeyable
      return r
    of InstrC, ErrvC, OvfC:
      return Unkeyable                     # impure / stateful
    of DerefC, PatC, AtC:
      var r = "(" & $int(cursorTagId(n)) & "@m" & $c.memEpoch
      if not c.serChildren(n, depth + 1, r): return Unkeyable
      r.add ')'
      if r.len > MaxKeyLen: return Unkeyable
      return r
    else:
      var r = "(" & $int(cursorTagId(n))
      if not c.serChildren(n, depth + 1, r): return Unkeyable
      r.add ')'
      if r.len > MaxKeyLen: return Unkeyable
      return r
  else:
    return Unkeyable

proc serTop(c: var CondElimCtx; n: Cursor): string = c.ser(n, 0)

# ---- fact bookkeeping ------------------------------------------------------

proc tagOfKey(key: string): int =
  ## The leading tag ordinal of a composite key, or -1.
  if key.len < 2 or key[0] != '(': return -1
  var i = 1
  var v = 0
  var any = false
  while i < key.len and key[i] in {'0'..'9'}:
    v = v * 10 + (ord(key[i]) - ord('0')); inc i; any = true
  if any: v else: -1

proc splitBinKey(key: string): (string, string) =
  ## The two child keys of a binary composite `(N A B)`; ("","") on mismatch.
  # children are space-separated at depth 0
  var parts: seq[string] = @[]
  var depth = 0
  var start = -1
  for i in 0 ..< key.len:
    let ch = key[i]
    if ch == '(': inc depth
    elif ch == ')': dec depth
    elif ch == ' ' and depth == 1:
      if start >= 0: parts.add key[start ..< i]
      start = i + 1
  if start >= 0 and start < key.len: parts.add key[start ..< key.len - 1]
  if parts.len == 2: (parts[0], parts[1]) else: ("", "")

proc learn(c: var CondElimCtx; key: string; value: bool) =
  ## Record a fact and its decompositions.
  if key == Unkeyable or key.len == 0: return
  if c.facts.getOrDefault(key, not value) == value: return   # already known
  c.facts[key] = value
  let t = tagOfKey(key)
  if t == ord(NotTagId):
    let (a, b) = splitBinKey(key)
    discard b
    if a.len == 0:
      # unary composite: strip "(N " and ")"
      var inner = key
      let sp = inner.find(' ')
      if sp > 0: c.learn(inner[sp+1 ..< inner.len-1], not value)
    else:
      c.learn(a, not value)
  elif t == ord(AndTagId):
    if value:
      let (a, b) = splitBinKey(key)
      if a.len > 0: c.learn(a, true)
      if b.len > 0: c.learn(b, true)
  elif t == ord(OrTagId):
    if not value:
      let (a, b) = splitBinKey(key)
      if a.len > 0: c.learn(a, false)
      if b.len > 0: c.learn(b, false)
  elif t == ord(EqTagId) or t == ord(NeqTagId):
    let dualTag = if t == ord(EqTagId): ord(NeqTagId) else: ord(EqTagId)
    let sp = key.find(' ')
    if sp > 0:
      c.facts["(" & $dualTag & key[sp ..< key.len]] = not value

proc lookupFact(c: var CondElimCtx; key: string): int =
  ## 1 known true, 0 known false, -1 unknown.
  if key == Unkeyable or key.len == 0: return -1
  if c.facts.hasKey(key):
    return ord(c.facts.getOrDefault(key))
  # (not X) with X known
  if tagOfKey(key) == ord(NotTagId):
    let sp = key.find(' ')
    if sp > 0:
      let inner = key[sp+1 ..< key.len-1]
      if c.facts.hasKey(inner):
        return 1 - ord(c.facts.getOrDefault(inner))
  -1

# ---- mutation tracking -----------------------------------------------------


proc bumpSym(c: var CondElimCtx; s: SymId) =
  c.version[s] = c.verOf(s) + 1
  c.defs.del s
  c.assignedLog.add s

proc rejoin(c: var CondElimCtx; mark: int) =
  ## At a control-flow join: any sym assigned inside the region gets a fresh
  ## version and loses its recorded definition (it may or may not hold it).
  var seen = initHashSet[SymId]()
  for i in mark ..< c.assignedLog.len:
    seen.incl c.assignedLog[i]
  c.assignedLog.setLen mark
  for s in seen:
    c.version[s] = c.verOf(s) + 1
    c.defs.del s

proc bumpMem(c: var CondElimCtx) =
  inc c.memEpoch

proc scanImpureCalls(c: var CondElimCtx; n: var Cursor; found: var bool) =
  case n.kind
  of TagLit:
    if n.exprKind == CallC or n.stmtKind == CallS:
      var callee = n
      inc callee
      if callee.kind != Symbol or
         not c.isReadonlyCallee(callee.symId, symName(callee)):
        found = true
    n.into:
      while n.hasMore:
        c.scanImpureCalls(n, found)
  else:
    inc n

proc bumpForCalls(c: var CondElimCtx; n: Cursor) =
  ## One epoch bump when the subtree contains any call that may write —
  ## readonly callees (per summary) leave memory alone and don't bump, which
  ## is what lets facts over reader-call results survive the statements
  ## between two assert clusters.
  var probe = n
  var found = false
  c.scanImpureCalls(probe, found)
  if found: c.bumpMem()

proc lvalueRoot(n: Cursor): (SymId, bool) =
  ## The symbol at the base of an lvalue and whether the path stays
  ## non-memory (dot chains only; deref/pat/at make it a memory store).
  var c = n
  var mem = false
  while c.kind == TagLit:
    case c.exprKind
    of DotC:
      inc c                                # into the dot: at the base
    of DerefC, PatC, AtC:
      mem = true
      inc c
    else:
      return (SymId(0), true)
  if c.kind == Symbol: (c.symId, mem) else: (SymId(0), true)

# ---- prescan ---------------------------------------------------------------

proc prescan(c: var CondElimCtx; n: var Cursor) =
  case n.kind
  of TagLit:
    if n.exprKind in {AddrC, HaddrC}:
      var inner = n
      inc inner
      let (root, _) = lvalueRoot(inner)
      if root != SymId(0): c.addrTaken.incl root
    elif n.stmtKind == JmpS:
      var l = n
      inc l
      if l.kind == Symbol:
        c.labelUses.mgetOrPut(l.symId, 0) += 1
    elif n.stmtKind in {AsgnS, StoreS}:
      var l = n
      inc l
      let (root, _) = lvalueRoot(l)
      if root != SymId(0):
        c.assignCount.mgetOrPut(root, 0) += 1
    n.into:
      while n.hasMore:
        prescan(c, n)
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StrLit, DotToken:
    inc n
  else:
    inc n

# ---- the walk --------------------------------------------------------------

proc trStmt(c: var CondElimCtx; dest: var TokenBuf; n: var Cursor)

proc copyTree(dest: var TokenBuf; n: var Cursor) =
  dest.addSubtree n
  skip n

proc emitBoolLit(dest: var TokenBuf; value: bool; li: NifLineInfo) =
  dest.openTag TagId(ord(if value: TrueTagId else: FalseTagId))
  if li.isValid: dest.appendLineInfo li
  dest.closeTag()

proc detectDiamond(c: var CondElimCtx; ifCur: Cursor): (SymId, string) =
  ## `(if (elif C (stmts… (asgn b E))) (else (stmts… (asgn b LIT))))` —
  ## xelim's and/or lowering. Returns (b, serialized "(and C E)"/"(or C E)"),
  ## or (0, ""). Both tails must assign the SAME b; the else's value must be a
  ## bool literal (false ⇒ and, true ⇒ or).
  result = (SymId(0), "")
  var branches: seq[Cursor] = @[]
  var it = ifCur
  it.into:
    while it.hasMore:
      branches.add it
      skip it
  if branches.len != 2: return
  if branches[0].substructureKind != ElifU: return
  if branches[1].substructureKind != ElseU: return

  proc tailAsgn(body: Cursor): (SymId, Cursor, bool) =
    ## last statement of the (possibly stmts-wrapped) body, if an (asgn sym E)
    result = (SymId(0), default(Cursor), false)
    var n = body
    while n.kind == TagLit and n.stmtKind in {StmtsS, ScopeS}:
      var inner = n
      var lst = default(Cursor)
      var any = false
      inner.into:
        while inner.hasMore:
          lst = inner; any = true
          skip inner
      if not any: return
      n = lst
    if n.kind != TagLit or n.stmtKind != AsgnS: return
    var a = n
    inc a                                  # into the asgn: at the LHS
    if a.kind != Symbol: return
    let lhs = a.symId
    skip a
    result = (lhs, a, true)

  var e = branches[0]
  inc e                                    # into the elif: at COND
  skip e                                   # past COND: at the elif body
  let (b1, v1, ok1) = tailAsgn(e)
  discard v1
  var el = branches[1]
  inc el                                   # into the else: at the body
  let (b2, v2, ok2) = tailAsgn(el)
  if not ok1 or not ok2 or b1 != b2 or b1 == SymId(0): return
  let lit2 = v2.exprKind
  if lit2 notin {TrueC, FalseC}: return
  let tag = if lit2 == FalseC: ord(AndTagId) else: ord(OrTagId)
  result = (b1, $tag)

proc trIf(c: var CondElimCtx; dest: var TokenBuf; n: var Cursor) =
  # Pre-detect the and/or diamond so the joined value is known afterwards.
  let dia = c.detectDiamond(n)
  let mark = c.assignedLog.len

  # Emit the if, folding conditions and walking branch bodies with the branch
  # condition as a local fact. Each branch walks from the PRE-if memory epoch;
  # only branches that can FALL THROUGH contribute their epoch to the join —
  # a diverging panic arm's `quit` call must not poison the facts the guard
  # just established for the surviving path.
  let tag = n.cursorTagId
  let li = rawLineInfo(n)
  dest.openTag tag
  if li.isValid: dest.appendLineInfo li
  var soleElifKey = ""
  var firstElifKey = Unkeyable
  var diaVal = Unkeyable
  var nBranches = 0
  var soleElifBody = default(Cursor)
  let preEpoch = c.memEpoch
  var joinEpoch = preEpoch
  n.into:
    while n.hasMore:
      inc nBranches
      let sk = n.substructureKind
      if sk == ElifU:
        let btag = cursorTagId(n)
        let bli = rawLineInfo(n)
        dest.openTag btag
        if bli.isValid: dest.appendLineInfo bli
        var b = n
        b.into:
          let condKey = c.serTop(b)
          soleElifKey = condKey
          if nBranches == 1: firstElifKey = condKey
          let known = c.lookupFact(condKey)
          if known >= 0:
            emitBoolLit(dest, known == 1, rawLineInfo(b))
            c.changed = true
            when defined(condelimStats): inc ceFolds
            skip b
          else:
            # conditions are pure reads; copy as-is
            copyTree(dest, b)
          # branch body: facts + cond-true, discarded afterwards
          soleElifBody = b
          let bodyCur = b
          let saved = c.facts
          c.memEpoch = preEpoch
          if condKey != Unkeyable: c.learn(condKey, true)
          while b.hasMore:
            c.trStmt(dest, b)
          if nBranches == 1 and dia[0] != SymId(0):
            # the diamond temp's value AS SERIALIZED AT THE BRANCH TAIL — this
            # is what lets nested and/or diamonds compose (the inner diamond's
            # own (and ...) key was recorded while walking the arm)
            diaVal = c.lastAsgnKey.getOrDefault(dia[0], Unkeyable)
          c.facts = saved
          if not c.divergesTail(bodyCur) and c.memEpoch > joinEpoch:
            joinEpoch = c.memEpoch
        dest.closeTag()
        skip n
      elif sk == ElseU:
        let btag = cursorTagId(n)
        let bli = rawLineInfo(n)
        dest.openTag btag
        if bli.isValid: dest.appendLineInfo bli
        var b = n
        b.into:
          let bodyCur = b
          let saved = c.facts
          c.memEpoch = preEpoch
          if soleElifKey != Unkeyable and soleElifKey.len > 0 and nBranches == 2:
            c.learn(soleElifKey, false)
          while b.hasMore:
            c.trStmt(dest, b)
          c.facts = saved
          if not c.divergesTail(bodyCur) and c.memEpoch > joinEpoch:
            joinEpoch = c.memEpoch
        dest.closeTag()
        skip n
      else:
        c.trStmt(dest, n)
  dest.closeTag()
  c.memEpoch = joinEpoch

  # Join: whatever the branches assigned is no longer known.
  c.rejoin(mark)
  # Post-if learning:
  if nBranches == 1:
    when defined(condelimStats):
      if c.divergesTail(soleElifBody): inc ceGuardsSeen
  if nBranches == 1 and soleElifKey.len > 0 and soleElifKey != Unkeyable and
     c.divergesTail(soleElifBody):
    # `(if (elif C <noreturn>))`: the surviving path has C false.
    c.learn(soleElifKey, false)
    when defined(condelimStats): inc ceGuardsLearned
  if dia[0] != SymId(0) and firstElifKey != Unkeyable and diaVal != Unkeyable:
    # the diamond's joined temp now denotes (and/or COND VALUE)
    c.bumpSym dia[0]
    c.defs[dia[0]] = "(" & dia[1] & " " & firstElifKey & " " & diaVal & ")"

proc singleOfValue(c: var CondElimCtx; ranges: Cursor): string =
  ## The serialization of a single-scalar `(ranges V)`; "" otherwise.
  result = ""
  if ranges.kind != TagLit: return
  var count = 0
  var r = ranges
  r.into:
    while r.hasMore:
      inc count
      if count == 1 and r.kind in {IntLit, UIntLit, CharLit}:
        result = c.ser(r, 0)
      else:
        result = ""
      skip r
  if count != 1: result = ""

proc trCase(c: var CondElimCtx; dest: var TokenBuf; n: var Cursor) =
  ## `(case SEL (of RANGES BODY)… (else BODY)?)` — sem code lives inside case
  ## arms, and so do its re-materialized assertion clusters. Inside an
  ## `(of (ranges V) …)` arm the fact `SEL == V` holds, which is exactly what
  ## the arm's inlined `kind(x) == V` re-asserts test. Same epoch discipline
  ## as `trIf`: each arm walks from the pre-case epoch, only fall-through arms
  ## feed the join.
  let mark = c.assignedLog.len
  let tag = n.cursorTagId
  let li = rawLineInfo(n)
  dest.openTag tag
  if li.isValid: dest.appendLineInfo li
  let preEpoch = c.memEpoch
  var joinEpoch = preEpoch
  var selKey = Unkeyable
  var first = true
  n.into:
    while n.hasMore:
      if first:
        selKey = c.serTop(n)
        copyTree(dest, n)                  # the selector: a pure read
        first = false
      elif n.kind == TagLit and n.substructureKind in {OfU, ElseU}:
        let isOf = n.substructureKind == OfU
        let btag = cursorTagId(n)
        let bli = rawLineInfo(n)
        dest.openTag btag
        if bli.isValid: dest.appendLineInfo bli
        var b = n
        b.into:
          var armFact = Unkeyable
          if isOf:
            if b.hasMore:
              if selKey != Unkeyable:
                let v = c.singleOfValue(b)
                if v.len > 0:
                  armFact = "(" & $ord(EqTagId) & " " & selKey & " " & v & ")"
              copyTree(dest, b)            # the (ranges …)
          let bodyCur = b
          let saved = c.facts
          c.memEpoch = preEpoch
          if armFact != Unkeyable: c.learn(armFact, true)
          while b.hasMore:
            c.trStmt(dest, b)
          c.facts = saved
          if not c.divergesTail(bodyCur) and c.memEpoch > joinEpoch:
            joinEpoch = c.memEpoch
        dest.closeTag()
        skip n
      else:
        c.trStmt(dest, n)
  dest.closeTag()
  c.memEpoch = joinEpoch
  c.rejoin(mark)

proc trStmt(c: var CondElimCtx; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of TagLit:
    let sk = n.stmtKind
    case sk
    of IfS:
      c.trIf(dest, n)
    of CaseS:
      c.trCase(dest, n)
    of StmtsS, ScopeS:
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      n.into:
        while n.hasMore:
          c.trStmt(dest, n)
      dest.closeTag()
    of AsgnS, StoreS:
      # record the mutation, then maybe fold a known boolean RHS
      var probe = n
      inc probe                            # into: at the LHS
      let lhsCur = probe
      let (root, mem) = lvalueRoot(lhsCur)
      skip probe                           # past LHS: at the RHS
      let rhsCur = probe
      let isPlainSym = lhsCur.kind == Symbol
      # serialize the RHS BEFORE bumping the LHS (self-referential RHS reads
      # the OLD value)
      let rhsKey = c.serTop(rhsCur)
      let known = c.lookupFact(rhsKey)
      c.bumpForCalls(rhsCur)               # a non-readonly call in the RHS writes
      if mem or root == SymId(0):
        c.bumpMem()
      else:
        c.bumpSym root
      if isPlainSym and rhsKey != Unkeyable:
        c.lastAsgnKey[lhsCur.symId] = rhsKey
      if isPlainSym and rhsKey != Unkeyable and
         c.assignCount.getOrDefault(lhsCur.symId, 0) <= 1:
        # Always the FULL key, never the folded literal: the fact table knows
        # the key's value anyway, and a diamond composition needs the
        # structural form (the literal broke `(and C E)` matching). Only for a
        # sym assigned exactly ONCE in the body — see the `VarS` note; a
        # diamond's joined temp is assigned in both arms and gets its def from
        # `trIf` at the join instead.
        c.defs[lhsCur.symId] = rhsKey
      if known >= 0 and rhsCur.kind == TagLit and
         rhsCur.exprKind notin {TrueC, FalseC}:
        # rewrite the RHS to its literal
        let tag = n.cursorTagId
        let li = rawLineInfo(n)
        dest.openTag tag
        if li.isValid: dest.appendLineInfo li
        var a = n
        a.into:
          copyTree(dest, a)                # LHS verbatim
          emitBoolLit(dest, known == 1, rawLineInfo(a))
          skip a
          while a.hasMore: copyTree(dest, a)
        dest.closeTag()
        skip n
        c.changed = true
      else:
        copyTree(dest, n)
    of CallS:
      c.bumpForCalls(n)
      copyTree(dest, n)
    of VarS, GvarS, TvarS, ConstS:
      # (var :name pragmas type INIT?) — record def when INIT is present
      var probe = n
      inc probe
      if probe.kind == SymbolDef:
        let s = probe.symId
        inc probe                          # past name
        skip probe                         # past pragmas
        skip probe                         # past type
        c.bumpSym s
        if probe.kind != DotToken:
          c.bumpForCalls(probe)            # impure initializer call writes
          if c.assignCount.getOrDefault(s, 0) == 0:
            # SINGLE-ASSIGNMENT ONLY. Substituting a MUTABLE local by its
            # initializer gives every local initialized to the same value one
            # shared key, so a fact about one leaks onto another: two `SymId`
            # locals both starting at 0 made `lenSym == 0` false (learned in an
            # else-branch) delete an unrelated `if dataSym == 0` lazy-init.
            let k = c.serTop(probe)
            if k != Unkeyable: c.defs[s] = k
      copyTree(dest, n)
    of LoopS, WhileS:
      # back-edge join: no facts in, none out; body assignments are unknown
      # after the loop (rejoin), and an iteration may store/call (epoch bump)
      let saved = c.facts
      c.facts = initTable[string, bool]()
      let mark = c.assignedLog.len
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      n.into:
        while n.hasMore:
          c.trStmt(dest, n)
      dest.closeTag()
      c.facts = saved
      c.rejoin(mark)
      c.bumpMem()                          # a loop iteration may store/call
    of LabS:
      var l = n
      inc l
      if l.kind == SymbolDef and c.labelUses.getOrDefault(l.symId, 0) > 0:
        c.facts.clear()                    # a real join point
      copyTree(dest, n)
    of ProcS:
      # nested proc decl (if any): opaque
      copyTree(dest, n)
    else:
      # any other statement: walk generically, treating it as opaque for
      # facts if it can store (retr etc. cannot; keep it simple: copy and,
      # when it contains calls anywhere, bump the epoch)
      c.bumpForCalls(n)
      copyTree(dest, n)
  else:
    copyTree(dest, n)

proc initCondElimShared*(): CondElimShared =
  CondElimShared(noreturnCache: initTable[SymId, bool](),
                 roCache: initTable[SymId, bool]())

proc runCondElim*(buf: var TokenBuf; m: ptr MainModule = nil;
                  summaries: ptr FunctionSummaryTable = nil;
                  shared: ptr CondElimShared = nil;
                  forcedNoreturn: openArray[string] = [];
                  forcedReadonly: openArray[string] = []): bool =
  ## Fold re-tested conditions in `buf` (a proc BODY). Returns true (and
  ## replaces `buf`) when something folded; `branchprune` should run after to
  ## delete the decided branches. `forcedNoreturn` is a test hook naming
  ## callee basenames to treat as noreturn when no module context is given.
  var c = CondElimCtx(m: m, summaries: summaries,
                      forcedNoreturn: initHashSet[string](),
                      forcedReadonly: initHashSet[string](),
                      noreturnCache: initTable[SymId, bool](),
                      roCache: initTable[SymId, bool](),
                      addrTaken: initHashSet[SymId](),
                      assignCount: initTable[SymId, int](),
                      labelUses: initTable[SymId, int](),
                      version: initTable[SymId, int](),
                      defs: initTable[SymId, string](),
                      lastAsgnKey: initTable[SymId, string](),
                      facts: initTable[string, bool]())
  for f in forcedNoreturn: c.forcedNoreturn.incl f
  for f in forcedReadonly: c.forcedReadonly.incl f
  if shared != nil:
    c.noreturnCache = move shared.noreturnCache
    c.roCache = move shared.roCache
  block:
    var n = buf.beginRead()
    while n.hasMore:
      prescan(c, n)
  var dest = createTokenBuf(buf.len, buf.pool, buf.tags)
  var n = buf.beginRead()
  while n.hasMore:
    c.trStmt(dest, n)
  result = c.changed
  if shared != nil:
    shared.noreturnCache = move c.noreturnCache
    shared.roCache = move c.roCache
  if c.changed:
    buf = ensureMove(dest)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc runOn(s: string): (string, bool) =
    var b = parseFromBuffer(s, "t", 100, sharedTags = createLengTagPool())
    let ch = runCondElim(b, forcedNoreturn = ["die"], forcedReadonly = ["rd"])
    (toString(b), ch)

  proc expectFold(input, expected: string) =
    let (got, ch) = runOn(input)
    var e = parseFromBuffer(expected, "t", 100, sharedTags = createLengTagPool())
    let want = toString(e)
    doAssert ch, "expected a fold for: " & input
    doAssert got == want, "fold MISMATCH\n  got:  " & got & "\n  want: " & want

  proc expectNoChange(input: string) =
    let (_, ch) = runOn(input)
    doAssert not ch, "unexpected fold for: " & input

  # the canonical assert pair: second guard folds to (false).
  expectFold(
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))))",
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(if (elif (false) (stmts (call die.0)))))")
  # an assignment to x between the guards blocks the fold.
  expectNoChange(
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(asgn x.0 1) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))))")
  # temps: two temps computed from the same expression share a key.
  expectFold(
    "(stmts (var :a.0 . (bool) (lt x.0 4)) " &
    "(if (elif (not a.0) (stmts (call die.0)))) " &
    "(var :b.0 . (bool) (lt x.0 4)) " &
    "(if (elif (not b.0) (stmts (call die.0)))))",
    "(stmts (var :a.0 . (bool) (lt x.0 4)) " &
    "(if (elif (not a.0) (stmts (call die.0)))) " &
    "(var :b.0 . (bool) (lt x.0 4)) " &
    "(if (elif (false) (stmts (call die.0)))))")
  # non-noreturn guard body: nothing learned.
  expectNoChange(
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call log.0)))) " &
    "(if (elif (not (eq x.0 0)) (stmts (call log.0)))))")
  # a call between guards kills MEMORY facts but not register facts.
  expectFold(
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(call log.0) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))))",
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(call log.0) " &
    "(if (elif (false) (stmts (call die.0)))))")
  expectNoChange(
    "(stmts (if (elif (not (eq (deref p.0) 0)) (stmts (call die.0)))) " &
    "(call log.0) " &
    "(if (elif (not (eq (deref p.0) 0)) (stmts (call die.0)))))")
  # memory facts DO flow when nothing intervenes.
  expectFold(
    "(stmts (if (elif (not (eq (deref p.0) 0)) (stmts (call die.0)))) " &
    "(if (elif (not (eq (deref p.0) 0)) (stmts (call die.0)))))",
    "(stmts (if (elif (not (eq (deref p.0) 0)) (stmts (call die.0)))) " &
    "(if (elif (false) (stmts (call die.0)))))")
  # the xelim and-diamond: guard 1 over the joined temp proves both arms, so
  # guard 2's re-materialized diamond condition folds and its own joined temp
  # is known — the second guard folds too.
  expectFold(
    "(stmts (var :a.0 . (bool).) " &
    "(if (elif (not (eq p.0 (nil))) (stmts (asgn a.0 (lt 0 y.0)))) (else (stmts (asgn a.0 (false))))) " &
    "(if (elif (not a.0) (stmts (call die.0)))) " &
    "(var :b.0 . (bool).) " &
    "(if (elif (not (eq p.0 (nil))) (stmts (asgn b.0 (lt 0 y.0)))) (else (stmts (asgn b.0 (false))))) " &
    "(if (elif (not b.0) (stmts (call die.0)))))",
    "(stmts (var :a.0 . (bool).) " &
    "(if (elif (not (eq p.0 (nil))) (stmts (asgn a.0 (lt 0 y.0)))) (else (stmts (asgn a.0 (false))))) " &
    "(if (elif (not a.0) (stmts (call die.0)))) " &
    "(var :b.0 . (bool).) " &
    # the re-materialized diamond folds twice over: its condition is a known
    # fact AND its then-arm value is one, so the arm assigns a literal.
    "(if (elif (true) (stmts (asgn b.0 (true)))) (else (stmts (asgn b.0 (false))))) " &
    "(if (elif (false) (stmts (call die.0)))))")
  # facts learned inside a branch do not leak past the if.
  expectNoChange(
    "(stmts (if (elif (eq x.0 0) (stmts (call log.0))) (else (stmts (call log.0)))) " &
    "(if (elif (eq x.0 0) (stmts (call log.0)))))")
  # a used label clears facts; an unused one is transparent.
  expectNoChange(
    "(stmts (jmp L.0) (lab :L.0) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))) (jmp L.1) (lab :L.1) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))))")
  expectFold(
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) (lab :L.2) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))))",
    "(stmts (if (elif (not (eq x.0 0)) (stmts (call die.0)))) (lab :L.2) " &
    "(if (elif (false) (stmts (call die.0)))))")
  # READONLY calls key like expressions: the sem shape `if kind(n) == X:` with
  # a repeated inner check on a fresh temp folds…
  expectFold(
    "(stmts (var :a.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (eq a.0 91) (stmts (var :b.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (not (eq b.0 91)) (stmts (call die.0))))))))",
    "(stmts (var :a.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (eq a.0 91) (stmts (var :b.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (false) (stmts (call die.0))))))))")
  # …but an impure call between the reads bumps the epoch and blocks it.
  expectNoChange(
    "(stmts (var :a.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (eq a.0 91) (stmts (call mut.0 n.0) (var :b.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (not (eq b.0 91)) (stmts (call die.0))))))))")
  # inside a single-value case arm the selector value is a fact: the arm's
  # re-materialized `kind == V` assert folds.
  expectFold(
    "(stmts (var :a.0 . (u 32) (call rd.0 n.0)) " &
    "(case a.0 (of (ranges 89) (stmts (var :b.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (not (eq b.0 89)) (stmts (call die.0)))))) (else (stmts (call log.0)))))",
    "(stmts (var :a.0 . (u 32) (call rd.0 n.0)) " &
    "(case a.0 (of (ranges 89) (stmts (var :b.0 . (u 32) (call rd.0 n.0)) " &
    "(if (elif (false) (stmts (call die.0)))))) (else (stmts (call log.0)))))")
  # a multi-value arm proves nothing, but its body is still walked (the inner
  # pair still folds against itself).
  expectFold(
    "(stmts (case a.0 (of (ranges 3 4) (stmts " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0))))))))",
    "(stmts (case a.0 (of (ranges 3 4) (stmts " &
    "(if (elif (not (eq x.0 0)) (stmts (call die.0)))) " &
    "(if (elif (false) (stmts (call die.0))))))))")
  # two MUTABLE locals initialized to the same value must not share a key: a
  # fact about one would decide the other (this deleted expreval's
  # `if dataSym == 0: dataSym = …` lazy-init because a sibling `lenSym`,
  # likewise starting at 0, was known non-zero in that else-branch).
  expectNoChange(
    "(stmts (var :p.0 . (u 32) (conv (u 32) 0)) (var :q.0 . (u 32) (conv (u 32) 0)) " &
    "(if (elif (eq p.0 0) (stmts (asgn p.0 7))) " &
    "(else (stmts (if (elif (eq q.0 0) (stmts (asgn q.0 9))))))))")
  # an ADDRESS-TAKEN local is memory: a call that receives its address may
  # advance it, so a guard over its field must NOT survive the call (this
  # miscompiled sigmatch: `f.rem > 0` folded to true across `sigmatchLoop(&f)`).
  expectNoChange(
    "(stmts (var :f.0 . T.0 (call mk.0)) " &
    "(if (elif (not (lt 0 (dot f.0 rem.0 0))) (stmts (call die.0)))) " &
    "(call loop.0 (haddr f.0)) " &
    "(if (elif (lt 0 (dot f.0 rem.0 0)) (stmts (call log.0)))))")
  # nested diamonds compose: `a = (x < 4) and (y == 1)`; inside `if a:` both
  # component re-asserts fold.
  expectFold(
    "(stmts (var :a.0 . (bool).) " &
    "(if (elif (lt x.0 4) (stmts (asgn a.0 (eq y.0 1)))) (else (stmts (asgn a.0 (false))))) " &
    "(if (elif a.0 (stmts " &
    "(if (elif (not (lt x.0 4)) (stmts (call die.0)))) " &
    "(if (elif (not (eq y.0 1)) (stmts (call die.0))))))))",
    "(stmts (var :a.0 . (bool).) " &
    "(if (elif (lt x.0 4) (stmts (asgn a.0 (eq y.0 1)))) (else (stmts (asgn a.0 (false))))) " &
    "(if (elif a.0 (stmts " &
    "(if (elif (false) (stmts (call die.0)))) " &
    "(if (elif (false) (stmts (call die.0))))))))")
  echo "condelim self-tests passed"

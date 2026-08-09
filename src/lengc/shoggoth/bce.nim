#
#
#           Lengc Bounds-Check Elimination
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Deletes an index check that a *dominating* identical check already made.
##
## `hexer`'s `desugar.trRequires` lowers every `.requires` contract to a guard
## at the top of the callee body:
##
## ```
## (if (elif (not (lt i (dot s len))) (stmts (call panic "…"))))
## ```
##
## and `seq.[]` is `.inline`, so after the inliner every element access carries
## its own copy of that guard — `t.hashes[h]` written three times in one loop
## iteration pays for three. Since `panic` does not return, **reaching the
## second guard proves the first one's condition was false**; if nothing since
## can have changed that condition, the second guard is dead code.
##
## That is the whole rule. It needs no arithmetic reasoning and no range
## analysis, which is why it is sound for free: the fact being propagated is
## literally "this exact expression was already tested".
##
## Measured motivation ([[destination_measured_bounds_checks_not_inlining]]):
## on the hot table-probe loop these guards are **44 % of arkham's executed
## instructions** against ~9 % for gcc — five times the combined prize of
## inlining and LICM. They are also what makes inlining lose: the same
## hand-inlined loop is +8.6 % with checks on and −16 % with them off.
##
## Invalidation is deliberately blunt: a write drops every fact mentioning the
## written root, and an indeterminate store or a call that can return drops
## everything. The hot loops this targets contain neither once the accessors are
## inlined, so precision here buys nothing.

import std / [tables, sets, assertions, os, syncio]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/pragmaKind, tag enums
import trackers, patchsets
import cse                                   # readSummaryPragma — the `noreturn` attribute
import ".." / nifmodules                     # MainModule: resolving the callee's summary

type
  Context = object
    orig: ptr TokenBuf
    m: ptr MainModule
    proven: Tracker[string, int]     ## canonical guard condition -> fact index
    mentions: seq[seq[SymId]]        ## fact index -> symbols the condition reads
    noReturnCache: Table[SymId, bool]
    defExpr: Table[SymId, int]       ## single-def pure local -> position of its RHS
    shortCircuit: Table[SymId, (int, int, bool)]
      ## bool temp -> (pos of A, pos of B, isAnd) for the `A and B` / `A or B`
      ## diamond hexer lowers a short-circuit operator to.
    defCount: Table[SymId, int]
    tainted: HashSet[SymId]          ## address taken, or defined by an impure RHS
    patchset: Patchset
    dotBuf: TokenBuf                 ## a single `.`: replaces a dead guard
    removed*: int
    dbgIfs, dbgShape, dbgNoRet, dbgSeen: int
    dbgR1, dbgR2, dbgR3, dbgR4, dbgR5: int

proc createContext(orig: ptr TokenBuf; m: ptr MainModule): Context =
  result = Context(orig: orig, m: m,
                   proven: initTracker[string, int](),
                   mentions: @[],
                   noReturnCache: initTable[SymId, bool](),
                   defExpr: initTable[SymId, int](),
                   shortCircuit: initTable[SymId, (int, int, bool)](),
                   defCount: initTable[SymId, int](),
                   tainted: initHashSet[SymId](),
                   patchset: initPatchset(orig),
                   dotBuf: createTokenBuf(2, orig[].pool, orig[].tags),
                   removed: 0)
  result.dotBuf.addDotToken()

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result


# ---- single-def resolution ------------------------------------------------

const MaxResolveDepth = 8
  ## A guard condition normalizes to the expression its symbols were *defined*
  ## from, so two inlined copies of one accessor — which bind `s.len` to their
  ## own fresh temp each — produce the same key. Depth-limited: the chains this
  ## has to see through are two or three links (`sroa` temp -> field load).

proc isPureExpr(cur: Cursor): bool =
  ## No call, no `addr`: a value that depends only on the memory its symbols
  ## name, so the mention set below fully describes when it can change.
  if not cur.hasMore: return true
  case cur.kind
  of TagLit:
    if cur.exprKind == CallC or cur.stmtKind == CallS: return false
    if cur.exprKind in AddrKinds: return false
    var n = cur
    var ok = true
    n.loopInto:
      if ok and not isPureExpr(n): ok = false
      skip n
    return ok
  else: return true


proc soleStmt(body: Cursor; res: var Cursor): bool =
  ## Descend through nested single-statement `(stmts …)` / `(scope …)` wrappers
  ## to the one statement they contain. hexer's scope lowering nests these
  ## several deep around a spliced expression.
  var cur = body
  var guard = 0
  while true:
    inc guard
    if guard > 32: return false
    if not cur.hasMore or cur.kind != TagLit: return false
    if cur.stmtKind notin {StmtsS, ScopeS}:
      res = cur
      return true
    var inner = cur
    var count = 0
    var only = default(Cursor)
    inner.loopInto:
      inc count
      only = inner
      skip inner
    if count != 1: return false
    cur = only

proc asgnTo(stmt: Cursor; target: var SymId; rhs: var Cursor): bool =
  if not stmt.hasMore or stmt.kind != TagLit or stmt.stmtKind != AsgnS: return false
  var lhs = child0(stmt)
  if lhs.kind != Symbol: return false
  target = symId(lhs)
  rhs = lhs
  skip rhs
  result = rhs.hasMore

proc matchShortCircuit(c: var Context; n: Cursor) =
  ## `x = A and B` becomes `(if (elif A (asgn x B)) (else (asgn x false)))`;
  ## `x = A or B` becomes `(if (elif A (asgn x true)) (else (asgn x B)))`.
  ## Without recognizing this, every `.requires` with a conjunction — which is
  ## every `seq[int]` index check — leaves its guard keyed on an opaque bool
  ## temp with two definitions, and no two copies ever compare equal.
  if n.stmtKind != IfS: return
  var arms = n
  var count = 0
  var condA = default(Cursor)
  var thenBody = default(Cursor)
  var elseBody = default(Cursor)
  var haveElse = false
  var bad = false
  arms.loopInto:
    case arms.substructureKind
    of ElifU:
      inc count
      var b = arms
      var i = 0
      b.loopInto:
        if i == 0: condA = b
        elif i == 1: thenBody = b
        inc i
        skip b
      if i != 2: bad = true
    of ElseU:
      haveElse = true
      var b = arms
      var i = 0
      b.loopInto:
        if i == 0: elseBody = b
        inc i
        skip b
      if i != 1: bad = true
    else: bad = true
    skip arms
  if bad or count != 1 or not haveElse: return
  var tStmt, eStmt: Cursor
  if not soleStmt(thenBody, tStmt): return
  if not soleStmt(elseBody, eStmt): return
  var tSym, eSym: SymId
  var tRhs, eRhs: Cursor
  if not asgnTo(tStmt, tSym, tRhs): return
  if not asgnTo(eStmt, eSym, eRhs): return
  if tSym != eSym or tSym == SymId(0): return
  if not isPureExpr(condA): return
  if eRhs.kind == TagLit and eRhs.exprKind == FalseC and isPureExpr(tRhs):
    c.shortCircuit[tSym] = (cursorToPosition(c.orig[], condA),
                            cursorToPosition(c.orig[], tRhs), true)
  elif tRhs.kind == TagLit and tRhs.exprKind == TrueC and isPureExpr(eRhs):
    c.shortCircuit[tSym] = (cursorToPosition(c.orig[], condA),
                            cursorToPosition(c.orig[], eRhs), false)

proc preScan(c: var Context; start: Cursor) =
  if not start.hasMore or start.kind != TagLit: return
  let sk = start.stmtKind
  if sk == VarS:
    var n = start
    var nameSym = SymId(0)
    var i = 0
    var rhs = default(Cursor)
    var have = false
    n.loopInto:
      if i == 0 and n.kind == SymbolDef: nameSym = symId(n)
      elif i == 3:
        rhs = n; have = true
      inc i
      skip n
    if nameSym != SymId(0):
      # `(var :x . T .)` is a DECLARATION, not a definition: the value arrives
      # in a later `(asgn x …)` — which hexer emits for every scope-lowered
      # temp, so counting the decl would make every such temp look
      # multiply-defined and block resolution entirely.
      if have and rhs.hasMore and rhs.kind != DotToken:
        c.defCount[nameSym] = c.defCount.getOrDefault(nameSym) + 1
        if isPureExpr(rhs):
          c.defExpr[nameSym] = cursorToPosition(c.orig[], rhs)
        else:
          c.tainted.incl nameSym
  elif sk == AsgnS:
    var lhs = child0(start)
    var rhs = lhs
    skip rhs
    let root = rootOf(lhs)
    if root != SymId(0):
      c.defCount[root] = c.defCount.getOrDefault(root) + 1
      if lhs.kind == Symbol and rhs.hasMore and isPureExpr(rhs):
        c.defExpr[root] = cursorToPosition(c.orig[], rhs)
      else:
        c.tainted.incl root
  elif sk == IfS:
    matchShortCircuit(c, start)
  elif start.exprKind in AddrKinds:
    let s = rootOf(child0(start))
    if s != SymId(0): c.tainted.incl s
  var n = start
  n.loopInto:
    preScan(c, n)
    skip n

proc resolvable(c: Context; s: SymId): bool {.inline.} =
  s notin c.tainted and c.defCount.getOrDefault(s) == 1 and c.defExpr.hasKey(s)

proc isDiamond(c: Context; s: SymId): bool {.inline.} =
  ## The two arms of the diamond are its only definitions.
  s notin c.tainted and c.shortCircuit.hasKey(s) and
    c.defCount.getOrDefault(s) == 2

# ---- expression identity --------------------------------------------------

proc keyOf(c: var Context; cur: Cursor; result: var string; depth = 0) =
  ## Structural key of an expression, line info excluded. Same shape as
  ## `cse.hashExpr`; duplicated rather than exported so the two passes' notions
  ## of "the same expression" can diverge without one silently changing the other.
  case cur.kind
  of Symbol, SymbolDef:
    let s = symId(cur)
    if depth < MaxResolveDepth and cur.kind == Symbol and c.isDiamond(s):
      let (pa, pb, isAnd) = c.shortCircuit[s]
      result.add (if isAnd: "(&" else: "(|")
      var a = cursorAt(c.orig[], pa)
      keyOf(c, a, result, depth + 1)
      result.add ' '
      var b = cursorAt(c.orig[], pb)
      keyOf(c, b, result, depth + 1)
      result.add ')'
    elif depth < MaxResolveDepth and cur.kind == Symbol and c.resolvable(s):
      let d = cursorAt(c.orig[], c.defExpr[s])
      keyOf(c, d, result, depth + 1)
    else:
      result.add 'S'; result.add symName(cur)
  of IntLit:   result.add 'I'; result.add $intVal(cur)
  of UIntLit:  result.add 'U'; result.add $uintVal(cur)
  of CharLit:  result.add 'C'; result.addInt int charLit(cur)
  of StrLit:   result.add '"'; result.add strVal(cur); result.add '"'
  of Ident:    result.add 'i'; result.add strVal(cur)
  of FloatLit: result.add 'F'  # a float in an index guard: never equal to itself here
  of TagLit:
    result.add '('
    result.addInt int cursorTagId(cur)
    var n = cur
    n.loopInto:
      result.add ' '
      keyOf(c, n, result, depth)
      skip n
    result.add ')'
  else:
    result.add '?'

proc keyOf(c: var Context; cur: Cursor): string =
  result = ""
  keyOf(c, cur, result)

proc collectSyms(c: var Context; cur: Cursor; acc: var seq[SymId]; depth = 0) =
  ## The RESOLVED mention set: a normalized key names the symbols its resolved
  ## form reads, so invalidation must track those, not the temps it saw through.
  if not cur.hasMore: return
  case cur.kind
  of Symbol:
    let s = symId(cur)
    if depth < MaxResolveDepth and c.isDiamond(s):
      if s notin acc: acc.add s
      let (pa, pb, _) = c.shortCircuit[s]
      var a = cursorAt(c.orig[], pa)
      collectSyms(c, a, acc, depth + 1)
      var b = cursorAt(c.orig[], pb)
      collectSyms(c, b, acc, depth + 1)
    elif depth < MaxResolveDepth and c.resolvable(s):
      if s notin acc: acc.add s
      let d = cursorAt(c.orig[], c.defExpr[s])
      collectSyms(c, d, acc, depth + 1)
    elif s notin acc: acc.add s
  of TagLit:
    var n = cur
    n.loopInto:
      collectSyms(c, n, acc, depth)
      skip n
  else: discard

# ---- the guard shape ------------------------------------------------------

proc calleeOf(call: Cursor): SymId =
  let f = child0(call)
  result = (if f.kind == Symbol: symId(f) else: SymId(0))

proc isNoReturn(c: var Context; fn: SymId): bool =
  ## Does `fn` carry `(attr "noreturn")` in its summary? Resolved through the
  ## same path `cse.callSummary` uses, and memoized (negatives too).
  if fn == SymId(0): return false
  let cached = c.noReturnCache.getOrDefault(fn, false)
  if fn in c.noReturnCache: return cached
  var res = false
  if c.m != nil and canLoadForeign(c.m[], fn):
    let d = getDeclOrNil(c.m[], fn)
    if d != nil and d.kind == ProcY:
      var probe = d.pos
      let pd = takeProcDecl(probe)
      var summary = default(FunctionSummary)
      if readSummaryPragma(pd.pragmas, summary):
        res = summary.noReturn
  c.noReturnCache[fn] = res
  result = res

proc guardCondition(c: var Context; n: Cursor; cond: var Cursor): bool =
  ## `n` is `(if (elif COND (stmts (call NORETURN …))))` with exactly one branch
  ## and no `else` — an assertion of `not COND`. Yields COND.
  if n.stmtKind != IfS: return false
  inc c.dbgIfs
  var arm = n
  var count = 0
  var body = default(Cursor)
  var got = default(Cursor)
  arm.loopInto:
    inc count
    if arm.substructureKind != ElifU:
      inc c.dbgR1
      return false
    var b = arm
    b.into:
      if not b.hasMore: return false
      got = b; skip b
      if not b.hasMore: return false
      body = b; skip b
      if b.hasMore:
        inc c.dbgR2
        return false
    skip arm
  if count != 1:
    inc c.dbgR1
    return false
  # the arm must be a `(stmts …)` whose only statement is a noreturn call
  if body.stmtKind != StmtsS:
    inc c.dbgR3
    return false
  var s = body
  var stmts = 0
  var theCall = default(Cursor)
  s.loopInto:
    inc stmts
    theCall = s
    skip s
  if stmts != 1:
    inc c.dbgR4
    return false
  if theCall.stmtKind != CallS:
    inc c.dbgR5
    return false
  inc c.dbgShape
  if not isNoReturn(c, calleeOf(theCall)):
    inc c.dbgNoRet
    return false
  inc c.dbgSeen
  cond = got
  result = true

# ---- fact invalidation ----------------------------------------------------

proc dropMentioning(c: var Context; target: SymId) =
  if target == SymId(0): return
  var toClear: seq[string] = @[]
  for key, idx in c.proven.pairs:
    if idx > 0 and target in c.mentions[idx-1]: toClear.add key
  for key in toClear: c.proven[key] = 0

proc dropAll(c: var Context) = c.proven.clearAll()

proc invalidateForStore(c: var Context; lhs: Cursor) =
  ## A write through a pointer or to an unrootable target could be anywhere.
  let root = rootOf(lhs)
  if root == SymId(0): dropAll(c)
  else: dropMentioning(c, root)

proc invalidateForCall(c: var Context; call: Cursor) =
  if not isNoReturn(c, calleeOf(call)): dropAll(c)

# ---- traversal ------------------------------------------------------------

proc openBranches(c: var Context) = c.proven.openBranches()
proc openBranch(c: var Context) = c.proven.openBranch()
proc openFinalBranch(c: var Context) = c.proven.openFinalBranch()
proc closeBranch(c: var Context) = c.proven.closeBranch()
proc closeBranches(c: var Context) = c.proven.closeBranches()
proc gotoLabel(c: var Context; L: LabelId) = c.proven.gotoLabel L
proc landLabel(c: var Context; L: LabelId) = c.proven.landLabel L

proc tr(c: var Context; n: var Cursor)   # forward

proc trGuard(c: var Context; n: var Cursor; cond: Cursor) =
  let key = keyOf(c, cond)
  if getEnv("NIM_BCE_DBG") == "2":
    var top = cond
    if top.kind == TagLit: inc top
    let ts = (if top.kind == Symbol: symId(top) else: SymId(0))
    stderr.writeLine "bce-key ", (if c.proven[key] > 0: "HIT  " else: "miss "), key,
                     "   [top=", (if ts != SymId(0): symName(top) else: "-"),
                     " defs=", c.defCount.getOrDefault(ts),
                     " tainted=", (ts in c.tainted),
                     " hasDef=", c.defExpr.hasKey(ts), "]"
  if c.proven[key] > 0:
    c.patchset.addSubst(cursorToPosition(c.orig[], n), cursorAt(c.dotBuf, 0))
    inc c.removed
    skip n
    return
  var syms: seq[SymId] = @[]
  collectSyms(c, cond, syms)
  c.mentions.add ensureMove(syms)
  c.proven[key] = c.mentions.len            # 1-based: 0 means "not proven"
  skip n

proc trIf(c: var Context; n: var Cursor) =
  openBranches c
  n.loopInto:
    case n.substructureKind
    of ElifU:
      n.into:
        if n.hasMore: skip n               # the condition reads only
        openBranch c
        if n.hasMore: tr(c, n)
        while n.hasMore: skip n
        closeBranch c
    of ElseU:
      n.into:
        openFinalBranch c
        if n.hasMore: tr(c, n)
        while n.hasMore: skip n
        closeBranch c
    else: skip n
  closeBranches c

proc collectClobbers(c: var Context; start: Cursor; writes: var HashSet[SymId];
                     total: var bool) =
  ## What a region can change: the roots it assigns, plus `total` when it stores
  ## through a pointer or performs a call that can return.
  if not start.hasMore or start.kind != TagLit: return
  let sk = start.stmtKind
  if sk in {AsgnS, StoreS}:
    var lhs = child0(start)
    if sk == StoreS: skip lhs
    let root = rootOf(lhs)
    if root == SymId(0): total = true
    else: writes.incl root
  elif sk == CallS:
    if not isNoReturn(c, calleeOf(start)): total = true
  elif start.exprKind == CallC:
    if not isNoReturn(c, calleeOf(start)): total = true
  var n = start
  n.loopInto:
    collectClobbers(c, n, writes, total)
    skip n

proc trLoopBody(c: var Context; n: var Cursor) =
  case n.stmtKind
  of WhileS:
    n.into:
      if n.hasMore: skip n                 # condition
      if n.hasMore: tr(c, n)
      while n.hasMore: skip n
  of LoopS:
    n.into:
      if n.hasMore: tr(c, n)
      if n.hasMore: skip n
      if n.hasMore: tr(c, n)
      if n.hasMore: tr(c, n)
      while n.hasMore: skip n
  else: skip n

proc trLoop(c: var Context; n: var Cursor) =
  ## Anything the body writes is written "before" the guard on the next
  ## iteration, so it must be invalidated before the body is walked at all.
  var writes = initHashSet[SymId]()
  var total = false
  collectClobbers(c, n, writes, total)
  openBranches c
  openBranch c
  closeBranch c
  openBranch c
  if total: dropAll(c)
  else:
    for s in writes: dropMentioning(c, s)
  trLoopBody(c, n)
  closeBranch c
  closeBranches c

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  if n.kind != TagLit:
    skip n
    return
  var cond = default(Cursor)
  if guardCondition(c, n, cond):
    trGuard(c, n, cond)
    return
  case n.stmtKind
  of IfS: trIf(c, n)
  of CaseS:
    n.into:
      if n.hasMore: skip n
      openBranches c
      while n.hasMore:
        case n.substructureKind
        of OfU:
          n.into:
            if n.hasMore: skip n
            openBranch c
            if n.hasMore: tr(c, n)
            while n.hasMore: skip n
            closeBranch c
        of ElseU:
          n.into:
            openFinalBranch c
            if n.hasMore: tr(c, n)
            while n.hasMore: skip n
            closeBranch c
        else: skip n
      closeBranches c
  of WhileS, LoopS: trLoop(c, n)
  of AsgnS, StoreS:
    let stmt = n
    var lhs = child0(stmt)
    if stmt.stmtKind == StoreS: skip lhs
    var inner = n
    inner.loopInto:
      tr(c, inner)                       # `tr` advances `inner` itself
    skip n
    invalidateForStore(c, lhs)
  of CallS:
    let call = n
    skip n
    invalidateForCall(c, call)
  of JmpS:
    let probe = child0(n)
    if probe.kind == Symbol: gotoLabel(c, LabelId(uint32(symId(probe))))
    skip n
  of LabS:
    let probe = child0(n)
    if probe.kind == SymbolDef: landLabel(c, LabelId(uint32(symId(probe))))
    skip n
  of RetS, BreakS:
    skip n
    dropAll c
  else:
    if n.exprKind == CallC:
      let call = n
      skip n
      invalidateForCall(c, call)
    else:
      var inner = n
      inner.loopInto:
        tr(c, inner)                     # `tr` advances `inner` itself
      skip n

proc runBCE*(buf: var TokenBuf; m: ptr MainModule): int =
  ## In-place redundant-index-check elimination for one proc body. Returns the
  ## number of guards deleted.
  var c = createContext(addr buf, m)
  block:
    let pn = beginRead(buf)
    preScan(c, pn)
  var n = beginRead(buf)
  tr(c, n)
  endRead(n)
  if not c.patchset.isEmpty:
    var newBuf = c.patchset.apply()
    buf = ensureMove(newBuf)
  if getEnv("NIM_BCE_DBG").len > 0 and c.dbgIfs > 0:
    stderr.writeLine "bce: ifs=", c.dbgIfs, " shape=", c.dbgShape,
                     " notNoReturn=", c.dbgNoRet, " guards=", c.dbgSeen,
                     " removed=", c.removed,
                     "  [r1=", c.dbgR1, " r2=", c.dbgR2, " r3=", c.dbgR3,
                     " r4=", c.dbgR4, " r5=", c.dbgR5, "]"
  result = c.removed

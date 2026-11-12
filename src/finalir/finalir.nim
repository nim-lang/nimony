#
#
#           Nimony Final IR Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Lowers Nimony IR to the **Final IR** described in `doc/final_ir.md`.
##
## This pass is the structured counterpart of `nj.nim`: where `nj.nim`
## *eliminates* jumps by materialising control-flow flags (`mflag`/`jtrue`)
## and exploiting else-branches, the Final IR *keeps* a small set of
## structured control-flow constructs and never introduces a single cfvar:
##
## - `loop`   — an **infinite** back-edge loop with no condition slot. Every
##              forward way out is a `(jmp loopExit)`; the body ends in
##              `(continue .)`, the sole back-edge.
## - `ite`    — if-then-else (expression `if`s were already lowered by xelim).
## - `case`   — kept as-is (`doc/final_ir.md`: "Format as existing").
## - `lab`/`jmp` — the one structured multi-exit. `jmp` is forward-only and
##              scoped, so a forward traversal sees every predecessor of a
##              `(lab L)` before reaching it (see *Analysis: exit summaries*).
## - `try`/`except`/`fin` — the cleanup/handler regions that survive lowering;
##              a `fin` cannot become a `jmp` (it must run on *every* exit).
## - `ret`/`raise` — primitive exits kept as-is: they carry a payload and cross
##              cleanup, so they stay named exits rather than plain transfers.
##
## The cfvar/guard/else-exploitation machinery of `nj.nim` is therefore gone:
## the multi-join at a `(lab)` encodes "did we already leave" positionally, so
## the control-flow flags are no longer needed.
##
## The traversal keeps using the **typenav** logic (`c.typeCache`): locals are
## registered as they are declared, scopes are opened/closed and emit `kill`
## instructions, and `getType` drives selector/typing decisions.

import std / [tables, sets, assertions, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import ".." / hexer / [xelim, mover, passes, lifter]
import ".." / lib / symparser
import finalir_model, condextractor, defroots, versions

type
  Exit = object
    ## A structured exit target on the lexical stack. A `loop` and a `block`
    ## each push one; `break` resolves to the matching entry and emits a
    ## forward `(jmp name)`. The `(lab name)` is emitted after the construct
    ## *only if* some `jmp` actually targeted it (`used`).
    name: SymId        # the `(lab name)` this exit lands on
    blockName: SymId   # source block name for `break label`, or NoSymId
    isLoop: bool       # loops are also the target of an unnamed `break`
    used: bool         # did any `jmp`/`break` target this exit?
    cont: SymId        # loops: the `(lab cont)` in front of the back-edge
    contUsed: bool     # did a source-level `continue` target it?
    scopeDepth: int    # `typeCache.scopeDepth` outside the construct; a jump
                       # out kills the locals of the scopes above it

  LoopJoin = object
    sym: SymId
    entry, header: int

  CurrentProc = object
    resultSym: SymId
    returnType: Cursor
    tmpCounter: int
    exits: seq[Exit]
    vers: Versions                       ## the versions of its variables
    labelEdges: Table[SymId, seq[Mark]]  ## where the `jmp`s to a label are
    live: bool                           ## can control fall through to here?
    pendingBarrier: bool                  ## a barrier whose `(unknown .)` is owed
    spared: seq[SymId]                    ## the shared variables it names

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentProc
    analysisFacts: bool ## emit `kill`/`unknown`; only the prover reads them
    lifter: ref LiftingCtx ## asks a type for its `=destroy`
    callExprs: Table[SymId, TokenBuf] ## whole init call of a local, so a `for`
                                      ## whose iterator xelim hoisted into a
                                      ## temp can still be read (see `trFor`)
    hoistedConsts: TokenBuf ## the consts of routines and blocks, moved to
                            ## module level; see `toFinalIr`

proc openScope(c: var Context) =
  c.typeCache.openScope()

const
  MemSym = SymId(0) ## stands for every location a barrier can write

proc newDef(c: var Context; s: SymId): int

proc isDestroyed(c: var Context; kind: SymKind; typ: Cursor): bool =
  ## Does a local of this kind and type die by a `=destroy` that is a barrier?
  kind in {VarY, LetY} and c.analysisFacts and destroyHasSideEffect(c.lifter[], typ)

proc diesByBarrier(c: var Context; s: SymId): bool =
  let li = getLocalInfo(c.typeCache, s)
  result = isDestroyed(c, li.kind, li.typ)

template emitKill(c: var Context; dest: var TokenBuf; info: NifLineInfo;
                  locals: untyped) =
  ## `(kill …)` for the symbols `locals` yields, if any. A scope yields its
  ## locals in declaration order, so the output is deterministic as it is.
  ##
  ## A local with a destructor dies by a call of its `=destroy`, which the
  ## tree does not show yet. Unless it is a `func`, it is a call like any
  ## other `proc` and can write what a `ref` in the local points to, or a
  ## global: `(unknown .)`, a barrier, in front of the `kill`.
  if c.analysisFacts:
    var destroys = false
    for s in locals:
      if diesByBarrier(c, s): destroys = true
    if destroys:
      dest.addParLe("unknown", info)
      dest.addDotToken()
      dest.addParRi()
      discard newDef(c, MemSym)
    var first = true
    for s in locals:
      if first:
        dest.addParLe("kill", info)
        first = false
      dest.addSymUse s, info
    if not first:
      dest.addParRi()

proc closeScope(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  # insert kill instructions for the locals that go out of scope:
  emitKill c, dest, info, c.typeCache.currentScopeLocals
  c.typeCache.closeScope()

proc killLocalsAbove(c: var Context; dest: var TokenBuf; depth: int;
                     info: NifLineInfo) =
  ## A forward jump leaves every scope above `depth`; their locals die on that
  ## edge exactly as they do at the scopes' ends.
  emitKill c, dest, info, c.typeCache.localsOfScopesAbove(depth)

proc freshLabel(c: var Context; prefix: string): SymId =
  result = pool.symId(prefix & $c.current.tmpCounter)
  inc c.current.tmpCounter

# ---------------------------------------------------------------- versions

proc curVersion(c: var Context; s: SymId): int {.inline.} =
  current(c.current.vers, s)

proc here(c: var Context): Mark {.inline.} =
  mark(c.current.vers)

proc newDef(c: var Context; s: SymId): int =
  ## A definition of `s`: it gets the next version.
  if c.analysisFacts: define(c.current.vers, s) else: 0

proc inScope(c: var Context; s: SymId): bool =
  s == MemSym or getLocalInfo(c.typeCache, s).kind != NoSym or
    not isLocalName(pool.syms[s])

proc sourcesAt(c: var Context; since: Mark; marks: openArray[Mark]): seq[Sources] =
  ## `versions.sourcesAt` for the variables that are still in scope.
  result = @[]
  let all = sourcesAt(c.current.vers, since, marks)
  for src in all:
    if inScope(c, src.sym): result.add src

proc restoreTo(c: var Context; m: Mark) =
  ## Entering another branch: every variable is back at the version it had
  ## at `m`.
  if c.analysisFacts: restore c.current.vers, m

proc emitJoin(c: var Context; dest: var TokenBuf; s: SymId; olds: openArray[int];
              unknownSource: bool; info: NifLineInfo) =
  ## `(join x new old…)`: `x` gets the version `new`, which is whichever of the
  ## `old` versions the path taken brought along. A `.` among them is a path
  ## on which nothing is known about `x`.
  dest.addParLe("join", info)
  if s == MemSym: dest.addDotToken()
  else: dest.addSymUse s, info
  dest.addIntLit newDef(c, s), info
  for o in olds: dest.addIntLit o, info
  if unknownSource: dest.addDotToken()
  dest.addParRi()

proc emitJoinsOf(c: var Context; dest: var TokenBuf; start: Mark;
                 ends: openArray[Mark]; info: NifLineInfo) =
  ## The joins after a construct that began at `start` and whose branches
  ## ended at `ends`.
  if not c.analysisFacts: return
  for src in sourcesAt(c, start, ends):
    var differ = false
    for v in src.olds:
      if v != src.olds[0]: differ = true
    if differ or (src.olds.len > 0 and src.olds[0] != curVersion(c, src.sym)):
      emitJoin c, dest, src.sym, src.olds, false, info

proc emitUnknownJoins(c: var Context; dest: var TokenBuf; start, known: Mark;
                      info: NifLineInfo) =
  ## Entering an `except` or a `fin`: a variable the `try` body, which ended
  ## at `known`, may have defined arrives with a value nothing is known
  ## about, and one that only an earlier handler touched is back at what it
  ## was before the `try`.
  if not c.analysisFacts: return
  let inBody = changedBetween(c.current.vers, start, known)
  for src in sourcesAt(c, start, [start]):
    if src.sym in inBody:
      emitJoin c, dest, src.sym, [], true, info
    elif src.olds[0] != curVersion(c, src.sym):
      emitJoin c, dest, src.sym, src.olds, false, info

proc noteBarrier(c: var Context; d: DefRoot) =
  if d.barrier:
    c.current.pendingBarrier = true
    if d.sym != NoSymId and d.sym notin c.current.spared:
      c.current.spared.add d.sym

proc noteStore(c: var Context; d: DefRoot) =
  ## A store that is over: it defines `d.sym`, and it may be a barrier.
  if c.analysisFacts:
    if d.sym != NoSymId: discard newDef(c, d.sym)
    noteBarrier c, d

proc flushBarrier(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## `(unknown . x…)`: a barrier. Whatever could be written without being
  ## named is unknown from here on; the shared variables `x…` were written by
  ## name and are not affected.
  if c.current.pendingBarrier:
    c.current.pendingBarrier = false
    if c.analysisFacts:
      dest.addParLe("unknown", info)
      dest.addDotToken()
      for s in c.current.spared: dest.addSymUse s, info
      dest.addParRi()
      discard newDef(c, MemSym)
    c.current.spared.setLen 0

proc emitLab(c: var Context; dest: var TokenBuf; name: SymId; info: NifLineInfo;
             headerEdge = NoMark) =
  ## `(lab L join…)`. The sources of a join are listed in the order of the
  ## `jmp`s to `L`, then the fall-through if control can reach the label that
  ## way. `headerEdge`: the exit of a `for`, which is also reached from the
  ## loop's header; that source comes first.
  dest.addParLe LabV, info
  dest.addSymDef name, info
  if c.analysisFacts:
    var edges: seq[Mark] = @[]
    if headerEdge != NoMark: edges.add headerEdge
    if c.current.labelEdges.hasKey(name):
      let jumps = c.current.labelEdges.getOrDefault(name)
      for e in jumps: edges.add e
      c.current.labelEdges.del name
    if edges.len > 0:
      var lo = edges[0]
      for e in edges:
        if e < lo: lo = e
      for src in sourcesAt(c, lo, edges):
        let cur = curVersion(c, src.sym)
        var olds = src.olds
        var differ = false
        for v in olds:
          if v != cur: differ = true
        if differ:
          if c.current.live: olds.add cur
          emitJoin c, dest, src.sym, olds, false, info
  c.current.live = true
  dest.addParRi()

proc emitJmp(c: var Context; dest: var TokenBuf; name: SymId; info: NifLineInfo) =
  dest.addParLe JmpV, info
  dest.addSymUse name, info
  dest.addParRi()
  if c.analysisFacts:
    c.current.labelEdges.mgetOrPut(name, @[]).add here(c)
  c.current.live = false

# Mutually recursive translators:
proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

type
  CallInfo = object
    # Each entry holds the inner location expression of a `(haddr …)` argument.
    # Recording the full path (not just the root sym) lets later alias analysis
    # tell `c.field` apart from other fields of `c`. We copy the path into its
    # own buffer rather than holding a Cursor into `dest`, which any later
    # append to `dest` would invalidate via COW.
    mutates: seq[TokenBuf]
    info: NifLineInfo

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo

proc trStmtsInline(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Translate the children of a `(stmts ...)`/`(scope ...)` *without* emitting
  ## a wrapping node. The caller already opened the enclosing list.
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  n.into:
    while n.hasMore:
      trStmt c, dest, n

proc trScopedBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Translate a body that opens its own lexical scope and emit it as a fresh
  ## `(scope ...)`. Scope-exit `kill`s are appended before the closing paren.
  ## It must be a `scope`: with the flat `lab`/`jmp` layout a branch body is a
  ## sibling in the enclosing list, so a transparent `stmts` would let its
  ## locals live on to the end of the enclosing scope for `destroyer`.
  let info = n.info
  openScope c
  dest.addParLe ScopeS, info
  if n.stmtKind in {StmtsS, ScopeS}:
    n.into:
      while n.hasMore:
        trStmt c, dest, n
    closeScope c, dest, info
    dest.addParRi()
  else:
    trStmt c, dest, n
    closeScope c, dest, info
    dest.addParRi()

# ------------------------------------------------------------------ expressions

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    dest.takeTree n
  of UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident, SymbolDef, StrLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeTree n
  of TagLit:
    case n.exprKind
    of CallKinds:
      bug "call must have been bound to a location"
    of AndX, OrX:
      bug "and/or should have been handled by the expression elimination pass xelim.nim"
    else:
      takeInto dest, n:
        while n.hasMore:
          if n.isTagLit and n.exprKind in CallKinds:
            # A call surviving to the Final IR in an operand position is an
            # lvalue location (xelim lifts every value-producing call), e.g. the
            # `s[i]` argument of a location-taking builtin like `=wasMoved`.
            # Emit it via `trCall`, which accepts a call here, rather than the
            # plain-expression path that rejects one. (Matches nj.nim / #2081.)
            discard trCall(c, dest, n)
          else:
            trExpr c, dest, n
  else: bug "Unmatched ParRi"

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo =
  let info = n.info
  result = CallInfo(info: info)
  var defs: seq[DefRoot] = @[]
  if c.analysisFacts:
    var barrier = false
    callDefs c.typeCache, n, defs, barrier
    if barrier: c.current.pendingBarrier = true
  takeInto dest, n: # the `(call)`
    trExpr c, dest, n # handle `fn`
    while n.hasMore:
      if n.exprKind == HaddrX:
        var inner = n
        inc inner # skip haddr tag
        if rootOf(inner, CanFollowCalls) != NoSymId:
          var pathBuf = createTokenBuf(4)
          pathBuf.addSubtree inner
          result.mutates.add ensureMove pathBuf
      trExpr c, dest, n
  # The call is over: it defines what it was handed as `var`.
  for d in defs: noteStore c, d

proc callIsOver(c: var Context; dest: var TokenBuf; callInfo: CallInfo) =
  # `unknown` marks that a `(haddr …)` argument's pointee may have been mutated.
  # This is cleaned up by the later alias/versionizer pass.
  if not c.analysisFacts: return
  for i in 0 ..< callInfo.mutates.len:
    dest.addParLe("unknown", callInfo.info)
    dest.add callInfo.mutates[i]
    dest.addParRi() # unknown
  flushBarrier c, dest, callInfo.info

proc trBoundExpr(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo =
  ## The expression is about to be bound directly to a location, so a `call`
  ## as the *whole* RHS is legal here (`doc/final_ir.md`: "Calls bind directly
  ## to their destination").
  if n.exprKind in CallKinds:
    result = trCall(c, dest, n)
  else:
    trExpr c, dest, n
    # `n` sits at the (possibly elided) close after the consumed expression;
    # the info is unused anyway since `mutates` is empty
    result = CallInfo(mutates: @[], info: n.endInfo)

# ------------------------------------------------------------------- statements

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let callInfo = trCall(c, dest, n)
  callIsOver(c, dest, callInfo)

proc containsCall(n: Cursor): bool =
  result = n.isTagLit and n.exprKind in CallKinds
  if not result:
    var n = n
    linearScan n:
      if n.exprKind in CallKinds:
        result = true
        break

proc trCond(c: var Context; dest: var TokenBuf; n: var Cursor; tag: string;
            negated: bool; info: NifLineInfo) =
  ## Opens `(tag` and emits the condition or selector `n` into it. A barrier
  ## in it (a call of a `proc` that returns a location) is reported in front
  ## of the construct: nothing can be put between a condition and its
  ## branches.
  if c.analysisFacts and containsCall(n):
    var cond = createTokenBuf(8)
    if negated: cond.addParLe NotX, info
    trExpr c, cond, n
    if negated: cond.addParRi()
    flushBarrier c, dest, info
    dest.addParLe tag, info
    dest.add cond
  else:
    dest.addParLe tag, info
    if negated: dest.addParLe NotX, info
    trExpr c, dest, n
    if negated: dest.addParRi()

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  dest.addParLe(n.cursorTagId, n.info)
  let localStart = n
  n = sub(n)

  let symId = n.symId
  if kind == ResultY:
    c.current.resultSym = symId

  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.typeCache.registerLocal(symId, kind, n)
  takeTree dest, n # type

  # For a `for` whose iterator xelim hoisted into a temp (`forRangeAssumes`).
  if n.isTagLit and n.exprKind in CallKinds:
    var whole = createTokenBuf(16)
    whole.addSubtree n
    c.callExprs[symId] = whole

  let callInfo = trBoundExpr(c, dest, n)
  let closeInfo = n.endInfo
  n = localStart; skip n
  dest.addParRi(closeInfo)
  if kind != ConstY: discard newDef(c, symId) # a const is no variable
  callIsOver(c, dest, callInfo)

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `(asgn X Y)` stays an `(asgn X Y)`: the operands keep source order.
  let info = n.info
  dest.addParLe AsgnS, info
  let asgnStart = n
  n = sub(n)
  if n.isSymbol:
    let symId = n.symId
    inc n # skip the destination symbol
    dest.addSymUse symId, info
    # `dest = f(args)`: the call binds directly to its destination, no temp.
    let callInfo = trBoundExpr(c, dest, n)
    let closeInfo = n.endInfo
    n = asgnStart; skip n
    dest.addParRi(closeInfo)
    noteStore c, DefRoot(sym: symId, barrier: isSharedVar(c.typeCache, symId))
    callIsOver(c, dest, callInfo)
  else:
    # As above, with a path as the destination: `a[i] = f(x)`.
    let d = if c.analysisFacts: defRoot(c.typeCache, n) else: DefRoot()
    var value = n
    skip value          # `value` is the RHS; `n` still at the destination
    trExpr c, dest, n   # the destination location
    let callInfo = trBoundExpr(c, dest, value) # then the value
    skip n              # past the RHS, so `n` sits at the `)`
    let closeInfo = n.endInfo
    n = asgnStart; skip n
    dest.addParRi(closeInfo)
    noteStore c, d
    callIsOver(c, dest, callInfo)

proc condIsComplex(n: Cursor): bool =
  ## A condition needs the two-target compiler (`Cx`) iff it contains a
  ## short-circuit `and`/`or`. A plain leaf (incl. a lone `not`) stays a single
  ## `ite` whose condition the tracker negates directly.
  var n = n
  if not n.isTagLit: return false
  if n.exprKind in {AndX, OrX}: return true
  linearScan n:
    if n.exprKind in {AndX, OrX}: return true
  result = false

proc genLeaf(c: var Context; dest: var TokenBuf; n: var Cursor;
             trueL, falseL, fallL: SymId) =
  ## Emit the guarded transfer for a pure boolean leaf: a one-armed `(ite cond
  ## (jmp …) .)` whose taken edge is a forward `jmp`. The jump to whichever
  ## target is the textual fall-through (`fallL`) is elided. This is exactly the
  ## shape the tracker already handles for loop guards.
  let info = n.info
  if falseL == fallL:
    # fall through when false; jump to `trueL` when true.
    trCond c, dest, n, "ite", false, info # condition (consumes the leaf)
    dest.addParLe StmtsS, info
    emitJmp c, dest, trueL, info
    dest.addParRi()             # close `stmts`
    dest.addDotToken()          # no else
    dest.addParRi()             # close `ite`
    c.current.live = true
  elif trueL == fallL:
    # fall through when true; jump to `falseL` when false (negated guard).
    trCond c, dest, n, "ite", true, info
    dest.addParLe StmtsS, info
    emitJmp c, dest, falseL, info
    dest.addParRi()
    dest.addDotToken()
    dest.addParRi()
    c.current.live = true
  else:
    # neither edge falls through: conditional jump to `trueL`, then `jmp falseL`.
    trCond c, dest, n, "ite", false, info
    dest.addParLe StmtsS, info
    emitJmp c, dest, trueL, info
    dest.addParRi()
    dest.addDotToken()
    dest.addParRi()
    c.current.live = true
    emitJmp c, dest, falseL, info

proc genCond(c: var Context; dest: var TokenBuf; n: var Cursor;
             trueL, falseL, fallL: SymId) =
  ## The two-target condition compiler `Cx(cond)(trueL, falseL)` from
  ## `doc/final_ir.md`. `and` chains on the FALSE target, `or` on the TRUE
  ## target (exact duals), `not` swaps targets. Each `and`/`or` join is a fresh
  ## shared `(lab)` — so a deeply nested boolean is *flat* and analyses in one
  ## forward pass instead of an exponential path tree. `fallL` names the label
  ## textually following this emission, for fall-through elision.
  case n.exprKind
  of AndX:
    let scStart = n
    n = sub(n)
    let z = freshLabel(c, "´cz.")
    genCond c, dest, n, z, falseL, z        # Cx(a)(z, f), a-true falls to z
    emitLab c, dest, z, n.info
    genCond c, dest, n, trueL, falseL, fallL # Cx(b)(t, f)
    n = scStart; skip n
  of OrX:
    let scStart = n
    n = sub(n)
    let z = freshLabel(c, "´cz.")
    genCond c, dest, n, trueL, z, z         # Cx(a)(t, z), a-false falls to z
    emitLab c, dest, z, n.info
    genCond c, dest, n, trueL, falseL, fallL # Cx(b)(t, f)
    n = scStart; skip n
  of NotX:
    let scStart = n
    n = sub(n)
    genCond c, dest, n, falseL, trueL, fallL # swap targets
    n = scStart; skip n
  else:
    genLeaf c, dest, n, trueL, falseL, fallL

proc genIfViaCx(c: var Context; dest: var TokenBuf; n: var Cursor;
                info: NifLineInfo; ifStart: Cursor) =
  ## `if <complex cond>: then [else: else]` via `Cx`, so short-circuit
  ## `and`/`or` lower to shared `(lab)`/`(jmp)` merges. `n` is at the `(elif`;
  ## `ifStart` is the enclosing `(if ...)` head captured by `trIf`, whose close
  ## is consumed here.
  var afterElif = n
  skip afterElif
  if afterElif.hasMore and afterElif.substructureKind != ElseU:
    bug "multi-branch `if` reached the Final IR: `xelim` nests elif chains into elif+else"
  let hasElse = afterElif.substructureKind == ElseU
  let thenL = freshLabel(c, "´ct.")
  let endL = freshLabel(c, "´ce.")
  let elseL = if hasElse: freshLabel(c, "´cl.") else: endL
  let elifStart = n # skip `elif`
  n = sub(n)
  genCond c, dest, n, thenL, elseL, thenL
  emitLab c, dest, thenL, info
  trScopedBody c, dest, n      # then-branch
  n = elifStart; skip n        # end of `elif`
  if hasElse:
    emitJmp c, dest, endL, info
    emitLab c, dest, elseL, info
    let elseStart = n # skip `else`
    n = sub(n)
    trScopedBody c, dest, n    # else-branch
    n = elseStart; skip n      # end of `else`
  if n.hasMore:
    bug "`if` with a branch after its `else` reached the Final IR"
  emitLab c, dest, endL, info
  n = ifStart; skip n          # end of `if`

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Precondition: `xelim` nested any elif chain into one elif+else. Checked
  ## with `bug`, not `assert`: the final resync would silently drop an extra
  ## branch.
  let info = n.info
  let ifStart = n
  n = sub(n)
  if n.substructureKind != ElifU:
    bug "`if` whose first branch is not an `elif` reached the Final IR"
  var condPeek = n
  inc condPeek               # at the condition
  if condIsComplex(condPeek):
    # Short-circuit `and`/`or`: use the two-target condition compiler.
    genIfViaCx c, dest, n, info, ifStart
    return

  let elifStart = n # skip `elif`
  n = sub(n)
  trCond c, dest, n, "ite", false, info # condition
  let start = here(c)
  let liveBefore = c.current.live
  trScopedBody c, dest, n    # then-branch
  n = elifStart; skip n      # end of `elif`
  let thenEnd = here(c)
  let thenLive = c.current.live
  c.current.live = liveBefore

  if n.hasMore:
    if n.substructureKind != ElseU:
      bug "multi-branch `if` reached the Final IR: `xelim` nests elif chains into elif+else"
    restoreTo c, start
    let elseStart = n
    n = sub(n)
    trScopedBody c, dest, n  # else-branch
    n = elseStart; skip n    # end of `else`
    if n.hasMore:
      bug "`if` with a branch after its `else` reached the Final IR"
    # `(join x new then else)`
    emitJoinsOf c, dest, start, [thenEnd, here(c)], info
  else:
    dest.addDotToken()       # no else section
    # without an `else` the second source is what held before the `ite`
    emitJoinsOf c, dest, start, [thenEnd, start], info
  c.current.live = c.current.live or thenLive

  n = ifStart; skip n        # end of `if`
  dest.addParRi()            # close `ite`

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `case` is kept as a construct (doc/final_ir.md). We only translate the
  # selector and the branch bodies; `ranges` hold pure constants after xelim
  # and are copied verbatim.
  let info = n.info
  # Touch the selector type so typenav stays consistent across the switch.
  discard c.typeCache.getType(n.childCursor)
  let caseStart = n
  n = sub(n)
  trCond c, dest, n, "case", false, info # selector
  let start = here(c)
  let liveBefore = c.current.live
  var ends: seq[Mark] = @[]
  var anyLive = false
  var hasElse = false
  while n.substructureKind == OfU:
    restoreTo c, start
    c.current.live = liveBefore
    takeInto dest, n:        # `of`
      takeTree dest, n       # ranges (pure constants)
      trScopedBody c, dest, n # branch body
    ends.add here(c)
    anyLive = anyLive or c.current.live
  if n.substructureKind == ElseU:
    hasElse = true
    restoreTo c, start
    c.current.live = liveBefore
    takeInto dest, n:        # `else`
      trScopedBody c, dest, n
    ends.add here(c)
    anyLive = anyLive or c.current.live
  if n.hasMore:
    # the resync below would silently drop it
    bug "`case` with a branch that is neither `of` nor `else` reached the Final IR"
  # `(join x new branch…)`, one source per branch in order
  emitJoinsOf c, dest, start, ends, info
  # without an `else` we cannot tell here that the branches are exhaustive
  c.current.live = anyLive or (not hasElse and liveBefore)
  dest.addParRi(n.endInfo)   # close `case`
  n = caseStart; skip n

proc trBreak(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `break` is a forward `(jmp loopExit)` to the matching construct's exit.
  let info = n.info
  let breakStart = n
  n = sub(n)
  var target = -1
  if not n.hasMore or n.isDotToken:
    if n.isDotToken: inc n
    # unnamed break: the innermost enclosing loop. A `block` is only ever left
    # by name; sem names it even for `.feature: "anonBlockBreaks"`.
    target = c.current.exits.len - 1
    while target >= 0 and not c.current.exits[target].isLoop:
      dec target
  elif n.isSymbol:
    let name = n.symId
    inc n
    for i in countdown(c.current.exits.len - 1, 0):
      if c.current.exits[i].blockName == name:
        target = i; break
  else:
    bug "invalid `break` structure"
  assert target >= 0, "break has no matching loop/block"
  c.current.exits[target].used = true
  killLocalsAbove c, dest, c.current.exits[target].scopeDepth, info
  emitJmp c, dest, c.current.exits[target].name, info
  n = breakStart; skip n

proc trContinue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## A source-level `continue` is a forward `(jmp cont)` to the label in front
  ## of the innermost loop's back-edge, so `(continue .)` stays the last
  ## statement of a loop body and its only back-edge.
  let info = n.info
  var target = c.current.exits.len - 1
  while target >= 0 and not c.current.exits[target].isLoop:
    dec target
  assert target >= 0, "continue has no enclosing loop"
  c.current.exits[target].contUsed = true
  # the loop's own scope is not left: its locals die at the back-edge
  killLocalsAbove c, dest, c.current.exits[target].scopeDepth + 1, info
  emitJmp c, dest, c.current.exits[target].cont, info
  skip n

proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `return` stays primitive: it carries the result and runs the epilogue.
  let info = n.info
  let retStart = n
  n = sub(n)
  dest.addParLe("ret", info)
  if not n.hasMore:
    dest.addDotToken()
  elif n.isDotToken:
    dest.addDotToken()
    inc n
  elif n.isSymbol and n.symId == c.current.resultSym:
    dest.addSymUse n.symId, info
    inc n
  else:
    trExpr c, dest, n
  dest.addParRi(n.endInfo)
  n = retStart; skip n
  c.current.live = false

proc trRaise(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `raise` stays primitive: it is type-dispatched by `except` and crosses
  # `finally`. A bare `(raise .)` re-raises the in-flight exception.
  let info = n.info
  let raiseStart = n
  n = sub(n)
  dest.addParLe("raise", info)
  if not n.hasMore:
    dest.addDotToken()
  elif n.isDotToken:
    dest.addDotToken()
    inc n
  else:
    trExpr c, dest, n
  dest.addParRi(n.endInfo)
  n = raiseStart; skip n
  c.current.live = false

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # A `block` has no Final IR construct of its own: it is `body (lab blockExit)`
  # where `break label` jumps forward to `blockExit`.
  let info = n.info
  let blockStart = n # `block`
  n = sub(n)
  let blockName = if n.isSymbolDef: n.symId else: NoSymId
  inc n # name or empty
  let exitL = freshLabel(c, "´blk.")
  c.current.exits.add Exit(name: exitL, blockName: blockName, isLoop: false,
                           scopeDepth: c.typeCache.scopeDepth)
  trScopedBody c, dest, n
  let used = c.current.exits[^1].used
  c.current.exits.shrink(c.current.exits.len - 1)
  n = blockStart; skip n # close `block`
  if used:
    emitLab c, dest, exitL, info

proc typedOperand(n: Cursor): Cursor =
  ## The location `(inc T loc …)`, `(incl T loc …)` and their kin write.
  result = n
  inc result
  skip result # the type

proc scanLoopDefs(c: var Context; body: Cursor): seq[LoopJoin] =
  ## What the body of a loop may define, so that the loop's header can hand
  ## out the joined versions before the body is translated. Read off the
  ## body as it is written, and generous: a variable listed here that the
  ## body does not define after all costs a join that says so.
  result = @[]
  if not c.analysisFacts: return
  var syms: seq[SymId] = @[]
  template note(d: DefRoot) =
    if d.sym != NoSymId and d.sym notin syms: syms.add d.sym
    if d.barrier and MemSym notin syms: syms.add MemSym
  template visit(n: Cursor) =
    if n.stmtKind == AsgnS:
      note defRoot(c.typeCache, n.childCursor)
    elif n.stmtKind in {IncS, DecS, InclS, ExclS}:
      note defRoot(c.typeCache, typedOperand(n))
    elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX}:
      note defRoot(c.typeCache, n.childCursor)
    elif n.exprKind in CallKinds or n.stmtKind in CallKindsS:
      var defs: seq[DefRoot] = @[]
      var barrier = false
      callDefs c.typeCache, n, defs, barrier
      for d in defs: note d
      if barrier and MemSym notin syms: syms.add MemSym
    elif n.stmtKind in {RetS, RaiseS} or
        (n.symKind in {VarY, LetY} and isDestroyed(c, n.symKind, asLocal(n).typ)):
      # a local with a destructor dies in the body, or may: an exit kills
      # the locals of the scopes it leaves (see `emitKill`)
      if MemSym notin syms: syms.add MemSym
  var n = body
  if n.isTagLit:
    visit n
    linearScan n:
      visit n
  for s in syms:
    if inScope(c, s):
      let entry = curVersion(c, s)
      result.add LoopJoin(sym: s, entry: entry, header: newDef(c, s))

proc emitLoopJoins(c: var Context; dest: var TokenBuf; joins: seq[LoopJoin];
                   bodyStart: Mark; info: NifLineInfo) =
  ## `(join x new entry back)` for a loop: `new` is what `x` is at the top of
  ## every iteration, `entry` what it was in front of the loop and `back` what
  ## the back-edge brings along.
  if not c.analysisFacts: return
  for src in sourcesAt(c, bodyStart, []):
    let s = src.sym
    var listed = false
    for j in joins:
      if j.sym == s: listed = true
    if not listed:
      bug "loop body defines a variable its header does not join: " &
        (if s == MemSym: "(memory)" else: pool.syms[s])
  for j in joins:
    dest.addParLe("join", info)
    if j.sym == MemSym: dest.addDotToken()
    else: dest.addSymUse j.sym, info
    dest.addIntLit j.header, info
    dest.addIntLit j.entry, info
    dest.addIntLit curVersion(c, j.sym), info
    dest.addParRi()

proc emitLoopTail(c: var Context; tail: var TokenBuf; info: NifLineInfo;
                  iterDefs: seq[DefRoot] = @[]; iterBarrier = false) =
  ## What follows the statements of a loop body: the label a `continue` jumps
  ## to, the scope's `kill`s and the back-edge. `iterDefs`, `iterBarrier`:
  ## what the iterator of a `for` writes when it is resumed.
  if c.current.exits[^1].contUsed:
    emitLab c, tail, c.current.exits[^1].cont, info
  for d in iterDefs: noteStore c, d
  if iterBarrier: c.current.pendingBarrier = true
  flushBarrier c, tail, info
  closeScope c, tail, info # kills run on the back-edge path
  tail.copyIntoKind ContinueV, info: # the sole back-edge
    tail.addDotToken()
  c.current.live = false

proc emitLoopBody(c: var Context; dest: var TokenBuf; body, tail: var TokenBuf;
                  info: NifLineInfo) =
  ## The translated statements of a loop body, then its tail. With a
  ## `continue`, the statements get their own `scope` and the label follows
  ## it: a `jmp` must not skip a declaration whose destructor would then run
  ## on an uninitialized value.
  if c.current.exits[^1].contUsed:
    dest.addParLe ScopeS, info
    dest.add body
    dest.addParRi()
  else:
    dest.add body
  dest.add tail

proc trLoopFromBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `n` points at the loop *body*. Emit the infinite
  ## `(loop (stmts <body> (continue .)))` and, if any `break` targeted it, the
  ## trailing `(lab loopExit)`.
  let info = n.info
  let exitL = freshLabel(c, "´lx.")
  c.current.exits.add Exit(name: exitL, isLoop: true, cont: freshLabel(c, "´lc."),
                           scopeDepth: c.typeCache.scopeDepth)
  let joins = scanLoopDefs(c, n)
  let bodyStart = here(c)
  openScope c
  var body = createTokenBuf(64)
  if n.stmtKind in {StmtsS, ScopeS}:
    n.into: # the body statement list
      while n.hasMore:
        trStmt c, body, n
  else:
    # a lone statement, as a pass ahead of this one may build it
    trStmt c, body, n
  var tail = createTokenBuf(16)
  emitLoopTail c, tail, info
  dest.addParLe LoopV, info
  emitLoopJoins c, dest, joins, bodyStart, info
  dest.addParLe ScopeS, info # a loop body is a scope: its locals die each iteration
  emitLoopBody c, dest, body, tail, info
  dest.addParRi() # close `scope`
  dest.addParRi() # close `loop`
  let used = c.current.exits[^1].used
  c.current.exits.shrink(c.current.exits.len - 1)
  if used:
    emitLab c, dest, exitL, info

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let whileStart = n # `while`
  n = sub(n)
  if n.exprKind == TrueX:
    # `while true` is already the canonical infinite loop.
    skip n # the `(true)` condition
    trLoopFromBody c, dest, n
    n = whileStart; skip n # close `while`
  else:
    # Rewrite `while cond: body` to `while true: (if cond: body else: break)`;
    # the leading-guard `break` becomes the forward `jmp loopExit`.
    let info = n.info
    var w = createTokenBuf(16)
    w.copyIntoKind StmtsS, info:
      w.copyIntoKind IfS, info:
        w.copyIntoKind ElifU, info:
          w.takeTree n # condition
          w.takeTree n # body
        w.copyIntoKind ElseU, info:
          w.copyIntoKind StmtsS, info:
            w.addParPair BreakS, info
    n = whileStart; skip n # close `while`
    var ww = beginRead(w)
    trLoopFromBody c, dest, ww

proc registerForVars(c: var Context; vars: Cursor) =
  ## Declare a `for`'s loop variables to the type cache: the iterator that
  ## binds them is not inlined yet.
  var vars = vars
  if vars.substructureKind in {UnpackflatU, UnpacktupU}:
    vars = sub(vars) # peek only, never left
    while vars.hasMore:
      registerForVars c, vars
      skip vars
  elif isLocal(vars.symKind):
    let local = asLocal(vars)
    c.typeCache.registerLocal(local.name.symId, vars.symKind, local.typ)
    discard newDef(c, local.name.symId)

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## A `for` stays a `for`: hexer's `elimForLoops` still needs the iterator
  ## call and the loop variables. Its body is lowered like a `loop`'s.
  let info = n.info
  let forStmt = asForStmt(n) # peek at structure before advancing
  let forStart = n
  let forTag = n.cursorTagId
  n = sub(n)
  var assumeBuf = createTokenBuf(0)
  forRangeAssumes(assumeBuf, forStmt, c.callExprs, info)

  let exitL = freshLabel(c, "´lx.")
  c.current.exits.add Exit(name: exitL, isLoop: true, cont: freshLabel(c, "´lc."),
                           scopeDepth: c.typeCache.scopeDepth)
  # the whole `for`: what the iterator is handed as `var` it may define
  let joins = scanLoopDefs(c, forStart)
  var iterDefs: seq[DefRoot] = @[]
  var iterBarrier = false
  if c.analysisFacts:
    var iter = n
    while iter.exprKind in {HderefX, HaddrX}: inc iter
    if iter.exprKind in CallKinds:
      callDefs c.typeCache, iter, iterDefs, iterBarrier
  let bodyStart = here(c)
  openScope c
  dest.addParLe(forTag, info)
  takeTree dest, n # the iterator call, verbatim: xelim already normalized it
  registerForVars c, n
  takeTree dest, n # the loop variables
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  var body = createTokenBuf(64)
  n.into: # the body statement list
    while n.hasMore:
      trStmt c, body, n
  var tail = createTokenBuf(16)
  emitLoopTail c, tail, info, iterDefs, iterBarrier
  emitLoopJoins c, dest, joins, bodyStart, info
  dest.addParLe ScopeS, info # as in `trLoopFromBody`: the body is a scope
  if assumeBuf.len > 0:
    dest.add assumeBuf
  emitLoopBody c, dest, body, tail, info
  dest.addParRi() # close `scope`
  dest.addParRi(n.endInfo) # close `for`
  c.current.exits.shrink(c.current.exits.len - 1)
  # The exit label is emitted even when nothing jumps to it: the iterator's
  # own termination test is not visible yet, and without the label the code
  # after the loop would look unreachable to the prover, whose facts are
  # contradictory on a dead path. It is reached from the loop's header, when
  # the iterator is done: with the joined versions, and before any other source.
  emitLab c, dest, exitL, info, bodyStart
  n = forStart; skip n # close `for`

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `try`/`except`/`fin` are kept as regions: a `fin` must run on *every* exit
  # crossing it, so it cannot be lowered to a `jmp` (doc/final_ir.md).
  let info = n.info
  dest.addParLe("try", info)
  let tryStart = n
  n = sub(n)
  let start = here(c)
  trScopedBody c, dest, n # try body
  let bodyEnd = here(c)
  var ends = @[bodyEnd]
  while n.substructureKind == ExceptU:
    takeInto dest, n: # `except`
      # `(join x new .)`: a handler is entered from anywhere in the body
      emitUnknownJoins c, dest, start, bodyEnd, info
      while n.hasMore:
        if n.stmtKind in {StmtsS, ScopeS}:
          trScopedBody c, dest, n # handler body
        elif isLocal(n.symKind):
          # the bound exception variable, e.g. `(let e ErrorCode .)`
          let local = asLocal(n)
          c.typeCache.registerLocal(local.name.symId, n.symKind, local.typ)
          discard newDef(c, local.name.symId)
          takeTree dest, n
        else:
          takeTree dest, n # exception type pattern
    ends.add here(c)
  if n.substructureKind == FinU:
    takeInto dest, n: # `fin`
      # entered from the end of the body, from a handler, and from wherever
      # an exception, a `ret` or a `jmp` leaves them
      emitUnknownJoins c, dest, start, here(c), info
      trScopedBody c, dest, n
  else:
    # `(join x new body handler…)`
    emitJoinsOf c, dest, start, ends, info
  if n.hasMore:
    # the resync below would silently drop it
    bug "`try` with a clause that is neither `except` nor `fin` reached the Final IR"
  c.current.live = true
  dest.addParRi(n.endInfo) # close `try`
  n = tryStart; skip n

proc trAsmStmt(dest: var TokenBuf; n: var Cursor)

proc trAsmIf(dest: var TokenBuf; n: var Cursor) =
  ## The branches of an `if` from the first `elif` on, as nested `ite`s.
  let info = n.info
  dest.addParLe IteV, info
  n.into:                   # elif
    dest.takeTree n         # the condition, verbatim
    trAsmStmt dest, n
  if not n.hasMore:
    dest.addDotToken()
  elif n.substructureKind == ElseU:
    n.into:
      trAsmStmt dest, n
  else:
    dest.copyIntoKind StmtsS, info:
      trAsmIf dest, n
  dest.addParRi()

proc trAsmStmt(dest: var TokenBuf; n: var Cursor) =
  ## An `{.assembler.}` body keeps its statements and their order; only its
  ## `if`s are spelled as the `ite` the back end reads again as that `if`.
  case n.stmtKind
  of IfS:
    n.into:
      trAsmIf dest, n
  of StmtsS, ScopeS:
    copyInto dest, n:
      while n.hasMore:
        trAsmStmt dest, n
  of WhileS:
    # `while true: …` — the condition stays, the body may hold `if`s
    copyInto dest, n:
      dest.takeTree n
      trAsmStmt dest, n
  else:
    dest.takeTree n

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1, returnType: r.retType, live: true)

  # An `{.assembler.}` body is machine code written by hand: it has no contracts
  # to check, and its constructs are outside the Final IR's vocabulary anyway (a
  # machine flag as an `if` condition is not an expression). It is passed on
  # verbatim, except that its `if`s become `ite` (`trAsmStmt`); the prover
  # skips it (`traverseProc`).
  let isAsm = hasPragma(r.pragmas, AssemblerP)
  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isAsm:
      trAsmStmt dest, n
    elif isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
      if n.stmtKind in {StmtsS, ScopeS}:
        c.typeCache.openScope()
        let info = n.info
        copyIntoKind dest, StmtsS, info:
          openScope c
          trStmtsInline c, dest, n
          closeScope c, dest, info
        c.typeCache.closeScope()
      else:
        # No body: a `.` for an importc / forward declaration.
        takeTree dest, n
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    # Transparent in statement position: `xelim`'s hoisted declarations and
    # `(pragmax … (stmts (let x …)))` are used after it. Only branch and loop
    # bodies open a scope (`trScopedBody`).
    copyInto dest, n:
      while n.hasMore:
        trStmt c, dest, n
  of ScopeS:
    trScopedBody c, dest, n
  of AsgnS:
    trAsgn c, dest, n
  of IfS:
    trIf c, dest, n
  of CaseS:
    trCase c, dest, n
  of WhileS:
    trWhile c, dest, n
  of ForS:
    trFor c, dest, n
  of ConstS:
    var consts = move c.hoistedConsts
    trLocal c, consts, n
    c.hoistedConsts = ensureMove consts
  of LocalDecls - {ConstS}:
    trLocal c, dest, n
  of ProcS, FuncS, MethodS, ConverterS, IteratorS:
    trProcDecl c, dest, n
  of RetS:
    trRet c, dest, n
  of BreakS:
    trBreak c, dest, n
  of ContinueS:
    trContinue c, dest, n
  of BlockS:
    trBlock c, dest, n
  of MacroS, TemplateS, TypeS:
    # Macro bodies are owned by the out-of-process plugin, never lowered in
    # the user's compile output. Pass through opaquely.
    takeTree dest, n
  of RaiseS:
    trRaise c, dest, n
  of TryS:
    trTry c, dest, n
  of CallKindsS:
    trStmtCall c, dest, n
  of AssumeS, AssertS:
    # A proposition, not code: `{.assume.}` and `{.assert.}` are never evaluated
    # at run time, so there is nothing to lower. A call in one (`s.len`) names a
    # location the prover resolves itself, and an `and` is logic rather than
    # control flow — binding either to a temp is exactly what would destroy the
    # statement. Handed on verbatim.
    takeTree dest, n
  of LabS:
    # Already Final IR: `xelim` lowers short-circuit `and`/`or` to these.
    emitLab c, dest, n.childCursor.symId, n.info
    skip n
  of JmpS:
    emitJmp c, dest, n.childCursor.symId, n.info
    skip n
  of ImportS, ImportasS, FromimportS, ImportexceptS, IncludeS, ExportS,
     ExportexceptS, CommentS, PragmasS:
    # module bookkeeping, not code
    takeTree dest, n
  of InclS, ExclS, IncS, DecS:
    let info = n.info
    let d = if c.analysisFacts: defRoot(c.typeCache, typedOperand(n)) else: DefRoot()
    trExpr c, dest, n
    noteStore c, d
    flushBarrier c, dest, info
  of YldS, DiscardS, EmitS:
    # children are plain expressions; `trExpr` handles a call operand (`s[i]`)
    let info = n.info
    trExpr c, dest, n
    flushBarrier c, dest, info
  else:
    if n.kind == DotToken:
      # an empty statement slot: a missing routine body, an emptied branch
      dest.takeTree n
    elif n.finalIrKind in {MflagV, VflagV}:
      # NJVL control-flow flags. `xelim` used to materialise short-circuit
      # conditions into these for `nj.nim`, and this pass passed the bool
      # storage through; that lowering went out with `nj.nim`, so nothing
      # produces one any more. The tags stay in the NIF spec (doc/tags.md).
      bug "cfvar in Final IR input"
    elif n.exprKind == PragmaxX:
      copyInto(dest, n):
        takeTree dest, n  # pragmas
        trStmt c, dest, n # body
    elif n.exprKind == ProccallX:
      trStmtCall c, dest, n
    elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX}:
      # a hook call in statement position; its children are locations
      let info = n.info
      let d = if c.analysisFacts: defRoot(c.typeCache, n.childCursor) else: DefRoot()
      trExpr c, dest, n
      noteStore c, d
      flushBarrier c, dest, info
    elif n.exprKind == TraceX:
      trExpr c, dest, n
    else:
      # Fallback: keep the statement, lower its children as expressions. A
      # nested statement body would stay unlowered here.
      # `-d:firFallbackProbe` lists what reaches this branch.
      when defined(firFallbackProbe):
        stderr.writeLine "FIR-FALLBACK " & globalTags.tags[n.cursorTagId]
      let info = n.info
      trExpr c, dest, n
      flushBarrier c, dest, info

proc stripAnalysisFactsInto(dest: var TokenBuf; n: var Cursor) =
  if n.isTagLit:
    if n.finalIrKind in {KillV, UnknownV} or n.substructureKind == JoinU:
      skip n
    else:
      copyInto dest, n:
        while n.hasMore:
          stripAnalysisFactsInto dest, n
  else:
    dest.takeTree n

proc stripAnalysisFacts*(buf: sink TokenBuf): TokenBuf =
  ## Remove the prover's `(kill …)`, `(unknown …)` and `(join …)` facts from a lowered
  ## module; the backend derives destruction from the `scope` tags.
  var n = beginRead(buf)
  result = createTokenBuf(buf.len)
  while n.hasMore:
    stripAnalysisFactsInto result, n

proc toFinalIr*(pass: var Pass; hooks: ref Table[SymId, HooksPerType];
                analysisFacts = true) =
  ## `analysisFacts`: see `Context.analysisFacts`. `hooks`: the hooks of the
  ## types of the module, which their declarations do not list yet.
  let lifter = createLiftingCtx(pass.moduleSuffix, pass.bits, hooks)
  assert lifter != nil # `lifter` is a `lenientnils` module
  var c = Context(counter: 0, typeCache: createTypeCache(pass.bits),
                  thisModuleSuffix: pass.moduleSuffix,
                  analysisFacts: analysisFacts, lifter: lifter)
  c.current.live = true
  c.openScope()
  lowerExprs(pass, TowardsFinalIr)
  pass.prepareForNext("finalir")
  var n = pass.n
  assert n.stmtKind == StmtsS, $n.kind
  pass.dest.addParLe(n.cursorTagId, n.info)
  c.openScope()
  # Every const ends up at module level, in front of the statement that
  # declared it: its value is a compile-time constant and sem gave it a global
  # name (`RoutineLikeSyms`), so no later pass has to care about a const in a
  # routine (a nested routine that lambdalifting lifts, an inline iterator body
  # that another module copies, ...).
  c.hoistedConsts = createTokenBuf(16)
  var stmt = createTokenBuf(300)
  n.into:
    while n.hasMore:
      trStmt c, stmt, n
      pass.dest.add c.hoistedConsts
      c.hoistedConsts.shrink 0
      pass.dest.add stmt
      stmt.shrink 0
    closeScope c, pass.dest, n.endInfo
  pass.dest.addParRi()

proc toFinalIr*(pass: var Pass; analysisFacts = true) =
  toFinalIr(pass, (ref Table[SymId, HooksPerType])(), analysisFacts)

when isMainModule:
  from std/os import paramStr, paramCount
  import std/syncio
  import ".." / lib / symparser
  let infile = paramStr(1)
  var owningBuf = createTokenBuf(300)
  let n = setupProgram(infile, infile.changeModuleExt".finalir.nif", owningBuf)
  var initialBuf = createTokenBuf(300)
  initialBuf.addSubtree(n)
  # A standalone debug driver: no target, so the host's width is stated.
  var pass = initPass(move initialBuf, "main", "xelim_finalir", sizeof(int)*8)
  toFinalIr(pass)
  let output = pass.dest.toString(false)
  if paramCount() >= 2:
    let outfile = paramStr(2)
    var f = syncio.open(outfile, fmWrite)
    try:
      syncio.write(f, output)
    finally:
      syncio.close(f)
  else:
    echo output

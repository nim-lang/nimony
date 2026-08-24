#
#
#           NIFC Tail-Call Encoding (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## One pass, two peephole rewrites over statement lists, which together produce
## `(ret (call …))` — the shape a backend reads as "this call is in tail
## position", so it can branch instead of call-and-return.
##
## * `trSink` pushes a `(ret r)` that ends a statement list down into the arms
##   of the `if` that produced `r`.
## * `trFold` folds every `(var :t T (call …)) (ret t)` pair — the ones that
##   were always there and the ones the sinking just made — into the encoding.
##
## They are two rewrite ROUNDS in that order, because the fold matches on
## ADJACENCY and so has to see the buffer the sinking produced. They are two
## `SHOGGOTH_DISABLE` names for the same reason they are one pass: sinking
## changes control-flow shape and folding changes stack discipline, so the two
## want separate answers to "which one broke it", but nothing else about them is
## separate — one walker, one grammar, one emitter, run twice.
##
## The pass runs at the very END of `optimizeBody`, after the vectorizer.
## `(ret (call …))` deliberately violates the Leng rule that calls are bound and
## do not nest — it is a directive to the backend ("do the tail call"), not an
## expression — so no other pass should have to tolerate it. Running last
## confines the exception to the output. The input it wants is the fully
## optimized body anyway: copyprop has by then collapsed nimsem's `result`
## variable away, so a proc whose body is one call arrives here as exactly the
## two statements the fold matches.
##
## The C backend needs no help with any of it: `genstmts` already renders
## `(ret X)` as `return X;`, so the fold just hands the C compiler `return f(a);`.
##
## Implementation. This is the only shoggoth pass that matches on statement
## ADJACENCY rather than within a subtree, which is what makes the `.` holes
## copyprop leaves behind visible here and nowhere else. The walk is therefore a
## PEEPHOLE: one forward pass per statement list carrying the last two real
## statements (`ListCtx`) — no materialized child list, no index arithmetic, and
## no hole-skipping beyond the one `if` that declines to shift the window. The
## sink rule's "the `ret` must end the list" then costs nothing to enforce: at
## the end of the walk `prev` IS the last real statement.
##
## The two sides are split the way `vectorizer.nim` splits them: a `TailScan`
## threads the MATCH side, a `RetEmitter` the BUILD side. What crosses between
## them is POSITIONS, never cursors — a cursor does not survive the patch round
## that rewrites the buffer.

import std / [tables]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import patchsets

type
  TailRule* = enum
    ## The two rewrites. The driver passes the enabled ones in; each round then
    ## collects with the single rule it is about, gathering nothing the round
    ## will not use. One `collect`, but a WALK PER ROUND, not one walk for both:
    ## the fold's input is the buffer the sink round rebuilt, and no position
    ## survives a `patch.apply()`.
    trSink        ## `… (if …) (ret r)` -> a `(ret E)` in every arm
    trFold        ## `(var :t T (call …)) (ret t)` -> `(ret (call …))`

  FoldSite = object
    ## The decl to delete, the `ret` to replace, and the call that becomes the
    ## replacement's operand.
    varPos, retPos, callPos: int
    retStart: int         ## the built `(ret <call>)`, once the emitter has it

  SunkArm = object
    ## One arm's tail `(asgn r E)`: the statement to replace, and the `E` that
    ## becomes that arm's own `(ret E)`.
    asgnPos, valuePos: int
    retStart: int         ## the built `(ret E)`, once the emitter has it

  SinkSite = object
    ## The `(ret r)` and `r`'s decl both go; every arm's tail assignment becomes
    ## a `ret` of its own.
    retPos, declPos: int
    arms: seq[SunkArm]

  ListCtx = object
    ## What the walk of ONE statement list carries: the peephole window, and
    ## where each symbol declared in this list was declared.
    prev, prev2: Cursor   ## the last two real statements; valid once `seen` says so
    seen: int             ## real statements passed so far
    decls: Table[SymId, int]

  TailScan = object
    ## The match side's pass-wide state, threaded as `s`. `buf` is here for
    ## `posOf` alone: a match must record positions, because a cursor does not
    ## survive the patch round that follows.
    buf: ptr TokenBuf
    want: TailRule
    folds: seq[FoldSite]
    sinks: seq[SinkSite]

proc posOf(s: TailScan; n: Cursor): int {.inline.} =
  cursorToPosition(s.buf[], n)

proc countReads(n: Cursor; sym: SymId): int =
  ## `Symbol` (read) occurrences of `sym` in one subtree. A `SymbolDef` is a decl,
  ## not a read, and does not count.
  case n.kind
  of Symbol:
    result = (if n.symId == sym: 1 else: 0)
  of TagLit:
    result = 0
    var it = n
    it.into:
      while it.hasMore:
        result += countReads(it, sym)
        skip it
  else: result = 0

# ---- grammar ---------------------------------------------------------------

proc declaredSym(n: Cursor; sym: var SymId): bool =
  ## The symbol a `(var :s …)` declares, malformed decls included: a caller
  ## looking for `s`'s decl must SEE a broken one rather than walk past it and
  ## adopt a later homonym.
  result = false
  if n.kind == TagLit and n.stmtKind == VarS:
    let c = n.childCursor
    if c.kind == SymbolDef:
      sym = c.symId
      result = true

proc varInit(n: Cursor; init: var Cursor): bool =
  ## The initializer of a node `declaredSym` has already accepted. A decl too
  ## short to have one is not one.
  result = false
  var c = n.childCursor
  skip c                                      # name
  if c.hasMore:
    skip c                                    # pragmas
    if c.hasMore:
      skip c                                  # type
      if c.hasMore:
        init = c
        result = true

proc matchVarDecl(n: Cursor; sym: var SymId; init: var Cursor): bool =
  ## `(var :sym pragmas T init)`, with both `sym` and `init` reported.
  result = declaredSym(n, sym) and varInit(n, init)

proc matchRetSym(n: Cursor; sym: var SymId): bool =
  ## `(ret sym)` exactly. `(ret sym …)` with extras is some shape we do not
  ## model, so it is not one.
  result = false
  if n.kind == TagLit and n.stmtKind == RetS:
    var c = n.childCursor
    if c.kind == Symbol:
      sym = c.symId
      skip c
      result = not c.hasMore

proc matchTailAsgn(n: Cursor; sym: SymId; value: var Cursor): bool =
  ## `(asgn sym E)` exactly, with `E` reported. A third child means a shape we
  ## do not model, so it is not one.
  result = false
  if n.kind == TagLit and n.stmtKind == AsgnS:
    var c = n.childCursor
    if c.kind == Symbol and c.symId == sym:
      skip c
      if c.hasMore:
        value = c
        skip c
        result = not c.hasMore

proc tailStmtOf(n: Cursor; res: var Cursor): bool =
  ## The last real statement of `n`, seen through the nested `(stmts …)`/`(scope …)`
  ## wrappers hexer's scope lowering leaves behind. False when a wrapper is empty.
  var cur = n
  var guard = 0
  result = true
  while result and cur.kind == TagLit and cur.stmtKind in {StmtsS, ScopeS} and
        guard < 64:
    inc guard
    var last = cur
    var found = false
    var it = cur
    it.into:
      while it.hasMore:
        if it.kind != DotToken:
          last = it
          found = true
        skip it
    if found: cur = last
    else: result = false
  if result: res = cur

proc matchTotalIf(n: Cursor; bodies: var seq[Cursor]): bool =
  ## Every arm's body of a TOTAL `if` — one that has an `else`. Without an
  ## `else` a path leaves the `if` having assigned nothing, and sinking would
  ## delete the `ret` that path still needed. A malformed arm answers false too.
  result = false
  if n.kind == TagLit and n.stmtKind == IfS:
    bodies.setLen 0
    var hasElse = false
    var ok = true
    var it = n
    it.into:
      while it.hasMore:
        let k = it
        case k.substructureKind
        of ElifU:
          var b = k.childCursor
          skip b                                # the condition
          if b.hasMore: bodies.add b
          else: ok = false
        of ElseU:
          var b = k.childCursor
          if b.hasMore: (bodies.add b; hasElse = true)
          else: ok = false
        else: ok = false
        skip it
    result = ok and hasElse and bodies.len > 0

proc matchSunkArms(s: TailScan; ifNode: Cursor; sym: SymId;
                   arms: var seq[SunkArm]): bool =
  ## Every arm of `ifNode` ends in `(asgn sym E)`. All or nothing: one arm that
  ## ends in something else and the `ret` cannot move at all.
  var bodies: seq[Cursor] = @[]
  result = matchTotalIf(ifNode, bodies)
  arms.setLen 0
  var i = 0
  while result and i < bodies.len:
    var last = default(Cursor)
    var value = default(Cursor)
    result = tailStmtOf(bodies[i], last) and matchTailAsgn(last, sym, value)
    if result:
      arms.add SunkArm(asgnPos: s.posOf(last), valuePos: s.posOf(value),
                       retStart: -1)
    inc i

# ---- the peephole walk -----------------------------------------------------

proc rememberDecl(s: TailScan; lc: var ListCtx; n: Cursor) =
  ## Record the FIRST decl this list makes of each symbol: its position when it
  ## is the initializer-free `(var :sym T .)` the sink rewrite can delete, and -1
  ## for any other decl of that symbol. First-wins is the point — a later
  ## homonym must not be adopted in place of a decl that disqualified the site.
  ##
  ## The decl has to be a SIBLING of the `ret`: that is what makes this statement
  ## list the symbol's whole scope, so counting mentions in it is complete — and
  ## it is what lets the decl go with the rewrite. Recording it during the walk
  ## is all that requirement costs.
  var sym = default(SymId)
  if s.want == trSink and declaredSym(n, sym) and sym notin lc.decls:
    var init = default(Cursor)
    lc.decls[sym] =
      if varInit(n, init) and init.kind == DotToken: s.posOf(n) else: -1

proc matchFold(s: var TailScan; list: Cursor; lc: ListCtx; cur: Cursor) =
  ## The window is `(var :t T (call …))` followed by `(ret t)`.
  ##
  ## ADJACENCY is the whole soundness argument. A statement between the call and
  ## the return is exactly what makes this not a tail call — a `=destroy` of a
  ## local, say, needs the frame the callee would inherit. The window has already
  ## stepped over the `.` holes copyprop left where it deleted bindings, which
  ## are not statements.
  ##
  ## The last conjunct is the temp's read count: the `ret` must be its ONLY
  ## read, since folding the call into it drops the temp entirely and a second
  ## reader would be left without one. It is tested last because it is the one
  ## test that walks a subtree.
  var t = default(SymId)
  var r = default(SymId)
  var init = default(Cursor)
  if s.want == trFold and lc.seen >= 1 and
     matchVarDecl(lc.prev, t, init) and
     init.kind == TagLit and init.exprKind == CallC and
     matchRetSym(cur, r) and r == t and
     countReads(list, t) == 1:
    s.folds.add FoldSite(varPos: s.posOf(lc.prev), retPos: s.posOf(cur),
                         callPos: s.posOf(init), retStart: -1)

proc matchSink(s: var TailScan; list: Cursor; lc: ListCtx) =
  ## Runs once the list is exhausted, so the window holds its last two real
  ## statements: `(if …)` then `(ret r)`.
  ##
  ## That the `ret` ENDS the list is the reason to match here rather than in the
  ## walk. The rewrite DELETES that `ret`; deleting it in front of live code
  ## would let control run on. Matching at the end makes the requirement
  ## structural instead of a lookahead.
  ##
  ## The read count closes the site: nothing may mention `r` but the arms'
  ## assignments and this `ret`.
  var sym = default(SymId)
  if s.want == trSink and lc.seen >= 2 and matchRetSym(lc.prev, sym):
    let declPos = lc.decls.getOrDefault(sym, -1)
    var arms: seq[SunkArm] = @[]
    if declPos >= 0 and matchSunkArms(s, lc.prev2, sym, arms) and
       countReads(list, sym) == arms.len + 1:
      s.sinks.add SinkSite(retPos: s.posOf(lc.prev), declPos: declPos,
                           arms: arms)

proc scan(s: var TailScan; n: Cursor)   # forward

proc scanList(s: var TailScan; list: Cursor) =
  ## One statement list: slide the peephole window over its real statements,
  ## and recurse into each of them.
  var lc = ListCtx()
  var it = list
  it.into:
    while it.hasMore:
      if it.kind != DotToken:
        rememberDecl(s, lc, it)
        matchFold(s, list, lc, it)
        lc.prev2 = lc.prev
        lc.prev = it
        inc lc.seen
      scan(s, it)
      skip it
  matchSink(s, list, lc)

proc scan(s: var TailScan; n: Cursor) =
  ## Every statement list under `n`.
  if n.kind == TagLit:
    if n.stmtKind in {StmtsS, ScopeS}:
      scanList(s, n)
    else:
      var it = n
      it.into:
        while it.hasMore:
          scan(s, it)
          skip it

proc collect(buf: var TokenBuf; want: TailRule): TailScan =
  ## Every site one rule matches, as positions into `buf`.
  result = TailScan(buf: addr buf, want: want)
  var root = beginRead(buf)
  while root.hasMore:
    scan(result, root)
    skip root
  endRead root

# ---- emission --------------------------------------------------------------

type
  RetEmitter = object
    ## The build side. Every replacement goes into ONE scratch buffer and no
    ## cursor is taken from it until they all have: `cursorAt` marks a buffer
    ## shared, so a cursor taken early makes each later `add` copy the whole
    ## thing — one copy per site instead of one in total.
    scratch: TokenBuf
    hole: TokenBuf        ## a lone `.`: what a deleted statement becomes

proc createRetEmitter(buf: TokenBuf; sizeHint: int): RetEmitter =
  result = RetEmitter(scratch: createTokenBuf(sizeHint, buf.pool, buf.tags),
                      hole: createTokenBuf(2, buf.pool, buf.tags))
  result.hole.addDotToken()

proc addRet(e: var RetEmitter; buf: var TokenBuf; valuePos: int): int =
  ## `(ret <the tree at valuePos>)`, reported by its start in the scratch buffer.
  result = e.scratch.len
  var v = cursorAt(buf, valuePos)
  e.scratch.openTag TagId(RetS)
  e.scratch.addSubtree v
  e.scratch.closeTag()
  endRead v

proc holeCursor(e: var RetEmitter): Cursor {.inline.} =
  cursorAt(e.hole, 0)

proc retCursor(e: var RetEmitter; start: int): Cursor {.inline.} =
  cursorAt(e.scratch, start)

# ---- the two rounds --------------------------------------------------------

proc sinkReturns(buf: var TokenBuf) =
  ## Push a `(ret r)` that ends a statement list down into the arms of the `if`
  ## that produced `r`:
  ##
  ##   (var :r T .)                          (if (elif c (stmts … (ret E1)))
  ##   (if (elif c (stmts … (asgn r E1)))        (else (stmts … (ret E2))))
  ##       (else (stmts … (asgn r E2))))
  ##   (ret r)
  ##
  ## Two things come of it. `r` disappears — one local and one copy per site, the
  ## same shape of win as forwarding a callee's result through its tail copy. And
  ## a call that was the arm's own tail becomes ADJACENT to a `ret`, which is
  ## exactly the condition the fold needs, so the tail-call encoding reaches a
  ## shape it otherwise never sees: in nimsem that is 339 sites and 111 arms,
  ## against the 10 tail calls the adjacent form finds on its own.
  ##
  ## The duplication is free on both backends: each `ret` was already a branch to
  ## one shared epilogue, and each arm already ended in a branch out of the `if`.
  var s = collect(buf, trSink)
  if s.sinks.len > 0:
    var e = createRetEmitter(buf, s.sinks.len * 16 + 4)
    for site in s.sinks.mitems:
      for arm in site.arms.mitems:
        arm.retStart = addRet(e, buf, arm.valuePos)
    var patch = initPatchset(addr buf)
    for site in s.sinks:
      patch.addSubst(site.retPos, e.holeCursor)
      patch.addSubst(site.declPos, e.holeCursor)
      for arm in site.arms:
        patch.addSubst(arm.asgnPos, e.retCursor(arm.retStart))
    var nb = patch.apply()
    buf = ensureMove(nb)

proc foldTailCalls(buf: var TokenBuf) =
  ## `(var :t T (call …)) (ret t)` -> `(ret (call …))`, the tail-call encoding.
  var s = collect(buf, trFold)
  if s.folds.len > 0:
    var e = createRetEmitter(buf, s.folds.len * 8 + 4)
    for site in s.folds.mitems:
      site.retStart = addRet(e, buf, site.callPos)
    var patch = initPatchset(addr buf)
    for site in s.folds:
      patch.addSubst(site.varPos, e.holeCursor)
      patch.addSubst(site.retPos, e.retCursor(site.retStart))
    var nb = patch.apply()
    buf = ensureMove(nb)

proc runTailCalls*(buf: var TokenBuf; rules: set[TailRule]) =
  ## The tail-call encoding. Sinking first, since the fold matches on adjacency
  ## and has to see the shape sinking produces; each round is its own patch
  ## round for the same reason.
  if trSink in rules: sinkReturns(buf)
  if trFold in rules: foldTailCalls(buf)

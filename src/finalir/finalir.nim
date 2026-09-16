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

import std / [tables, assertions, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import ".." / hexer / [xelim, mover, passes]
import finalir_model, condextractor

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

  CurrentProc = object
    resultSym: SymId
    returnType: Cursor
    tmpCounter: int
    exits: seq[Exit]

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentProc
    callExprs: Table[SymId, TokenBuf] ## whole init call of a local, so a `for`
                                      ## whose iterator xelim hoisted into a
                                      ## temp can still be read (see `trFor`)

proc openScope(c: var Context) =
  c.typeCache.openScope()

proc closeScope(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  # insert kill instructions for the locals that go out of scope:
  var locals: seq[SymId] = @[]
  for s in c.typeCache.currentScopeLocals:
    locals.add s
  # sort by name: the scope table iterates in SymId-hash order, which follows
  # pool interning order and is not stable across toolchain changes — the
  # golden files need deterministic output (kill order is a set)
  for i in 1 ..< locals.len:
    var j = i
    while j > 0 and pool.symString(locals[j-1]) > pool.symString(locals[j]):
      swap locals[j-1], locals[j]
      dec j
  var i = 0
  for s in locals:
    if i == 0:
      dest.addParLe("kill", info)
    dest.addSymUse s, info
    inc i
  if i > 0:
    dest.addParRi()
  c.typeCache.closeScope()

proc freshLabel(c: var Context; prefix: string): SymId =
  result = pool.symId(prefix & $c.current.tmpCounter)
  inc c.current.tmpCounter

proc emitLab(dest: var TokenBuf; name: SymId; info: NifLineInfo) =
  dest.addParLe LabV, info
  dest.addSymDef name, info
  dest.addParRi()

proc emitJmp(dest: var TokenBuf; name: SymId; info: NifLineInfo) =
  dest.addParLe JmpV, info
  dest.addSymUse name, info
  dest.addParRi()

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
  ## `(stmts ...)`. Scope-exit `kill`s are appended before the closing paren.
  let info = n.info
  openScope c
  dest.addParLe StmtsS, info
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

proc callIsOver(c: var Context; dest: var TokenBuf; callInfo: CallInfo) =
  # `unknown` marks that a `(haddr …)` argument's pointee may have been mutated.
  # This is cleaned up by the later alias/versionizer pass.
  for path in callInfo.mutates:
    dest.addParLe("unknown", callInfo.info)
    dest.add path
    dest.addParRi() # unknown

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

  # Record call inits so a `for` whose iterator xelim hoisted into a temp can
  # still be read (see `forRangeAssumes`).
  if n.isTagLit and n.exprKind in CallKinds:
    var whole = createTokenBuf(16)
    whole.addSubtree n
    c.callExprs[symId] = whole

  let callInfo = trBoundExpr(c, dest, n)
  n = localStart; skip n
  dest.addParRi()
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
    n = asgnStart; skip n
    dest.addParRi()
    callIsOver(c, dest, callInfo)
  else:
    var rhs = n
    skip rhs
    trExpr c, dest, n   # the destination location
    trExpr c, dest, rhs # then the value
    n = asgnStart; skip n
    dest.addParRi()

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
    dest.addParLe IteV, info
    trExpr c, dest, n            # condition (consumes the leaf)
    dest.addParLe StmtsS, info
    emitJmp dest, trueL, info
    dest.addParRi()             # close `stmts`
    dest.addDotToken()          # no else
    dest.addParRi()             # close `ite`
  elif trueL == fallL:
    # fall through when true; jump to `falseL` when false (negated guard).
    dest.addParLe IteV, info
    dest.addParLe NotX, info
    trExpr c, dest, n
    dest.addParRi()             # close `not`
    dest.addParLe StmtsS, info
    emitJmp dest, falseL, info
    dest.addParRi()
    dest.addDotToken()
    dest.addParRi()
  else:
    # neither edge falls through: conditional jump to `trueL`, then `jmp falseL`.
    dest.addParLe IteV, info
    trExpr c, dest, n
    dest.addParLe StmtsS, info
    emitJmp dest, trueL, info
    dest.addParRi()
    dest.addDotToken()
    dest.addParRi()
    emitJmp dest, falseL, info

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
    emitLab dest, z, n.info
    genCond c, dest, n, trueL, falseL, fallL # Cx(b)(t, f)
    n = scStart; skip n
  of OrX:
    let scStart = n
    n = sub(n)
    let z = freshLabel(c, "´cz.")
    genCond c, dest, n, trueL, z, z         # Cx(a)(t, z), a-false falls to z
    emitLab dest, z, n.info
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
  let hasElse = afterElif.substructureKind == ElseU
  let thenL = freshLabel(c, "´ct.")
  let endL = freshLabel(c, "´ce.")
  let elseL = if hasElse: freshLabel(c, "´cl.") else: endL
  let elifStart = n # skip `elif`
  n = sub(n)
  genCond c, dest, n, thenL, elseL, thenL
  emitLab dest, thenL, info
  trScopedBody c, dest, n      # then-branch
  n = elifStart; skip n        # end of `elif`
  if hasElse:
    emitJmp dest, endL, info
    emitLab dest, elseL, info
    let elseStart = n # skip `else`
    n = sub(n)
    trScopedBody c, dest, n    # else-branch
    n = elseStart; skip n      # end of `else`
  emitLab dest, endL, info
  n = ifStart; skip n          # end of `if`

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Precondition: xelim already produced a single elif-else construct here.
  let info = n.info
  let ifStart = n
  n = sub(n)
  assert n.substructureKind == ElifU
  var condPeek = n
  inc condPeek               # at the condition
  if condIsComplex(condPeek):
    # Short-circuit `and`/`or`: use the two-target condition compiler.
    genIfViaCx c, dest, n, info, ifStart
    return

  dest.addParLe IteV, info
  let elifStart = n # skip `elif`
  n = sub(n)
  trExpr c, dest, n          # condition
  trScopedBody c, dest, n    # then-branch
  n = elifStart; skip n      # end of `elif`

  if n.hasMore:
    assert n.substructureKind == ElseU
    let elseStart = n
    n = sub(n)
    trScopedBody c, dest, n  # else-branch
    n = elseStart; skip n    # end of `else`
  else:
    dest.addDotToken()       # no else section

  n = ifStart; skip n        # end of `if`
  dest.addParRi()            # close `ite`

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `case` is kept as a construct (doc/final_ir.md). We only translate the
  # selector and the branch bodies; `ranges` hold pure constants after xelim
  # and are copied verbatim.
  let info = n.info
  # Touch the selector type so typenav stays consistent across the switch.
  discard c.typeCache.getType(n.childCursor)
  dest.addParLe("case", info)
  let caseStart = n
  n = sub(n)
  trExpr c, dest, n          # selector
  while n.substructureKind == OfU:
    takeInto dest, n:        # `of`
      takeTree dest, n       # ranges (pure constants)
      trScopedBody c, dest, n # branch body
  if n.substructureKind == ElseU:
    takeInto dest, n:        # `else`
      trScopedBody c, dest, n
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
    # unnamed break: innermost enclosing loop *or* block
    target = c.current.exits.len - 1
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
  emitJmp dest, c.current.exits[target].name, info
  n = breakStart; skip n

proc trContinue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # The single backward transfer: the loop's back-edge to its header.
  let info = n.info
  dest.copyIntoKind ContinueV, info:
    dest.addDotToken() # no `join` information yet
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
  dest.addParRi()
  n = retStart; skip n

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
  dest.addParRi()
  n = raiseStart; skip n

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # A `block` has no Final IR construct of its own: it is `body (lab blockExit)`
  # where `break label` jumps forward to `blockExit`.
  let info = n.info
  let blockStart = n # `block`
  n = sub(n)
  let blockName = if n.isSymbolDef: n.symId else: NoSymId
  inc n # name or empty
  let exitL = freshLabel(c, "´blk.")
  c.current.exits.add Exit(name: exitL, blockName: blockName, isLoop: false)
  trScopedBody c, dest, n
  let used = c.current.exits[^1].used
  c.current.exits.shrink(c.current.exits.len - 1)
  n = blockStart; skip n # close `block`
  if used:
    emitLab dest, exitL, info

proc trLoopFromBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `n` points at the loop *body* (a `(stmts ...)`). Emit the infinite
  ## `(loop (stmts <body> (continue .)))` and, if any `break` targeted it, the
  ## trailing `(lab loopExit)`.
  let info = n.info
  let exitL = freshLabel(c, "´lx.")
  c.current.exits.add Exit(name: exitL, isLoop: true)
  openScope c
  dest.addParLe LoopV, info
  dest.addParLe StmtsS, info
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  n.into: # the body statement list
    while n.hasMore:
      trStmt c, dest, n
  closeScope c, dest, info # kills run on the back-edge path
  dest.copyIntoKind ContinueV, info: # the sole back-edge
    dest.addDotToken()
  dest.addParRi() # close `stmts`
  dest.addParRi() # close `loop`
  let used = c.current.exits[^1].used
  c.current.exits.shrink(c.current.exits.len - 1)
  if used:
    emitLab dest, exitL, info

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
  ## The loop variables of a `for`. They are bound by an iterator this pass
  ## cannot see, so nothing declares them for the type cache unless we do.
  var vars = vars
  if vars.substructureKind in {UnpackflatU, UnpacktupU}:
    vars = sub(vars) # peek only, never left
    while vars.hasMore:
      registerForVars c, vars
      skip vars
  elif isLocal(vars.symKind):
    let local = asLocal(vars)
    c.typeCache.registerLocal(local.name.symId, vars.symKind, local.typ)

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## A `for` survives lowering as a `for`, with only its body lowered.
  ##
  ## It used to become a bare `(loop body)` with the iterator call and the loop
  ## variables dropped on the floor, which is fine for an analysis that throws
  ## its input away and fatal for one whose output is compiled: hexer's
  ## `elimForLoops` runs long after this pass, and the call is the only record
  ## of what the loop iterates.
  ##
  ## The body is lowered exactly as a `loop`'s is — it ends in `(continue .)`,
  ## a `break` inside it is a forward `(jmp …)` to the trailing exit label — so
  ## a `for` is a loop construct that also says what it iterates.
  let info = n.info
  let forStmt = asForStmt(n) # peek at structure before advancing
  let forStart = n
  let forTag = n.cursorTagId
  n = sub(n)
  var assumeBuf = createTokenBuf(0)
  forRangeAssumes(assumeBuf, forStmt, c.callExprs, info)

  let exitL = freshLabel(c, "´lx.")
  c.current.exits.add Exit(name: exitL, isLoop: true)
  openScope c
  dest.addParLe(forTag, info)
  takeTree dest, n # the iterator call, verbatim: xelim already normalized it
  registerForVars c, n
  takeTree dest, n # the loop variables
  dest.addParLe StmtsS, info
  if assumeBuf.len > 0:
    dest.add assumeBuf
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  n.into: # the body statement list
    while n.hasMore:
      trStmt c, dest, n
  closeScope c, dest, info # kills run on the back-edge path
  dest.copyIntoKind ContinueV, info: # the sole back-edge
    dest.addDotToken()
  dest.addParRi() # close `stmts`
  dest.addParRi() # close `for`
  c.current.exits.shrink(c.current.exits.len - 1)
  # The exit label is emitted even when nothing jumps to it. An *inline*
  # iterator is not inlined until hexer's `elimForLoops`, so at this point no
  # `break` has been generated for the iterator's own termination test and the
  # loop reads as one nothing ever leaves — which would make everything after
  # it unreachable. That is not a harmless imprecision: a join on a dead path
  # keeps *both* arms of the next `if`, so `le >= 0` and `le < 0` come to hold
  # at once and a correct index gets "disproved". A `for` terminates; the label
  # says so.
  emitLab dest, exitL, info
  n = forStart; skip n # close `for`

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `try`/`except`/`fin` are kept as regions: a `fin` must run on *every* exit
  # crossing it, so it cannot be lowered to a `jmp` (doc/final_ir.md).
  let info = n.info
  dest.addParLe("try", info)
  let tryStart = n
  n = sub(n)
  trScopedBody c, dest, n # try body
  while n.substructureKind == ExceptU:
    takeInto dest, n: # `except`
      while n.hasMore:
        if n.stmtKind in {StmtsS, ScopeS}:
          trScopedBody c, dest, n # handler body
        elif isLocal(n.symKind):
          # the bound exception variable, e.g. `(let e ErrorCode .)`
          let local = asLocal(n)
          c.typeCache.registerLocal(local.name.symId, n.symKind, local.typ)
          takeTree dest, n
        else:
          takeTree dest, n # exception type pattern
  if n.substructureKind == FinU:
    takeInto dest, n: # `fin`
      trScopedBody c, dest, n
  dest.addParRi(n.endInfo) # close `try`
  n = tryStart; skip n

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1, returnType: r.retType)

  # An `{.assembler.}` body is machine code written by hand: it has no contracts
  # to check, and its constructs are outside the Final IR's vocabulary anyway (a
  # machine flag as an `if` condition is not an expression). `xelim` leaves such
  # a body verbatim — source order is the contract — so it never arrives in the
  # normalized form this pass assumes, and it is passed through verbatim here
  # for the same reason. It used to be replaced by a bodyless declaration, which
  # was only ever safe while this pass's output was thrown away: *not analysed*
  # is not the same as *not emitted*. The prover skips it (`traverseProc`).
  let isAsm = hasPragma(r.pragmas, AssemblerP)
  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isAsm:
      takeTree dest, n
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
  of StmtsS, ScopeS:
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
  of LocalDecls:
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
  of LabS, JmpS:
    # Already Final IR on arrival: `xelim` lowers short-circuit `and`/`or`
    # chains to the flat `(if c (jmp L))` / `(lab L)` form, so these are the
    # pass's own vocabulary coming back at it. They are also the two most
    # frequent tags to reach here at all (163 `jmp` and 122 `lab` across the
    # `tjson` closure), which is why they get a branch rather than the
    # operand-lowering fallback below.
    takeTree dest, n
  of ImportS, ImportasS, FromimportS, ImportexceptS, IncludeS, ExportS,
     ExportexceptS, CommentS, PragmasS:
    # Module bookkeeping, not code. Nothing to lower, and the backend and the
    # index both want them where they are.
    takeTree dest, n
  of YldS, DiscardS, InclS, ExclS, EmitS:
    # A statement whose children are plain expressions: keep the statement,
    # lower the operands. `trExpr` routes a call operand through `trCall`, so an
    # lvalue call (`s[i]`) in one of these is handled.
    trExpr c, dest, n
  else:
    if n.finalIrKind in {MflagV, VflagV}:
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
    elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX, TraceX}:
      # A hook call in statement position. Its children are locations, so
      # lowering them as expressions is exactly right — `contracts_fir` treats
      # the same set the same way.
      trExpr c, dest, n
    else:
      # Operand-lowering fallback: the statement's shape is kept and its
      # children are lowered as expressions. That is right for the hook calls
      # (`(destroy x)` and friends) and for anything else whose children are
      # plain expressions — but it does NOT lower nested *statements*, so a
      # construct with a body reaching here would keep an un-lowered body.
      # Nothing produces such a construct in this pass's input today:
      # `corofor` is the one that would, and its only producer is hexer's
      # `iterinliner`, which runs downstream of every caller of this pass. That
      # stops being true when the lowering moves into hexer's own pipeline, so
      # measure before relying on it.
      #
      # `-d:firFallbackProbe` prints one line per statement that lands here,
      # which is how the list above was established (the spelling matches
      # `-d:contractStats` in `contracts_fir.nim`).
      when defined(firFallbackProbe):
        stderr.writeLine "FIR-FALLBACK " & globalTags.tags[n.cursorTagId]
      trExpr c, dest, n

proc toFinalIr*(pass: var Pass) =
  var c = Context(counter: 0, typeCache: createTypeCache(pass.bits),
                  thisModuleSuffix: pass.moduleSuffix)
  c.openScope()
  lowerExprs(pass, TowardsFinalIr)
  pass.prepareForNext("finalir")
  var n = pass.n
  assert n.stmtKind == StmtsS, $n.kind
  pass.dest.addParLe(n.cursorTagId, n.info)
  c.openScope()
  n.into:
    while n.hasMore:
      trStmt c, pass.dest, n
    closeScope c, pass.dest, n.endInfo
  pass.dest.addParRi()

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

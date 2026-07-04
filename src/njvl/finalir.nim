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
import njvl_model

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
    callFirstArgs: Table[SymId, TokenBuf] ## first argument of a local's init call (for for-loop borrow tracking)

proc addParLe*(dest: var TokenBuf; kind: NjvlKind; info = NoLineInfo) =
  dest.add parLeToken(cast[TagId](kind), info)

proc openScope(c: var Context) =
  c.typeCache.openScope()

proc closeScope(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  # insert kill instructions for the locals that go out of scope:
  var i = 0
  for s in c.typeCache.currentScopeLocals:
    if i == 0:
      dest.add tagToken("kill", info)
    dest.addSymUse s, info
    inc i
  if i > 0:
    dest.addParRi()
  c.typeCache.closeScope()

proc freshLabel(c: var Context; prefix: string): SymId =
  result = pool.syms.getOrIncl(prefix & $c.current.tmpCounter)
  inc c.current.tmpCounter

proc emitLab(dest: var TokenBuf; name: SymId; info: PackedLineInfo) =
  dest.addParLe LabV, info
  dest.addSymDef name, info
  dest.addParRi()

proc emitJmp(dest: var TokenBuf; name: SymId; info: PackedLineInfo) =
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
    info: PackedLineInfo

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo

proc trStmtsInline(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Translate the children of a `(stmts ...)`/`(scope ...)` *without* emitting
  ## a wrapping node. The caller already opened the enclosing list.
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  inc n
  while n.hasMore:
    trStmt c, dest, n
  skipParRi n

proc trScopedBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Translate a body that opens its own lexical scope and emit it as a fresh
  ## `(stmts ...)`. Scope-exit `kill`s are appended before the closing paren.
  let info = n.info
  openScope c
  dest.addParLe StmtsS, info
  if n.stmtKind in {StmtsS, ScopeS}:
    inc n
    while n.hasMore:
      trStmt c, dest, n
    closeScope c, dest, info
    dest.addParRi()
    skipParRi n
  else:
    trStmt c, dest, n
    closeScope c, dest, info
    dest.addParRi()

# ------------------------------------------------------------------ expressions

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    dest.takeToken n
  of UnknownToken, EofToken, DotToken, Ident, SymbolDef, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParLe:
    case n.exprKind
    of CallKinds:
      bug "call must have been bound to a location"
    of AndX, OrX:
      bug "and/or should have been handled by the expression elimination pass xelim.nim"
    else:
      dest.takeToken n
      while n.hasMore:
        if n.kind == ParLe and n.exprKind in CallKinds:
          # A call surviving to the Final IR in an operand position is an
          # lvalue location (xelim lifts every value-producing call), e.g. the
          # `s[i]` argument of a location-taking builtin like `=wasMoved`.
          # Emit it via `trCall`, which accepts a call here, rather than the
          # plain-expression path that rejects one. (Matches nj.nim / #2081.)
          discard trCall(c, dest, n)
        else:
          trExpr c, dest, n
      dest.takeToken n
  of ParRi: bug "Unmatched ParRi"

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo =
  let info = n.info
  result = CallInfo(info: info)
  dest.add n
  inc n # skip `(call)`
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
  dest.takeParRi n

proc callIsOver(c: var Context; dest: var TokenBuf; callInfo: CallInfo) =
  # `unknown` marks that a `(haddr …)` argument's pointee may have been mutated.
  # This is cleaned up by the later alias/versionizer pass.
  for path in callInfo.mutates:
    dest.add tagToken("unknown", callInfo.info)
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
    result = CallInfo(mutates: @[], info: n.info)

# ------------------------------------------------------------------- statements

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let callInfo = trCall(c, dest, n)
  callIsOver(c, dest, callInfo)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  dest.takeToken n

  let symId = n.symId
  if kind == ResultY:
    c.current.resultSym = symId

  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.typeCache.registerLocal(symId, kind, n)
  takeTree dest, n # type

  # Record first argument of call inits for borrow tracking (used by trFor):
  if n.kind == ParLe and n.exprKind in CallKinds:
    var tmp = n
    inc tmp # skip call tag
    skip tmp # skip callee
    if tmp.hasMore: # has at least one argument
      var argBuf = createTokenBuf(8)
      argBuf.addSubtree tmp
      c.callFirstArgs[symId] = argBuf

  let callInfo = trBoundExpr(c, dest, n)
  skipParRi n
  dest.addParRi()
  callIsOver(c, dest, callInfo)

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `(asgn X Y)` becomes `(store Y X)`: storing the value first reflects the
  # actual evaluation order and is easier to analyze.
  let info = n.info
  dest.addParLe StoreV, info
  inc n
  if n.kind == Symbol:
    let symId = n.symId
    inc n # skip the destination symbol
    if n.exprKind in CallKinds:
      # `dest = f(args)`: the call binds directly to its destination, no temp.
      let callInfo = trBoundExpr(c, dest, n)
      dest.addSymUse symId, info
      skipParRi n
      dest.addParRi()
      callIsOver(c, dest, callInfo)
      return
    else:
      trExpr c, dest, n
    # add the destination later due to the reversed `(store value dest)` order:
    dest.addSymUse symId, info
  else:
    var rhs = n
    skip rhs
    trExpr c, dest, rhs # value first
    trExpr c, dest, n   # then the destination location
    n = rhs
  skipParRi n
  dest.addParRi()

proc condIsComplex(n: Cursor): bool =
  ## A condition needs the two-target compiler (`Cx`) iff it contains a
  ## short-circuit `and`/`or`. A plain leaf (incl. a lone `not`) stays a single
  ## `ite` whose condition the tracker negates directly.
  var n = n
  if n.kind != ParLe: return false
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      if n.exprKind in {AndX, OrX}: return true
      inc nested
    of ParRi: dec nested
    else: discard
    inc n
    if nested == 0: break
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
    inc n
    let z = freshLabel(c, "´cz.")
    genCond c, dest, n, z, falseL, z        # Cx(a)(z, f), a-true falls to z
    emitLab dest, z, n.info
    genCond c, dest, n, trueL, falseL, fallL # Cx(b)(t, f)
    skipParRi n
  of OrX:
    inc n
    let z = freshLabel(c, "´cz.")
    genCond c, dest, n, trueL, z, z         # Cx(a)(t, z), a-false falls to z
    emitLab dest, z, n.info
    genCond c, dest, n, trueL, falseL, fallL # Cx(b)(t, f)
    skipParRi n
  of NotX:
    inc n
    genCond c, dest, n, falseL, trueL, fallL # swap targets
    skipParRi n
  else:
    genLeaf c, dest, n, trueL, falseL, fallL

proc genIfViaCx(c: var Context; dest: var TokenBuf; n: var Cursor;
                info: PackedLineInfo) =
  ## `if <complex cond>: then [else: else]` via `Cx`, so short-circuit
  ## `and`/`or` lower to shared `(lab)`/`(jmp)` merges. `n` is at the `(elif`.
  var afterElif = n
  skip afterElif
  let hasElse = afterElif.substructureKind == ElseU
  let thenL = freshLabel(c, "´ct.")
  let endL = freshLabel(c, "´ce.")
  let elseL = if hasElse: freshLabel(c, "´cl.") else: endL
  inc n                        # skip `elif`
  genCond c, dest, n, thenL, elseL, thenL
  emitLab dest, thenL, info
  trScopedBody c, dest, n      # then-branch
  skipParRi n                  # end of `elif`
  if hasElse:
    emitJmp dest, endL, info
    emitLab dest, elseL, info
    inc n                      # skip `else`
    trScopedBody c, dest, n    # else-branch
    skipParRi n                # end of `else`
  emitLab dest, endL, info
  skipParRi n                  # end of `if`

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Precondition: xelim already produced a single elif-else construct here.
  let info = n.info
  inc n
  assert n.substructureKind == ElifU
  var condPeek = n
  inc condPeek               # at the condition
  if condIsComplex(condPeek):
    # Short-circuit `and`/`or`: use the two-target condition compiler.
    genIfViaCx c, dest, n, info
    return

  dest.addParLe IteV, info
  inc n                      # skip `elif`
  trExpr c, dest, n          # condition
  trScopedBody c, dest, n    # then-branch
  skipParRi n                # end of `elif`

  if n.hasMore:
    assert n.substructureKind == ElseU
    inc n
    trScopedBody c, dest, n  # else-branch
    skipParRi n              # end of `else`
  else:
    dest.addDotToken()       # no else section

  skipParRi n                # end of `if`
  dest.addParRi()            # close `ite`

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `case` is kept as a construct (doc/final_ir.md). We only translate the
  # selector and the branch bodies; `ranges` hold pure constants after xelim
  # and are copied verbatim.
  let info = n.info
  # Touch the selector type so typenav stays consistent across the switch.
  discard c.typeCache.getType(n.firstSon)
  dest.add tagToken("case", info)
  inc n
  trExpr c, dest, n          # selector
  while n.substructureKind == OfU:
    dest.add n               # `of`
    inc n
    takeTree dest, n         # ranges (pure constants)
    trScopedBody c, dest, n  # branch body
    dest.takeParRi n         # close `of`
  if n.substructureKind == ElseU:
    dest.add n               # `else`
    inc n
    trScopedBody c, dest, n
    dest.takeParRi n         # close `else`
  dest.takeParRi n           # close `case`

proc trBreak(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `break` is a forward `(jmp loopExit)` to the matching construct's exit.
  let info = n.info
  inc n
  var target = -1
  if n.kind == ParRi or n.kind == DotToken:
    if n.kind == DotToken: inc n
    # unnamed break: innermost enclosing loop *or* block
    target = c.current.exits.len - 1
  elif n.kind == Symbol:
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
  skipParRi n

proc trContinue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # The single backward transfer: the loop's back-edge to its header.
  let info = n.info
  dest.copyIntoKind ContinueV, info:
    dest.addDotToken() # no `join` information yet
  skip n

proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `return` stays primitive: it carries the result and runs the epilogue.
  let info = n.info
  inc n
  dest.add tagToken("ret", info)
  if n.kind == ParRi:
    dest.addDotToken()
  elif n.kind == DotToken:
    dest.addDotToken()
    inc n
  elif n.kind == Symbol and n.symId == c.current.resultSym:
    dest.addSymUse n.symId, info
    inc n
  else:
    trExpr c, dest, n
  dest.addParRi()
  skipParRi n

proc trRaise(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `raise` stays primitive: it is type-dispatched by `except` and crosses
  # `finally`. A bare `(raise .)` re-raises the in-flight exception.
  let info = n.info
  inc n
  dest.add tagToken("raise", info)
  if n.kind == ParRi:
    dest.addDotToken()
  elif n.kind == DotToken:
    dest.addDotToken()
    inc n
  else:
    trExpr c, dest, n
  dest.addParRi()
  skipParRi n

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # A `block` has no Final IR construct of its own: it is `body (lab blockExit)`
  # where `break label` jumps forward to `blockExit`.
  let info = n.info
  inc n # `block`
  let blockName = if n.kind == SymbolDef: n.symId else: NoSymId
  inc n # name or empty
  let exitL = freshLabel(c, "´blk.")
  c.current.exits.add Exit(name: exitL, blockName: blockName, isLoop: false)
  trScopedBody c, dest, n
  let used = c.current.exits[^1].used
  c.current.exits.shrink(c.current.exits.len - 1)
  skipParRi n # close `block`
  if used:
    emitLab dest, exitL, info

proc trLoopFromBody(c: var Context; dest: var TokenBuf; n: var Cursor;
                    forBorrow: TokenBuf) =
  ## `n` points at the loop *body* (a `(stmts ...)`). Emit the infinite
  ## `(loop (stmts <body> (continue .)))` and, if any `break` targeted it, the
  ## trailing `(lab loopExit)`.
  let info = n.info
  let exitL = freshLabel(c, "´lx.")
  c.current.exits.add Exit(name: exitL, isLoop: true)
  openScope c
  dest.addParLe LoopV, info
  dest.addParLe StmtsS, info
  if forBorrow.len > 0:
    dest.add forBorrow
  assert n.stmtKind in {StmtsS, ScopeS}, $n.kind
  inc n # into the body statement list
  while n.hasMore:
    trStmt c, dest, n
  skipParRi n # end of body statement list
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
  inc n # `while`
  let empty = createTokenBuf(0)
  if n.exprKind == TrueX:
    # `while true` is already the canonical infinite loop.
    inc n
    skipParRi n
    trLoopFromBody c, dest, n, empty
    skipParRi n # close `while`
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
    skipParRi n # close `while`
    var ww = beginRead(w)
    trLoopFromBody c, dest, ww, empty
    endRead w

proc addForBorrowDecls(dest: var TokenBuf; vars: Cursor; firstArgBuf: TokenBuf) =
  var vars = vars
  if vars.substructureKind in {UnpackflatU, UnpacktupU}:
    inc vars
    while vars.hasMore:
      addForBorrowDecls dest, vars, firstArgBuf
      skip vars
  elif isLocal(vars.symKind):
    let local = asLocal(vars)
    if local.typ.typeKind in {MutT, LentT}:
      var localDecl = vars
      dest.addParLe(if local.typ.typeKind == MutT: VarS else: LetS, vars.info)
      inc localDecl # skip original local-decl tag
      takeTree dest, localDecl # name
      takeTree dest, localDecl # export marker
      takeTree dest, localDecl # pragmas
      takeTree dest, localDecl # type
      dest.addParLe HaddrX, vars.info
      dest.add firstArgBuf
      dest.addParRi()
      dest.addParRi()

proc extractForBorrow(c: var Context; forStmt: ForStmt; info: PackedLineInfo): TokenBuf =
  ## If the for-loop iterates with a borrowing iterator (yields `var T`/`lent T`),
  ## initialize the corresponding loop variables with a fake `(haddr firstArg)`
  ## so contract analysis treats the loop binders as borrowers.
  result = createTokenBuf(0)

  var firstArgBuf = createTokenBuf(0)
  var iterCall = forStmt.iter
  if iterCall.kind == ParLe and iterCall.exprKind in CallKinds:
    inc iterCall
    skip iterCall
    if iterCall.hasMore:
      firstArgBuf = createTokenBuf(8)
      firstArgBuf.addSubtree iterCall
  elif iterCall.kind == ParLe and iterCall.exprKind in {HderefX, HaddrX}:
    inc iterCall
    if iterCall.kind == Symbol:
      let tempSym = iterCall.symId
      if tempSym in c.callFirstArgs:
        firstArgBuf = createTokenBuf(8)
        firstArgBuf.addSubtree beginRead(c.callFirstArgs.getOrQuit(tempSym))
  elif iterCall.kind == Symbol:
    let tempSym = iterCall.symId
    if tempSym in c.callFirstArgs:
      firstArgBuf = createTokenBuf(8)
      firstArgBuf.addSubtree beginRead(c.callFirstArgs.getOrQuit(tempSym))

  if firstArgBuf.len == 0:
    return

  addForBorrowDecls result, forStmt.vars, firstArgBuf

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # After xelim the iterator advance/done-check already sits as a leading
  # `if done: break` inside the body, so a `for` is just a `loop` whose body
  # binds the loop variable(s).
  let info = n.info
  let forStmt = asForStmt(n) # peek at structure before advancing
  inc n
  let borrowBuf = extractForBorrow(c, forStmt, info)
  skip n # for loop iterator call
  skip n # for loop variables
  trLoopFromBody c, dest, n, borrowBuf
  skipParRi n # close `for`

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `try`/`except`/`fin` are kept as regions: a `fin` must run on *every* exit
  # crossing it, so it cannot be lowered to a `jmp` (doc/final_ir.md).
  let info = n.info
  dest.add tagToken("try", info)
  inc n
  trScopedBody c, dest, n # try body
  while n.substructureKind == ExceptU:
    dest.add n # `except`
    inc n
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
    dest.takeParRi n # close `except`
  if n.substructureKind == FinU:
    dest.add n # `fin`
    inc n
    trScopedBody c, dest, n
    dest.takeParRi n # close `fin`
  dest.takeParRi n # close `try`

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1, returnType: r.retType)

  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
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

proc trCfVarDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # xelim can produce cfvars that we pass through (the bool storage they need
  # is still valid Final IR; we do not generate any ourselves).
  dest.takeToken n # MflagV/VflagV
  let s = n.symId
  dest.takeToken n # SymDef
  dest.takeParRi n # ParRi
  let boolTyp = c.typeCache.builtins.boolType
  c.typeCache.registerLocal(s, VarY, boolTyp)

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
  else:
    if n.njvlKind in {MflagV, VflagV}:
      trCfVarDecl c, dest, n
    elif n.exprKind == PragmaxX:
      copyInto(dest, n):
        takeTree dest, n  # pragmas
        trStmt c, dest, n # body
    elif n.exprKind == ProccallX:
      trStmtCall c, dest, n
    else:
      trExpr c, dest, n

proc toFinalIr*(pass: var Pass) =
  var c = Context(counter: 0, typeCache: createTypeCache(),
                  thisModuleSuffix: pass.moduleSuffix)
  c.openScope()
  lowerExprs(pass, TowardsFinalIr)
  pass.prepareForNext("finalir")
  var n = pass.n
  assert n.stmtKind == StmtsS, $n.kind
  pass.dest.add n
  inc n
  c.openScope()
  while n.hasMore:
    trStmt c, pass.dest, n
  closeScope c, pass.dest, n.info
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
  var pass = initPass(move initialBuf, "main", "xelim_finalir", 0)
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

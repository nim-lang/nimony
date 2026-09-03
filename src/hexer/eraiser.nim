#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Exception lowering. Nimony's exceptions are checked returns, and this is the
one pass that implements them.

- `proc p(params): T {.raises.}` returns `(ErrorCode, T)` — or a bare
  `ErrorCode` when `T` is void.
- A raising call becomes a temp plus a check: `var tmp = rcall(args)` and
  `if tmp[0] != Success: raise (tmp[0], result)`. The expression the call
  stood for becomes `tmp[1]`.
- `let/var local = rcall(args)` retypes `local` to the success tuple and gets
  that same check; every other use of `local` projects onto `local[1]`.
- `result = x` and `return x` build the tuple. `result` itself already IS the
  tuple, so `return result` needs no rebuild — and must not get one, see
  `trRet`.
- `raise e` becomes `raise (e, result)`.

**Doing the whole job here is the point.** The control-flow half (the temps
and their checks) and the value half (the types) used to be two passes with
the destroyer wedged between them, on the grounds that introducing
`(ErrorCode, T)` early makes the lifter synthesise a hook per success tuple
that only delegates to `T`'s. Those hooks are cheap now and the inliner prunes
them, and keeping the halves apart cost more than it saved: `cps` had grown
its own copy of the value half's knowledge, because it lifts a local that
outlives a state into the coroutine frame, and after that there is no
declaration left to retype and no symbol left to project.

Downstream consequences of it all happening here:

- The destroyer sees a tuple-typed temp like any other local, and `cps` sees a
  coroutine that happens to return a tuple. Neither has to know about this
  pass.
- A pass that runs LATER and still needs to signal an error must emit the
  finished form, because nothing lowers raises after this point. The
  duplifier's out-of-memory check is the only such case; it goes through
  `builtintypes.addRaisedCode`.

`try` is lowered here too, to the flat `lab`/`jmp` form (see `trTry`), and a
`finally` is replicated onto every way out — the raises, the `break`s and the
`return`s that leave it. **No `try` and no catchable `raise` survives this
pass.** What that removed elsewhere:

- The destroyer no longer knows what a `try` is. It kept two `ScopeKind`s, a
  `finallySection` per scope and a fresh-variable cloner purely to replicate
  `finally` bodies; a `raise` is now an ordinary routine exit, and a `jmp` to a
  handler unwinds through the same scope walk a `break` always did.
- `lengcgen` no longer has an `except` label stack. Its `trTry` handles only
  the handler-less `try`/`finally` that `cps` builds for the `corofor`
  trampoline, and its `trRaise` only ever emits a `return`.

The one `raise` that still reaches codegen is the kind a LATER pass emits for
an unrecoverable failure — the duplifier's out-of-memory check — which no
handler catches and which builds its payload with
`builtintypes.addRaisedCode`.
]##

import std / [sets, tables, hashes, assertions, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops,
                        builtintypes, reporters]
import ".." / models / tags
import passes
include ".." / nimony / nif_annotations

type
  Context = object
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    hoisted: TokenBuf
      ## Statements that must run *before* the statement currently being
      ## emitted: the `canRaise` temp of a raising call plus its check.
      ## Emitting them here — instead of wrapping them into an
      ## `(expr (stmts ...) tmp)` in expression position — is what keeps this
      ## pass's output statement-based, so no follow-up `xelim` run is needed
      ## to flatten it again (see `doc/final_ir.md`).
    tupleVars: HashSet[SymId]
      ## Locals whose type this pass widened to a success tuple, so a plain
      ## use of one has to project onto its value half. Filled as the
      ## declarations go by: a local is only ever used after it is declared,
      ## so a single forward pass sees every declaration before every use.
    canRaise: bool
      ## Does the ROUTINE being translated raise? Decides whether `result` and
      ## the returns carry a tuple.
    resultSym: SymId
    retType: Cursor
      ## The routine's source-level return type, i.e. before
      ## `addSuccessTupleType` is applied to it.
    exits: seq[ExitScope]
      ## The constructs a `raise`/`break`/`return` has to unwind through,
      ## innermost last. See `ExitScope`.

  ExitKind = enum
    TryExit     ## the BODY of a `try` (never its handler or its finally:
                ## those are translated with the frame already popped, so a
                ## raise in them propagates past this `try`, as it must)
    BlockExit   ## a `block`/`while`/`corofor` that a `break` can target

  ExitScope = object
    kind: ExitKind
    label: SymId
      ## `BlockExit`: the block's label, `NoSymId` for an anonymous one.
      ## `TryExit`: the handler label — `NoSymId` when the `try` has no
      ## `except` arm and therefore catches nothing.
    exceptVar: SymId
      ## `except E as e` binding of this `try`, if it has one. The frame that
      ## CATCHES is the one that gets the code, which is why this lives on the
      ## frame instead of on a separate stack: an inner `except:` with no
      ## binding must not hand the value to an outer `except e:`.
    fin: Cursor
      ## This `try`'s `(fin ...)` body, to replicate on every way out.

proc hoistTail(c: var Context; dest: var TokenBuf; pos: int) =
  ## Move `dest[pos ..< ^0]` — a sequence of complete statements — in front of
  ## the statement currently being translated.
  if dest.len <= pos: return
  var tail = cursorAt(dest, pos)
  while tail.hasMore:
    takeTree c.hoisted, tail
  endRead tail
  dest.shrink pos

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
    {.ensuresNif: addedAny(dest).}

proc callCanRaise*(typeCache: var TypeCache; n: Cursor): bool =
  var fnType = skipProcTypeToParams(getType(typeCache, n.childCursor))
  if fnType.tagEnum != ParamsTagId:
    raiseAssert "BUG eraiser callCanRaise: callee type not params at " & infoToStr(n.info) &
         ": " & toString(getType(typeCache, n.childCursor), false)
  skip fnType # params
  skip fnType # return type
  # now pragmas follow:
  result = hasPragma(fnType, RaisesP)

# -------------------- the success tuple ------------------------------------

proc produceSuccessTuple(c: var Context; dest: var TokenBuf; typ: Cursor;
                         info: NifLineInfo): bool =
  ## Open `(tupconstr (tuple ErrorCode T) Success` — the caller appends the
  ## value and closes. Answers whether anything was opened at all: a routine
  ## that returns nothing has only the code to hand back.
  if isVoidType(typ):
    result = false
  else:
    dest.addParLe TupconstrX, info
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree typ
    dest.addParRi()
    dest.addSymUse pool.syms.getOrIncl(SuccessName), info
    result = true

proc produceRaiseTuple(c: var Context; dest: var TokenBuf; typ: Cursor; info: NifLineInfo) =
  ## As `produceSuccessTuple`, but the code is the caller's to append too.
  ## Pair with `finishRaiseTuple`.
  if not isVoidType(c.retType):
    dest.addParLe TupconstrX, info
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree typ
    dest.addParRi()

proc finishRaiseTuple(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## Close a `produceRaiseTuple` by pairing the code with whatever the result
  ## slot holds so far.
  if not isVoidType(c.retType):
    if c.resultSym != SymId(0):
      copyIntoKind dest, TupatX, info:
        dest.addSymUse c.resultSym, info
        dest.addIntLit 1, info
    dest.addParRi()

proc addErrorCodeOf(c: var Context; dest: var TokenBuf; target: SymId;
                    isVoidCall: bool; info: NifLineInfo) =
  ## The error code sitting in a raising call's temp. A void call's temp holds
  ## only the code, so it IS the code.
  if isVoidCall:
    dest.addSymUse target, info
  else:
    copyIntoKind dest, TupatX, info:
      dest.addSymUse target, info
      dest.addIntLit 0, info

# -------------------- unwinding --------------------------------------------

proc freshVars(n: var Cursor; newVars: var Table[SymId, SymId]; idgen: var int;
               dest: var TokenBuf) =
  ## Copy a subtree, renaming every local it DECLARES. A `finally` body is
  ## replicated once per exit, and two copies in one routine cannot share
  ## declarations.
  case n.kind
  of Symbol:
    let repl = newVars.getOrDefault(n.symId, n.symId)
    dest.addSymUse(repl, n.info)
    inc n
  of TagLit:
    let isLocalDecl = n.stmtKind in {VarS, LetS, CursorS, PatternvarS}
    copyInto dest, n:
      if isLocalDecl and n.isSymbolDef:
        let repl = pool.syms.getOrIncl("`ffv." & $idgen)
        newVars[n.symId] = repl
        dest.addSymDef(repl, n.info)
        inc idgen
        inc n
      while n.hasMore:
        freshVars(n, newVars, idgen, dest)
  of UIntLit, StrLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken,
     EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident:
    dest.addSubtree n
    inc n
  else:
    raiseAssert "BUG: unexpected ParRi in eraiser.freshVars" # classic ParRi only

proc freshLabel(c: var Context; prefix: string): SymId =
  result = pool.syms.getOrIncl(prefix & $c.tmpCounter)
  inc c.tmpCounter

proc emitFinCopy(c: var Context; dest: var TokenBuf; fin: Cursor) =
  ## Emit one copy of a `finally` body, its locals renamed. Every exit out of
  ## a `try` gets its own copy — that is what "the finally runs on every path"
  ## means once the paths are explicit jumps.
  if cursorIsNil(fin): return
  var copied = createTokenBuf(30)
  var newVars = initTable[SymId, SymId]()
  var src = fin
  # `tmpCounter` rather than a counter per copy: two replications in one
  # routine must not both call their first local `ffv.0`.
  freshVars(src, newVars, c.tmpCounter, copied)
  var n = beginRead(copied)
  tr c, dest, n
  endRead n

proc replicateFin(c: var Context; dest: var TokenBuf; idx: int) =
  ## `emitFinCopy` for an exit scope, with its `fin` DETACHED for the
  ## duration: a `raise` inside the copy unwinds this very stack, and would
  ## otherwise replicate the same body forever.
  if cursorIsNil(c.exits[idx].fin): return
  let saved = c.exits[idx].fin
  c.exits[idx].fin = default(Cursor)
  emitFinCopy c, dest, saved
  c.exits[idx].fin = saved

proc emitFinsDownTo(c: var Context; dest: var TokenBuf; stopAt: int) =
  ## Replicate the `finally` of every exit scope from the innermost down to
  ## `stopAt`, in the order they are left.
  var i = c.exits.len - 1
  while i >= stopAt:
    replicateFin c, dest, i
    dec i

proc catchingFrame(c: Context): int =
  ## Index of the `try` whose handler catches a raise thrown here, or -1 when
  ## nothing does and the raise leaves the routine.
  result = -1
  var i = c.exits.len - 1
  while i >= 0:
    if c.exits[i].kind == TryExit and c.exits[i].label != NoSymId:
      return i
    dec i

proc emitUnwind(c: var Context; dest: var TokenBuf; code: TokenBuf;
                info: NifLineInfo) =
  ## The whole way out for one raise: hand the code to the catching handler's
  ## binding if it has one, run every `finally` being left, then jump — to the
  ## handler, or clean out of the routine.
  let target = catchingFrame(c)
  if target >= 0 and c.exits[target].exceptVar != NoSymId:
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse c.exits[target].exceptVar, info
      dest.add code
  emitFinsDownTo c, dest, (if target >= 0: target + 1 else: 0)
  if target >= 0:
    copyIntoKind dest, JmpS, info:
      dest.addSymUse c.exits[target].label, info
  else:
    copyIntoKind dest, RetS, info:
      produceRaiseTuple c, dest, c.retType, info
      dest.add code
      finishRaiseTuple c, dest, info

proc addPropagationCheck(c: var Context; dest: var TokenBuf; target: SymId;
                         isVoidCall: bool; info: NifLineInfo) =
  ## `if tmp[0] != Success: <unwind with tmp[0]>` — a callee's error
  ## travelling on.
  var code = createTokenBuf(8)
  addErrorCodeOf c, code, target, isVoidCall, info
  copyIntoKind dest, IfS, info:
    copyIntoKind dest, ElifU, info:
      addErrorCodeOf c, dest, target, isVoidCall, info
      copyIntoKind dest, StmtsS, info:
        emitUnwind c, dest, code, info

# -------------------- declarations -----------------------------------------

proc takeRaisingHeader(c: var Context; dest: var TokenBuf; decl: Cursor;
                       n: var Cursor): bool =
  ## `typenav.takeRoutineHeader`, plus the signature half of this pass: a
  ## `.raises` routine RETURNS its success tuple.
  ##
  ## The `(raises)` pragma stays: `lengcgen` reads it to decide that a raising
  ## `.noreturn` proc must NOT get C's `noreturn` attribute, because under goto
  ## exceptions such a proc does return — it hands back an error code — and
  ## telling gcc otherwise deletes the callers' error paths. What `lengcgen`
  ## no longer does from the pragma is rewrite THIS return type; it now does
  ## that for proctypes only.
  ##
  ## This used to happen in `lengcgen`, at the very end, which was fine while
  ## nothing between here and there had to know a raising routine's real shape.
  ## `cps` does: it builds a coroutine's frame and its result slot out of the
  ## return type, and it cannot wait for codegen to tell it what that is.
  result = true # assume concrete
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.typeCache.registerParams(sym, decl, n)
      takeTree dest, n
    elif i == TypevarsPos:
      result = n.substructureKind != TypevarsU
      takeTree dest, n
    elif i == ReturnTypePos:
      addLengReturnType(dest, n, asRoutine(decl, SkipExclBody).pragmas, n.info)
      skip n
    else:
      takeTree dest, n

proc takeLocalHeader(c: var TypeCache; dest: var TokenBuf; n: var Cursor;
                     kind: SymKind; isTuple: bool) =
  ## The local is REGISTERED with its source-level type even when the emitted
  ## declaration widens to a tuple: every use of it is projected onto the value
  ## half, so the source-level type is what a type query about a use must
  ## answer.
  let name = n.symId
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.registerLocal(name, kind, n)
  if isVoidType(n) and isTuple:
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    skip n
  else:
    if isTuple:
      dest.addParLe TupleT, n.info
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    takeTree dest, n # type
    if isTuple:
      dest.addParRi()

# -------------------- calls ------------------------------------------------

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; targetIsTuple: bool) =
  ## `targetIsTuple` says the destination this call's value flows into already
  ## HAS the success-tuple shape — a retyped local, or `result`. Then the
  ## call's result travels there whole and needs neither a temp nor a wrapper.
  let head = n.load()
  let info = n.info
  let callStart = n
  n = sub(n)
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  if fnType.tagEnum != ParamsTagId:
    raiseAssert "BUG eraiser trCall: callee type not params at " & infoToStr(info) &
         ": " & toString(getType(c.typeCache, n), false)
  skip fnType # params
  let retType = fnType
  skip fnType # return type
  # now pragmas follow:
  let canRaise = hasPragma(fnType, RaisesP)

  if canRaise and not targetIsTuple:
    let isVoid = retType.isDotToken or retType.typeKind == VoidT
    let hoistPos = dest.len
    # A void raising call is already in statement position, so its temp and
    # check stay put, wrapped in a `(stmts ...)`. A value-returning one is an
    # operand: its statements are hoisted in front of the enclosing statement
    # and only the value projection is left behind.
    if isVoid:
      dest.addParLe(StmtsS, info)
    block:
      let symId = pool.syms.getOrIncl("`canRaise." & $c.tmpCounter)
      inc c.tmpCounter
      # The temp holds the call's RETURN VALUE — an owned, freshly
      # constructed value — so it must be an owning local. As a CursorS the
      # duplifier could only `=dup` out of it (a cursor is a borrow) and the
      # destroyer never released it: every `dest = raisingCall()` leaked the
      # entire returned value once per call.
      copyIntoKind dest, VarS, info:
        addSymDef dest, symId, info
        dest.addEmpty2 info # export marker, pragma
        addSuccessTupleType dest, retType, info
        # value is the call expression:
        dest.addParLe(head.tagId, info)
        while n.hasMore:
          tr c, dest, n
        dest.addParRi(n.endInfo)
        n = callStart; skip n, SkipFull
      addPropagationCheck(c, dest, symId, isVoid, info)
      if isVoid:
        dest.addParRi()
      else:
        hoistTail(c, dest, hoistPos)
        copyIntoKind dest, TupatX, info:
          dest.addSymUse symId, info
          dest.addIntLit 1, info
  else:
    # A NON-raising call feeding a tuple-shaped destination is the mirror
    # case: the value is fine, it just has to arrive wrapped.
    var needsTuple = targetIsTuple and not canRaise
    if needsTuple:
      needsTuple = produceSuccessTuple(c, dest, retType, info)
    dest.addParLe(head.tagId, info)
    while n.hasMore:
      tr c, dest, n
    dest.addParRi(n.endInfo)
    n = callStart; skip n, SkipFull
    if needsTuple:
      dest.addParRi() # TupconstrX

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let target = n.symId
    # Look ahead at the initialiser BEFORE emitting the type: a raising call
    # makes this local hold the success tuple, and the type is the next thing
    # we write down.
    var val = n
    skip val # name
    skip val # export marker
    skip val # pragmas
    skip val # type
    let cr = val.exprKind in CallKinds and callCanRaise(c.typeCache, val)
    if cr:
      c.tupleVars.incl target
    c.typeCache.takeLocalHeader(dest, n, kind, cr)
    if n.exprKind in CallKinds:
      trCall c, dest, n, cr
    else:
      tr c, dest, n
  if cr:
    addPropagationCheck(c, dest, target, false, n.endInfo)

proc trResultDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  copyInto dest, n:
    c.resultSym = n.symId
    c.typeCache.takeLocalHeader(dest, n, ResultY, c.canRaise)
    tr(c, dest, n)
  # produce `result[0] = Success` statement for initialization:
  if c.canRaise:
    copyIntoKind dest, AsgnS, info:
      copyIntoKind dest, TupatX, info:
        dest.addSymUse c.resultSym, info
        dest.addIntLit 0, info
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info

# -------------------- statements -------------------------------------------

proc trStmtsInto(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## The statement-insertion loop, into an ALREADY-OPEN statement list:
  ## whatever `trCall` collected in `c.hoisted` while translating one
  ## statement goes in front of it. An enclosing statement's own hoists are
  ## parked across the descent.
  var outerHoisted = createTokenBuf(16)
  swap(outerHoisted, c.hoisted)
  while n.hasMore:
    let stmtStart = dest.len
    tr c, dest, n
    if c.hoisted.len > 0:
      # `stmtStart` is past every still-open tag, so the splice cannot
      # invalidate an enclosing scope's bookkeeping.
      dest.insert(c.hoisted, stmtStart)
      c.hoisted.shrink 0
  swap(c.hoisted, outerHoisted)

proc trStmtList(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Both `(stmts ...)` and `(scope ...)` are statement lists, so both must
  ## land the hoists here: a `scope` that only recursed would push its
  ## children's temps out to the enclosing `stmts`, in front of the very
  ## locals they read ("could not find symbol").
  copyInto dest, n:
    trStmtsInto c, dest, n

proc trScope(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  trStmtList c, dest, n
  c.typeCache.closeScope()

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: move(c.typeCache),
                   hoisted: createTokenBuf(16),
                   tupleVars: initHashSet[SymId](),
                   canRaise: hasPragma(r.pragmas, RaisesP),
                   retType: r.retType, resultSym: SymId(0))

  copyInto(dest, n):
    let isConcrete = takeRaisingHeader(c2, dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c2.typeCache.registerLocal(symId, r.kind, decl)
      c2.typeCache.openScope()
      let info = n.info
      copyIntoKind dest, StmtsS, info:
        if n.stmtKind == StmtsS:
          n.into:
            trStmtsInto c2, dest, n
        else:
          tr c2, dest, n
        if c2.canRaise and isVoidType(r.retType):
          # A void raising routine returns its code and nothing else, so it
          # needs a trailing "nothing went wrong".
          copyIntoKind dest, RetS, info:
            dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      c2.typeCache.closeScope()
    else:
      takeTree dest, n
  c.typeCache = move(c2.typeCache)

proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Leaving the routine runs every enclosing `finally` first.
  emitFinsDownTo c, dest, 0
  if c.canRaise:
    copyInto dest, n:
      if n.kind == DotToken:
        dest.addSymUse pool.syms.getOrIncl(SuccessName), n.info
        inc n
      elif n.kind == Symbol and n.symId == c.resultSym:
        # `(ret result)` — the trailing return every value-returning routine
        # ends with. `result` ALREADY IS the success tuple, and its code half
        # is `Success` on every path that reaches here (a raising path returns
        # its own tuple and never falls through), so hand it back untouched.
        #
        # Rebuilding it as `(Success, result[1])` would be correct but is a
        # READ of the payload, and the duplifier — which runs after this pass —
        # rightly answers a read in owned position with a `=dup`, so the
        # rebuild leaked the value it was supposed to be returning.
        dest.addSymUse n.symId, n.info
        inc n
      else:
        let maybeClose = produceSuccessTuple(c, dest, c.retType, n.info)
        tr c, dest, n
        if maybeClose:
          dest.addParRi() # tuple constructor
  else:
    copyInto dest, n:
      tr c, dest, n

proc trRaise(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## A `raise` the SOURCE wrote. The ones this pass produces for a failing
  ## call are built by `addPropagationCheck` and never come back through here.
  let info = n.info
  if n.childCursor.kind == DotToken:
    # Bare `(raise .)` (re-raise) reaches us when derefs lowers a heap-based
    # exception's no-match fall-through. In a `.raises` context we propagate
    # the in-flight exception by signalling `Failure` to the caller; the caller
    # consults the threadvar `exc` for the actual value. Outside a raises proc
    # there is no error channel, so we degrade to a bare `(ret .)`.
    skip n # the whole bare `(raise .)`
    if c.canRaise:
      var code = createTokenBuf(2)
      code.addSymUse pool.syms.getOrIncl(FailureName), info
      emitUnwind c, dest, code, info
    else:
      emitFinsDownTo c, dest, 0
      copyIntoKind dest, RetS, info:
        dest.addDotToken()
    return

  var code = createTokenBuf(8)
  n.into:
    tr c, code, n
  emitUnwind c, dest, code, info

proc trScopeOf(c: var Context; dest: var TokenBuf; n: var Cursor; info: NifLineInfo) =
  ## Translate one body — a `try` body or an `except` body — into a `(scope)`.
  ## It has to be a `scope` and not a `stmts`: the construct that used to make
  ## it a destructor scope is being dissolved here, and the destroyer only
  ## treats `(scope)` as one.
  c.typeCache.openScope()
  copyIntoKind dest, ScopeS, info:
    if n.stmtKind == StmtsS:
      n.into:
        trStmtsInto c, dest, n
    else:
      tr c, dest, n
  c.typeCache.closeScope()

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Lower `try` to the flat goto form — the shape `lengcgen` used to build at
  ## the very end, now built here, so nothing downstream has to know what a
  ## `try` is:
  ##
  ##   <except binding>      # declared BEFORE, the raise sites assign to it
  ##   (scope <body>)        # a raise inside became a `jmp` to `exlab`
  ##   <finally>             # the fall-through path's copy
  ##   (jmp `exend.N)
  ##   (lab :`exlab.N)
  ##   (scope <handler>)
  ##   <finally>             # the caught path's copy
  ##   (lab :`exend.N)
  ##
  ## The labels are DIRECT children of the enclosing statement list on purpose:
  ## `destroyer.collectLabels` scans direct children only, and a label it
  ## cannot see is a jump whose scope exits run no destructors.
  ##
  ## The handler skips the fall-through path's finally and gets its own copy,
  ## because the raise that reached it did NOT run this `try`'s finally — an
  ## exception caught here has its cleanup owed at the END of the handler, the
  ## same ordering the destroyer used to produce.
  let info = n.info
  let tryStart = n
  var body = sub(n)
  var probe = body
  skip probe                 # -> first `except`, or the `fin`, or nothing
  let hasExcept = probe.substructureKind == ExceptU
  var q = probe
  while q.substructureKind == ExceptU: skip q
  let fin = if q.substructureKind == FinU: q.childCursor else: default(Cursor)

  let handlerLab = if hasExcept: freshLabel(c, "`exlab.") else: NoSymId
  let endLab = if hasExcept: freshLabel(c, "`exend.") else: NoSymId

  # `except E as e:` — the binding is declared in front of the `try`, because
  # the sites that assign to it are inside the body.
  var excVar = NoSymId
  if hasExcept:
    var h = probe
    inc h
    if h.stmtKind == LetS:
      copyInto dest, h:
        excVar = h.symId
        c.typeCache.takeLocalHeader(dest, h, LetY)
        assert h.isDotToken
        dest.addSubtree h
        inc h

  # --- the guarded body, and the fall-through path ----------------------
  c.exits.add ExitScope(kind: TryExit, label: handlerLab, exceptVar: excVar,
                        fin: fin)
  trScopeOf c, dest, body, info
  # The frame comes off before the finally: a raise inside it propagates PAST
  # this `try`, and must not replicate the body it is already running.
  discard c.exits.pop()
  # Emitted AFTER the scope, so a body local's destructor runs before the
  # finally — which is well defined precisely because a `try` body is a real
  # scope and its finally can never name what it declares.
  emitFinCopy c, dest, fin

  if hasExcept:
    copyIntoKind dest, JmpS, info:
      dest.addSymUse endLab, info
    copyIntoKind dest, LabS, info:
      dest.addSymDef handlerLab, info

    var h = probe
    var first = true
    while h.substructureKind == ExceptU:
      let hinfo = h.info
      if not first:
        copyIntoKind dest, JmpS, hinfo:
          dest.addSymUse endLab, hinfo
      first = false
      var hh = sub(h)
      skip hh                # the `E as e` binding or the matched type
      # A raise inside the handler propagates PAST this `try` — it is not
      # caught by the handler it is raised in — but still owes this `try`'s
      # finally, so the frame stays on the stack without a catch label.
      c.exits.add ExitScope(kind: TryExit, label: NoSymId, exceptVar: NoSymId,
                            fin: fin)
      trScopeOf c, dest, hh, hinfo
      discard c.exits.pop()
      emitFinCopy c, dest, fin
      skip h

    copyIntoKind dest, LabS, info:
      dest.addSymDef endLab, info

  n = tryStart
  skip n

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var nn = n.childCursor
  if nn.kind == Symbol and ((nn.symId == c.resultSym and c.canRaise) or
                            c.tupleVars.contains(nn.symId)):
    let isResultSym = nn.symId == c.resultSym
    copyInto dest, n:
      dest.addSubtree n  # the destination, NOT projected: it IS the tuple
      inc n
      let typ = if isResultSym: c.retType else: getType(c.typeCache, n)
      let maybeClose = produceSuccessTuple(c, dest, typ, n.info)
      tr c, dest, n
      if maybeClose:
        dest.addParRi() # tuple constructor
  else:
    copyInto dest, n:
      tr c, dest, n
      tr c, dest, n

proc trBreak(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Leaving a `block` or a loop runs the `finally` of every `try` between
  ## here and it — but not of any `try` further out, which we are still in.
  let lab = n.childCursor
  var i = c.exits.len - 1
  while i >= 0:
    if c.exits[i].kind == BlockExit and
       (lab.kind != Symbol or c.exits[i].label == lab.symId):
      break
    dec i
  emitFinsDownTo c, dest, i + 1
  takeTree dest, n

proc trLoopOrBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `block`/`while`/`corofor`: a `break` inside lands here, so the unwinder
  ## has to be able to stop at it.
  var label = NoSymId
  if n.stmtKind == BlockS:
    let l = n.childCursor
    if l.kind in {Symbol, SymbolDef}: label = l.symId
  c.exits.add ExitScope(kind: BlockExit, label: label, exceptVar: NoSymId,
                        fin: default(Cursor))
  copyInto dest, n:
    while n.hasMore: tr c, dest, n
  discard c.exits.pop()

proc trPragmaBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(pragmax <pragmas> <body>)`: the pragmas are opaque here — the overflow
  ## and cast blocks are `constparams`' business — but the body is ordinary
  ## code and may raise.
  copyInto dest, n:
    dest.takeTree n # pragmas
    while n.hasMore: tr c, dest, n

proc trObjConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    takeTree dest, n # type
    while n.hasMore:
      if n.substructureKind == KvU:
        takeInto dest, n:
          takeTree dest, n # key
          tr c, dest, n
          if n.hasMore:
            # optional inheritance
            takeTree dest, n
      else:
        # V-Table:
        takeTree dest, n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    if (n.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(n.symId):
      let info = n.info
      copyIntoKind dest, TupatX, info:
        dest.addSymUse n.symId, info
        dest.addIntLit 1, info
    else:
      dest.addSubtree n
    inc n
  of SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StrLit, UnknownToken, DotToken, EofToken:
    takeTree dest, n
  of TagLit:
    let ek = n.exprKind
    case ek
    of CallKinds:
      trCall c, dest, n, false
    of TypeofX:
      takeTree dest, n
    of PragmaxX:
      trPragmaBlock c, dest, n
    of DotX:
      takeInto dest, n:
        tr c, dest, n
        while n.hasMore:
          dest.takeTree n
    of OconstrX:
      trObjConstr c, dest, n
    else:
      case n.stmtKind
      of ProcS, FuncS, MethodS, ConverterS:
        trProcDecl c, dest, n
      of LocalDecls - {ResultS}:
        trLocal c, dest, n
      of ResultS:
        trResultDecl c, dest, n
      of ScopeS:
        trScope c, dest, n
      of StmtsS:
        trStmtList c, dest, n
      of AsgnS:
        trAsgn c, dest, n
      of RetS:
        trRet c, dest, n
      of RaiseS:
        trRaise c, dest, n
      of TryS:
        trTry c, dest, n
      of BreakS:
        trBreak c, dest, n
      of BlockS, WhileS, CoroforS:
        trLoopOrBlock c, dest, n
      of MacroS, TemplateS, TypeS:
        takeTree dest, n
      of CallS, CmdS, IteratorS, EmitS, IfS, WhenS,
         ContinueS, ForS, CaseS, YldS,
         PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
         FromimportS, ImportexceptS, ExportS, ExportexceptS, CommentS,
         DiscardS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
         InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
         AsmS, DeferS, LabS, JmpS, NoStmt:
        # generic container: copy the head and recurse into the children
        copyInto dest, n:
          while n.hasMore:
            tr c, dest, n
  else:
    raiseAssert "BUG: unexpected ParRi in eraiser.tr" # classic ParRi only

proc injectRaisingCalls*(pass: var Pass; ptrSize: int) =
  var n = pass.n  # Extract cursor locally
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(pass.bits),
                  hoisted: createTokenBuf(16), tupleVars: initHashSet[SymId]())
  c.retType = c.typeCache.builtins.voidType
  c.typeCache.openScope()
  tr(c, pass.dest, n)  # Write to pass.dest
  c.typeCache.closeScope()

#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[

The destroyer runs after `to_stmts` as it relies on `var tmp = g()`
injections. It only destroys variables and transforms assignments.

Statements
==========

Assignments and var bindings need to use `=dup`. In the first version, we don't
emit `=copy`.

`x = f()` is turned into `=destroy(x); x =bitcopy f()`.
`x = lastUse y` is turned into either

  `=destroy(x); x =bitcopy y; =wasMoved(y)` # no self assignments possible

or

  `let tmp = y; =wasMoved(y); =destroy(x); x =bitcopy tmp`  # safe for self assignments

`x = someUse y` is turned into either

  `=destroy(x); x =bitcopy =dup(y)` # no self assignments possible

or

  `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)` # safe for self assignments

`var x = f()` is turned into `var x = f()`. There is nothing to do because the backend
interprets this `=` as `=bitcopy`.

`var x = lastUse y` is turned into `var x = y; =wasMoved(y)`.
`var x = someUse y` is turned into `var x = =dup(y)`.

]##

import std / [assertions, tables, hashes, sets, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [nifindexes, symparser, treemangler]
import passes
import ".." / nimony / [nimony_model, programs, typenav, decls]
import lifter

const
  NoLabel = SymId(0)

type
  ScopeKind = enum
    Other
      ## Default. If `finallySection` is set (only for `except`-body
      ## scopes), `leaveScope` inlines it — this is what makes the
      ## raise→except→finally path emit the finally.
    CaughtLocally
      ## Body scope of a `try` with an `except` arm. The raise is caught
      ## by that except, so the (own) finally runs naturally afterward;
      ## `leaveScope` skips it, and `trRaise` stops walking outward at
      ## this scope so an *outer* try's finally is not inlined either.
    TryFinOnlyBody
      ## Body scope of a `try`/`finally` *without* `except`. On normal
      ## exit (`trScope`) the surrounding `(fin ...)` clause is emitted
      ## naturally by `trTry`, so we skip; on raise the raise propagates
      ## past the try and `trRaise` must inline the finally before the
      ## jump.
    WhileOrBlock
  DestructorOp = object
    destroyProc: SymId
    arg: SymId

  Scope = object
    label: SymId
    kind: ScopeKind
    isTopLevel: bool
    destroyOps: seq[DestructorOp]
    info: NifLineInfo
    finallySection: Cursor
    parent: ptr Scope

  Context = object
    currentScope: Scope
    #procStart: Cursor
    anonBlock: SymId
    dest: TokenBuf
    lifter: ref LiftingCtx
    terminates: bool
      ## True when the statement just translated ends its statement list
      ## unconditionally (`return`/`raise`/`break`). Those already ran the
      ## enclosing scopes' destructors via `trReturn`/`trRaise`/`trBreak`,
      ## so `trScope` must NOT append the scope's destructor sequence
      ## after them: it would be dead code. See `trScope`.

proc createNestedScope(kind: ScopeKind; parent: var Scope; info: NifLineInfo;
                       label = NoLabel; fin = default(Cursor)): Scope =
  Scope(label: label,
    kind: kind, destroyOps: @[], info: info, parent: addr(parent),
    isTopLevel: false, finallySection: fin)

proc createEntryScope(info: NifLineInfo): Scope =
  Scope(label: NoLabel,
    kind: Other, destroyOps: @[], info: info, parent: nil,
    isTopLevel: true)

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId) =
  let info = c.currentScope.info
  copyIntoKind c.dest, CallS, info:
    copyIntoSymUse c.dest, destroyProc, info
    if isMutFirstParam(destroyProc):
      copyIntoKind c.dest, HaddrX, info:
        copyIntoSymUse c.dest, arg, info
    else:
      copyIntoSymUse c.dest, arg, info

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor)

proc freshVars(n: var Cursor; newVars: var Table[SymId, SymId]; idgen: var int;
               dest: var TokenBuf) =
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
  of UIntLit, StrLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident:
    dest.addSubtree n
    inc n
  else:
    raiseAssert "BUG: unexpected ParRi in destroyer.createFreshVars" # classic ParRi only

proc createFreshVars(c: var Context; n: Cursor): TokenBuf =
  var n = n
  var newVars = initTable[SymId, SymId]()
  var idgen = 0
  result = createTokenBuf(30)
  freshVars(n, newVars, idgen, result)

proc leaveScope(c: var Context; sptr: ptr Scope; kind = Other; raising = false) =
  ## Walk-out of one scope: optionally inline the scope's finally body and
  ## run destructors.
  ##
  ## `sptr` is taken as `ptr Scope` (not `var Scope` — Nimony's borrow
  ## checker would reject passing a field of `c` as both `var Context`
  ## and `var Scope`) so we can detach `sptr.finallySection` during the
  ## inline. The detach is what prevents infinite recursion: a raise
  ## inside the inlined finally body re-enters `trRaise`, walks the same
  ## `c.currentScope` chain that still links back to `sptr^`, and would
  ## otherwise inline this same finally forever. The restore at the end
  ## puts the finally back so subsequent branches of an enclosing
  ## `if`/`case` still see it on their normal exit.
  ##
  ## "Inline finally" decision per scope kind:
  ##   - `CaughtLocally`: never inline. A raise here will be caught by
  ##     this try's `except`; the finally runs naturally afterward.
  ##   - `TryFinOnlyBody`: inline only when leaving via a raise (the
  ##     raise propagates past the try and `nifcgen`'s straight-line
  ##     emission of the finally won't run before the raise jump). Skip
  ##     on normal exit, where `trTry` emits the finally clause itself.
  ##   - `Other`/`WhileOrBlock` with a `finallySection`: inline. The only
  ##     scope that has its `finallySection` set under `Other` is an
  ##     `except`-body; inlining the outer finally at its end is what
  ##     makes the raise→except→finally path actually run the finally
  ##     (nifcgen's else-branch structure only runs it on the no-raise
  ##     path).
  let savedFin = sptr.finallySection
  sptr.finallySection = default(Cursor)
  let inlineFin =
    case kind
    of CaughtLocally: false
    of TryFinOnlyBody: raising and savedFin != default(Cursor)
    of Other, WhileOrBlock: savedFin != default(Cursor)
  if inlineFin:
    var freshVars = createFreshVars(c, savedFin)
    var n = beginRead(freshVars)
    tr c, n
  for i in countdown(sptr.destroyOps.high, 0):
    callDestroy c, sptr.destroyOps[i].destroyProc, sptr.destroyOps[i].arg
  sptr.finallySection = savedFin

proc leaveNamedBlock(c: var Context; label: SymId) =
  #[ Consider:

  var x = f()
  block:
    break # do we want to destroy x here? No.

  ]#
  var it = addr(c.currentScope)
  while it != nil and it.label != label:
    leaveScope(c, it)
    it = it.parent
  if it != nil and it.label == label:
    leaveScope(c, it)
  else:
    bug "do not know which block to leave"

proc leaveAnonBlock(c: var Context) =
  var it = addr(c.currentScope)
  while it != nil and it.kind != WhileOrBlock:
    leaveScope(c, it)
    it = it.parent
  if it != nil and it.kind == WhileOrBlock:
    leaveScope(c, it)
  else:
    bug "do not know which block to leave"

proc trBreak(c: var Context; n: var Cursor) =
  if c.terminates:
    # unreachable: an earlier jump already left these scopes
    takeTree c.dest, n
    return
  let lab = n.childCursor
  if lab.kind == Symbol:
    leaveNamedBlock(c, lab.symId)
  else:
    leaveAnonBlock(c)
  takeTree c.dest, n

proc trReturn(c: var Context; n: var Cursor) =
  if c.terminates:
    #[ Unreachable: the statement list already ended in a jump, which ran
       the destructors for every enclosing scope. Running them a second
       time is not the harmless dead code it looks like — `eliminateJumps`
       (CPS) rewrites both `return`s into flag assignments and lays the
       tail out as straight-line fallthrough, so both sequences execute.
       A `proc … {.passive.}` whose body ends `result = …` + explicit
       `return` gets this shape from the frontend's implicit trailing
       `(ret result)`. ]#
    takeTree c.dest, n
    return
  var it = addr(c.currentScope)
  while it != nil:
    leaveScope(c, it)
    it = it.parent
  takeTree c.dest, n

proc trRaise(c: var Context; n: var Cursor) =
  #[
  Walk enclosing scopes, inlining each scope's finally before the raise
  jump. Stop at the first `CaughtLocally` scope: that scope is the body
  of a `try` with an `except` arm, so the raise is caught right there and
  cannot reach any outer finally. Without this stop, a nested
  `try`-with-`except` inside an outer `try`'s `except`-body would
  spuriously inline the outer finally before the raise lands on the
  inner handler — see `tnested_heap_with_fin.nim`.
  ]#
  if c.terminates:
    # unreachable: an earlier jump already left these scopes
    takeTree c.dest, n
    return
  var it = addr(c.currentScope)
  while it != nil:
    leaveScope(c, it, it.kind, raising = true)
    if it.kind == CaughtLocally:
      break
    it = it.parent
  takeTree c.dest, n

proc trLocal(c: var Context; n: var Cursor) =
  let info = n.info
  c.dest.addParLe(n.cursorTagId, n.info)
  var r = takeLocal(n, SkipFinalParRi)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.typ

  tr c, r.val
  c.dest.addParRi()

  let destructor = getDestructor(c.lifter[], r.typ, info)
  if destructor != NoSymId and r.kind notin {CursorY, PatternvarY, ResultY, GvarY, TvarY, GletY, TletY, ConstY}:
    c.currentScope.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trScope(c: var Context; body: var Cursor; kind = Other) =
  copyIntoKind c.dest, StmtsS, body.info:
    c.terminates = false
    if body.stmtKind == StmtsS:
      body.into:
        while body.hasMore:
          tr c, body
    else:
      tr c, body
    #[ A scope whose statement list ends in `return`/`raise`/`break` has
       already had its destructors emitted by `trReturn`/`trRaise`/
       `trBreak`, which walk the whole scope chain before the jump.
       Appending the sequence again here used to be merely dead code in
       straight-line output — but the CPS pass runs `eliminateJumps` AFTER
       us, and that rewrites `return` into `(jtrue ´r)` plus fallthrough,
       which makes the dead tail REACHABLE. The result was every escaping
       local destroyed twice, plus a destroy of the coroutine-frame field
       the return value had just been moved out of (arcopt elides the
       paired `=wasMoved` together with the first, genuinely dead,
       destroy). ]#
    if not c.terminates:
      leaveScope(c, addr(c.currentScope), kind)

proc registerSinkParameters(c: var Context; params: Cursor) =
  if not params.isTagLit: return
  var p = params
  p = sub(p)  # throwaway copy; bounds the walk under vpr
  while p.hasMore:
    let r = takeLocal(p, SkipFinalParRi)
    if r.typ.typeKind == SinkT:
      let destructor = getDestructor(c.lifter[], r.typ.childCursor, p.endInfo)
      if destructor != NoSymId:
        c.currentScope.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trProcDecl(c: var Context; n: var Cursor) =
  c.dest.addParLe(n.cursorTagId, n.info)
  var r = takeRoutine(n, SkipFinalParRi)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pattern
  copyTree c.dest, r.typevars
  copyTree c.dest, r.params
  copyTree c.dest, r.retType
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.effects
  if r.body.stmtKind == StmtsS and not isGeneric(r):
    if hasPragma(r.pragmas, NodestroyP):
      copyTree c.dest, r.body
    else:
      var s2 = createEntryScope(r.body.info)
      s2.isTopLevel = false
      swap c.currentScope, s2
      registerSinkParameters(c, r.params)
      trScope c, r.body
      swap c.currentScope, s2
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()

proc trNestedScope(c: var Context; body: var Cursor; kind = Other; fin = default(Cursor)) =
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(kind, oldScope, body.info, NoLabel, fin)
  trScope c, body, kind
  swap c.currentScope, oldScope

proc trWhile(c: var Context; n: var Cursor) =
  #[ while prop(createsObj())
      was turned into `while (let tmp = createsObj(); prop(tmp))` by  `duplifier.nim`
      already and `to_stmts` did turn it into:

      while true:
        let tmp = createsObj()
        if not prop(tmp): break

      For these reasons we don't have to do anything special with `cond`. The same
      reasoning applies to `if` and `case` statements.
  ]#
  copyInto(c.dest, n):
    tr c, n
    trNestedScope c, n, WhileOrBlock

proc trBlock(c: var Context; n: var Cursor) =
  let label = n.childCursor
  let labelId = if label.kind == SymbolDef: label.symId else: c.anonBlock
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(WhileOrBlock, oldScope, n.info, labelId)
  copyInto(c.dest, n):
    takeTree c.dest, n
    trScope c, n
    swap c.currentScope, oldScope

proc trIf(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    while n.hasMore:
      case n.substructureKind
      of ElifU:
        copyInto(c.dest, n):
          tr c, n
          trNestedScope c, n
      of ElseU:
        copyInto(c.dest, n):
          trNestedScope c, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
          TypevarU, StaticTypevarU, EfldU, FldU, WhenU, TypevarsU, CaseU, OfU,
          StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
          UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, NoSub:
        takeTree c.dest, n

proc trCase(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    tr c, n
    while n.hasMore:
      case n.substructureKind
      of OfU:
        copyInto(c.dest, n):
          takeTree c.dest, n
          trNestedScope c, n
      of ElseU:
        copyInto(c.dest, n):
          trNestedScope c, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
          TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
          StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
          UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, NoSub:
        takeTree c.dest, n

proc trTry(c: var Context; n: var Cursor) =
  var nn = n
  inc nn
  skip nn # try statements
  var hasExcept = false
  while nn.substructureKind == ExceptU:
    hasExcept = true
    skip nn
  copyInto(c.dest, n):
    let fin = if nn.substructureKind == FinU: nn.childCursor else: default(Cursor)
    # Body kind depends on whether an `except` arm exists:
    #   - has except: raise from the body is caught by that except, so the
    #     finally runs naturally afterward (no duplication) and there's no
    #     reason to walk past this scope.
    #   - no except (plain try/finally): raise propagates past the try and
    #     the finally must be inlined before the raise jump. Use
    #     `TryFinOnlyBody` so `leaveScope` inlines the finally on raise
    #     but not on normal exit (where `trTry` emits the finally clause
    #     itself).
    let bodyKind = if hasExcept: CaughtLocally else: TryFinOnlyBody
    trNestedScope c, n, bodyKind, fin
    while n.substructureKind == ExceptU:
      copyInto(c.dest, n):
        takeTree c.dest, n # `E as e`
        trNestedScope c, n, Other, fin
    if n.substructureKind == FinU:
      copyInto(c.dest, n):
        trNestedScope c, n

proc tr(c: var Context; n: var Cursor) =
  # `c.terminates` tracks whether the statement we are about to translate
  # leaves its statement list unconditionally. Only the three jump
  # statements set it; everything else clears it. A plain nested `(stmts`
  # is transparent (it is not a scope of its own here) so it inherits the
  # flag from its last child — see the generic branch below, which does
  # not clear the flag after the child loop.
  if isAtom(n) or isDeclarative(n):
    # Emits no code of its own, so it neither terminates the statement
    # list nor resurrects it: leave `c.terminates` alone.
    takeTree c.dest, n
  else:
    case n.stmtKind
    of RetS:
      trReturn(c, n)
      c.terminates = true
    of RaiseS:
      trRaise(c, n)
      c.terminates = true
    of BreakS:
      trBreak(c, n)
      c.terminates = true
    of IfS:
      trIf c, n
      c.terminates = false
    of CaseS:
      trCase c, n
      c.terminates = false
    of BlockS:
      trBlock c, n
      c.terminates = false
    of LocalDecls:
      trLocal c, n
      c.terminates = false
    of WhileS, CoroforS:
      trWhile c, n
      c.terminates = false
    of TryS:
      trTry c, n
      c.terminates = false
    of ProcS, FuncS, MethodS, ConverterS:
      trProcDecl c, n
      c.terminates = false
    of IteratorS:
      # iterinliner passes only `.closure` iterators through to here. Their
      # bodies need destroyer treatment (scope tracking, =destroy injection
      # on locals) when the closure flag is actually set; non-closure iters
      # would have been stripped by iterinliner.
      var probe = n
      let routine = asRoutine(probe, SkipExclBody)
      if hasPragma(routine.pragmas, ClosureP):
        trProcDecl c, n
      else:
        takeTree c.dest, n
      c.terminates = false
    of MacroS:
      # Macros are out-of-process plugins compiled separately; their
      # bodies don't participate in lowering.
      takeTree c.dest, n
      c.terminates = false
    of CallS, CmdS, TemplateS, TypeS, EmitS, AsgnS,
        ScopeS, WhenS, ContinueS, ForS, YldS, StmtsS, PragmasS,
        PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
        FromimportS, ImportexceptS, ExportS, ExportexceptS,
        CommentS, DiscardS, UnpackdeclS, AssumeS, AssertS,
        CallstrlitS, InfixS, PrefixS, HcallS, StaticstmtS,
        BindS, MixinS, UsingS, AsmS, DeferS, NoStmt:
      if n.isTagLit:
        let isStmtList = n.stmtKind == StmtsS
        c.dest.addParLe(n.cursorTagId, n.info)
        c.terminates = false
        n.into:
          while n.hasMore:
            tr(c, n)
        c.dest.addParRi()
        # A transparent `(stmts` keeps whatever its last child left behind;
        # anything else (a call, an assignment, …) falls through.
        if not isStmtList:
          c.terminates = false
      else:
        c.dest.addSubtree n
        inc n
        c.terminates = false

proc injectDestructors*(pass: var Pass; lifter: ref LiftingCtx) =
  var n = pass.n  # Extract cursor locally
  var c = Context(lifter: lifter, currentScope: createEntryScope(n.info),
    anonBlock: pool.syms.getOrIncl("`anonblock.0"),
    dest: move(pass.dest), terminates: false)
  assert n.stmtKind == StmtsS
  c.dest.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      tr(c, n)

    leaveScope c, addr(c.currentScope)
  # The root `(stmts` is deliberately left OPEN: the pipeline appends the
  # generated hooks and closes it. An emitted close cannot be rolled back
  # under `-d:virtualParRi` (it seals the tag and is elided), so the old
  # "close here, shrink away in the pipeline" dance is impossible.
  genMissingHooks lifter[]
  pass.dest = ensureMove c.dest

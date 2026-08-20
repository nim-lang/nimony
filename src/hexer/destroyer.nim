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
import ".." / lengc / shoggoth / trackers
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
    labels: seq[SymId]
      ## The `(lab :L)`s declared directly in this scope's statement list. A
      ## `(jmp L)` nested inside must run the destructors of every scope it
      ## leaves on the way out to the one that owns `L` — the same walk
      ## `leaveNamedBlock` does for `break`, minus the final `leaveScope`
      ## (the jump lands *inside* the owning scope, it does not exit it).
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
    flow: Tracker[SymId, bool]
      ## Path bookkeeping: **has control already left this statement list?**
      ##
      ## `return`/`raise`/`break` run every enclosing scope's destructors on
      ## their way out (`trReturn`/`trRaise`/`trBreak`), so `trScope` must not
      ## append the scope's destructor sequence after one of them — it would be
      ## dead code, and the CPS pass makes dead tails reachable again (see
      ## `trScope`). The same holds for `jmp`.
      ##
      ## That fact is a *join*: an `if` diverges only if every arm does, a
      ## `(lab L)` is live again if anything jumps to it. Rather than hand-roll
      ## it, this is the `Tracker` from `lengc/shoggoth/trackers.nim` — the one
      ## `cse` and `copyprop` use. `markDiverged`/`gotoLabel` set it,
      ## `openBranch`/`landLabel` clear it for a fresh path, and
      ## `closeBranches`/`landLabel` re-derive it for the enclosing path. The
      ## traversal therefore never computes divergence itself; it only says
      ## where the branches and labels are.
      ##
      ## The key space (`SymId -> "already destroyed on this path"`) is unused
      ## so far — while no jump crosses a scope owning destructible locals
      ## there is nothing per-local to merge. It is where that state goes when
      ## one does.

template terminates(c: Context): bool = c.flow.diverged
  ## The path has already left: everything until the next join is dead.

proc collectLabels(body: Cursor): seq[SymId] =
  ## The label symbols defined at the top level of `body`. `jmp` is
  ## forward-only and scoped, so a jump's target is always a `(lab)` in one of
  ## the enclosing scopes' own statement lists — one direct-children scan per
  ## scope is enough to resolve every jump inside it.
  result = @[]
  var b = body
  if b.stmtKind notin {StmtsS, ScopeS}: return
  b = sub(b)   # peek only, never left
  while b.hasMore:
    if b.stmtKind == LabS:
      let sym = b.childCursor
      if sym.kind in {Symbol, SymbolDef}:
        result.add sym.symId
    skip b

proc labelIdOf(n: Cursor): LabelId =
  ## The `(lab :L)` / `(jmp L)` operand as a `Tracker` label.
  assert n.kind in {Symbol, SymbolDef}, "lab/jmp operand is not a symbol"
  LabelId(uint32(n.symId))

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
      # A nested routine's body is a path of its own: its divergence must not
      # leak into the enclosing statement list.
      var outerFlow = initTracker[SymId, bool]()
      swap c.flow, outerFlow
      registerSinkParameters(c, r.params)
      trScope c, r.body
      swap c.flow, outerFlow
      swap c.currentScope, s2
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()

proc trNestedScope(c: var Context; body: var Cursor; kind = Other; fin = default(Cursor)) =
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(kind, oldScope, body.info, NoLabel, fin)
  c.currentScope.labels = collectLabels(body)
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
    # The body may run zero times, so the path is live afterwards no matter
    # what the body does: one non-exhaustive branch. `clearAll` is the loop
    # protocol from `trackers.nim` — nothing can be carried across a back-edge.
    c.flow.clearAll()
    c.flow.openBranches()
    c.flow.openBranch()
    trNestedScope c, n, WhileOrBlock
    c.flow.closeBranch()
    c.flow.closeBranches()
    c.flow.clearAll()

proc trBlock(c: var Context; n: var Cursor) =
  let label = n.childCursor
  let labelId = if label.kind == SymbolDef: label.symId else: c.anonBlock
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(WhileOrBlock, oldScope, n.info, labelId)
  copyInto(c.dest, n):
    takeTree c.dest, n
    # A `break` targeting this block makes its end a join, and `trBreak` has
    # already marked that path diverged — so the block as a whole is modelled
    # as one non-exhaustive branch: live afterwards.
    c.flow.openBranches()
    c.flow.openBranch()
    trScope c, n
    c.flow.closeBranch()
    c.flow.closeBranches()
    swap c.currentScope, oldScope

proc trIf(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    # The arms are one sibling group. `closeBranches` derives the `if`'s own
    # divergence from them — an `if`/`else` whose every arm returns leaves the
    # path dead, which the old "clear the flag after every `if`" could not say.
    # Without an `else` the group is NOT exhaustive (`openFinalBranch` is what
    # marks it so), and the implicit no-arm-matched path keeps it live.
    c.flow.openBranches()
    while n.hasMore:
      case n.substructureKind
      of ElifU:
        copyInto(c.dest, n):
          tr c, n            # the condition runs on the fall-through path
          c.flow.openBranch()
          trNestedScope c, n
          c.flow.closeBranch()
      of ElseU:
        copyInto(c.dest, n):
          c.flow.openFinalBranch()
          trNestedScope c, n
          c.flow.closeBranch()
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
          TypevarU, StaticTypevarU, EfldU, FldU, WhenU, TypevarsU, CaseU, OfU,
          StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
          UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, NoSub:
        takeTree c.dest, n
    c.flow.closeBranches()

proc trCase(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    tr c, n
    c.flow.openBranches()
    while n.hasMore:
      case n.substructureKind
      of OfU:
        copyInto(c.dest, n):
          takeTree c.dest, n
          c.flow.openBranch()
          trNestedScope c, n
          c.flow.closeBranch()
      of ElseU:
        copyInto(c.dest, n):
          c.flow.openFinalBranch()
          trNestedScope c, n
          c.flow.closeBranch()
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
          TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
          StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
          UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, NoSub:
        takeTree c.dest, n
    c.flow.closeBranches()

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
    # Body and handlers are one non-exhaustive sibling group: the body may
    # leave via a raise the handlers catch, so neither alone decides whether
    # the `try` as a whole falls through.
    c.flow.openBranches()
    c.flow.openBranch()
    trNestedScope c, n, bodyKind, fin
    c.flow.closeBranch()
    while n.substructureKind == ExceptU:
      copyInto(c.dest, n):
        takeTree c.dest, n # `E as e`
        c.flow.openBranch()
        trNestedScope c, n, Other, fin
        c.flow.closeBranch()
    c.flow.closeBranches()
    if n.substructureKind == FinU:
      copyInto(c.dest, n):
        trNestedScope c, n

proc tr(c: var Context; n: var Cursor) =
  # `c.terminates` (i.e. `c.flow.diverged`) says whether control has already
  # left this statement list. Only the jump statements set it — `ret`, `raise`,
  # `break` via `markDiverged`, `jmp` via `gotoLabel` — and only a join clears
  # it: `openBranch` starts a fresh path, `landLabel` folds the incoming jumps
  # back in, `closeBranches` re-derives it for the enclosing path. Nothing here
  # computes it.
  if isAtom(n) or isDeclarative(n):
    takeTree c.dest, n
  else:
    case n.stmtKind
    of RetS:
      trReturn(c, n)
      c.flow.markDiverged()
    of RaiseS:
      trRaise(c, n)
      c.flow.markDiverged()
    of BreakS:
      trBreak(c, n)
      c.flow.markDiverged()
    of IfS:
      trIf c, n
    of CaseS:
      trCase c, n
    of BlockS:
      trBlock c, n
    of ScopeS:
      # An explicit scope is a real destructor scope: locals declared inside it
      # die at its end, not at the end of the enclosing branch. `xelim`'s flat
      # `lab`/`jmp` lowering of `if`/`elif` chains relies on this — the branch
      # bodies are no longer nested inside `(elif ... (stmts ...))`, they are
      # `(scope ...)` siblings in the enclosing statement list, and a
      # condition operand's temporaries get a `(scope …)` of their own.
      var oldScope = move c.currentScope
      c.currentScope = createNestedScope(Other, oldScope, n.info)
      c.currentScope.labels = collectLabels(n)
      c.dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          tr c, n
        if not c.terminates:
          leaveScope(c, addr(c.currentScope), Other)
      c.dest.addParRi()
      swap c.currentScope, oldScope
    of JmpS:
      # A structural forward transfer: run the destructors of every scope it
      # leaves, then jump. Two passes — the first only looks for the scope
      # that owns the label, because emitting the destructors of scopes we
      # then turn out not to be leaving would be catastrophic. If no owner is
      # found the jump stays within its own scope (or targets a label the
      # destroyer does not model) and nothing is unwound.
      let sym = n.childCursor
      let target = labelIdOf(sym)
      let targetSym = sym.symId
      var probe = addr(c.currentScope)
      while probe != nil and targetSym notin probe.labels:
        probe = probe.parent
      if probe != nil:
        var it = addr(c.currentScope)
        while it != nil and targetSym notin it.labels:
          leaveScope(c, it)
          it = it.parent
      takeTree c.dest, n
      c.flow.gotoLabel target
    of LabS:
      # The join: `landLabel` folds every `(jmp L)`'s stashed state back in
      # with the fall-through, and the path is live again.
      let target = labelIdOf(n.childCursor)
      takeTree c.dest, n
      c.flow.landLabel target
    of LocalDecls:
      trLocal c, n
    of WhileS, CoroforS:
      trWhile c, n
    of TryS:
      trTry c, n
    of ProcS, FuncS, MethodS, ConverterS:
      trProcDecl c, n
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
    of MacroS:
      # Macros are out-of-process plugins compiled separately; their
      # bodies don't participate in lowering.
      takeTree c.dest, n
    of CallS, CmdS, TemplateS, TypeS, EmitS, AsgnS,
        WhenS, ContinueS, ForS, YldS, StmtsS, PragmasS,
        PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
        FromimportS, ImportexceptS, ExportS, ExportexceptS,
        CommentS, DiscardS, UnpackdeclS, AssumeS, AssertS,
        CallstrlitS, InfixS, PrefixS, HcallS, StaticstmtS,
        BindS, MixinS, UsingS, AsmS, DeferS, NoStmt:
      if n.isTagLit:
        # A transparent `(stmts` keeps whatever its children left behind, and
        # so does everything else here: only a jump statement or a branch join
        # moves `c.flow`, so a call or an assignment needs no bookkeeping.
        c.dest.addParLe(n.cursorTagId, n.info)
        n.into:
          while n.hasMore:
            tr(c, n)
        c.dest.addParRi()
      else:
        c.dest.addSubtree n
        inc n

proc injectDestructors*(pass: var Pass; lifter: ref LiftingCtx) =
  var n = pass.n  # Extract cursor locally
  var c = Context(lifter: lifter, currentScope: createEntryScope(n.info),
    anonBlock: pool.syms.getOrIncl("`anonblock.0"),
    dest: move(pass.dest), flow: initTracker[SymId, bool]())
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

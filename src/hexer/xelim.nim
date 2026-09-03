#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Eliminate eXpressions in complex situations. In other words turns
## `let x = if cond: 3 else: 4` into
## `let tmp; if cond: tmp = 3 else: temp = 4; let x = tmp`

import std / [assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import passes
include ".." / nimony / nif_annotations

type
  Goal* = enum
    ElimExprs    # normal mode: eliminate expressions
    LowerCasts   # lower cast expressions: bind both source and result to variables
    TowardsFinalIr # goal mode: prepare for the Final IR (doc/final_ir.md).
                   # Calls bind to locations, and `and`/`or` are lowered to the
                   # label/jump-friendly if-with-bool-temp form (`trAnd`/`trOr`).
                   # Final IR never introduces a single cfvar.

proc isComplex(n: Cursor; goal: Goal): bool =
  var n = n
  case n.kind
  of TagLit:
    if n.stmtKind in {IfS, CaseS, WhileS, AsgnS, LetS, VarS, CursorS, PatternvarS, StmtsS, ResultS, GletS, TletS, GvarS, TvarS}:
      result = true
    elif n.exprKind == ExprX:
      var probe = n
      probe = sub(probe) # peek only, never left
      let inner = probe
      skip probe
      if probe.hasMore:
        # More than one son is always complex:
        result = true
      else:
        # ExprX with exactly one son might be harmless:
        result = isComplex(inner, goal)
    elif n.exprKind in {AndX, OrX}:
      # `and`/`or` are short-circuit CONTROL FLOW, not expressions, and
      # `controlflow.nim` models them as such: it emits the operand evaluation
      # into the enclosing statement stream *before* whatever the surrounding
      # expression has accumulated so far. Every xelim run must therefore lower
      # them, not just the final one — otherwise the duplifier (which asks the
      # mover, i.e. the control-flow graph, "is this the last read?") reasons
      # about an evaluation order that `xelim_final` then contradicts by
      # hoisting the `and`'s `if` *after* the pre-statements of earlier sibling
      # operands.
      #
      # Concretely, for `Obj(key: kk, selected: c and kk == sel)` the CF says
      # `kk == sel` runs first, so the mover declares `key: kk` the last read
      # and the duplifier sinks it (`tmp = kk; wasMoved kk`) — but the emitted
      # code runs that `wasMoved` before the `and`, which then compares an
      # emptied string and silently yields `false`. `if`/`case`/`try` operands
      # never had this problem precisely because they are complex in *every*
      # goal, so they are already statements by the time the duplifier looks.
      result = true
    elif goal in {LowerCasts, TowardsFinalIr} and n.exprKind in CallKinds:
      result = true
    else:
      result = false
      n.loopInto:
        if isComplex(n, goal):
          return true
        skip n
  else:
    result = false

type
  Mode = enum
    IsEmpty, IsAppend, IsBound, IsIgnored, IsLabel
  Target = object
    ## Where the value of the expression `trExpr` is translating should go.
    ## `IsLabel` is the control-flow mode: it compiles a short-circuit condition
    ## against a jump target rather than materialising it into a bool slot (the
    ## two-target condition compiler `Cx`, below). Selected by goal, see
    ## `CondJumpGoals`.
    m: Mode
    t: TokenBuf
    lab: SymId        ## `IsLabel`: transfer to this label…
    jumpIfTrue: bool  ## …exactly when the expression evaluates to this…
    conditional: bool ## …and: is this subtree one the short-circuit can skip?
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    goal: Goal

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target)
proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

proc tempSymName(c: var Context): string {.inline.} =
  result = "`x." & $c.counter
  inc c.counter

proc getType(c: var Context; n: Cursor): Cursor =
  result = getType(c.typeCache, n)
  assert result.typeKind != AutoT, "cannot compute type of: " & toString(n, false)

proc declareTemp(c: var Context; dest: var TokenBuf; n: Cursor): SymId =
  let info = n.info
  let typ = getType(c, n)
  let s = tempSymName(c)
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, typ # type
    dest.addDotToken() # value

proc declareTempBool(c: var Context; dest: var TokenBuf; info: NifLineInfo): SymId =
  let s = tempSymName(c)
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, c.typeCache.builtins.boolType # type
    dest.addDotToken() # value

proc addTarget(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc trExprInto(c: var Context; dest: var TokenBuf; n: var Cursor; v: SymId) =
  var tar = Target(m: IsEmpty)
  let typ = getType(c, n)
  let info = n.info
    # Capture before `trExpr` advances past the expression — when the
    # input is a standalone buffer (e.g. the hoisted RHS of `and`/`or`
    # short-circuit lowering) `n` lands at end-of-buffer and reading
    # `n.info` afterwards would assert in `nifcursors.load`.
  trExpr c, dest, n, tar

  if typ.typeKind in {VoidT, AutoT}:
    dest.addTarget tar
  else:
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse v, info
      dest.addTarget tar

proc hoistDeclsFromExprX(tc: var TypeCache; outerDest, transformed: var TokenBuf; n: var Cursor;
                         markNoinit = false) =
  ## Copy the subtree at `n` into `transformed`. If the subtree is an
  ## `(expr (stmts decls…) val…)`, top-level `let`/`var`/`cursor` decls
  ## inside the leading `(stmts …)` are *hoisted*: an uninitialised
  ## `(var :sym . . type .)` is emitted into `outerDest` and the original
  ## decl is rewritten as `(asgn sym init)` so the initialiser still runs
  ## at the original control-flow point. `n` is advanced past the consumed
  ## subtree.
  ##
  ## With `markNoinit`, the hoisted `var` carries `.noinit`. The decl came from
  ## a `let` in a short-circuited `and`/`or` operand: the value is always
  ## assigned before any use that the surrounding `if`/`elif` body can reach
  ## (the body runs only when that operand was evaluated). The init analysis
  ## cannot see that correlation through the hoist, so the tag tells it to treat
  ## the slot as initialised — used only on the Final-IR (analysis) path, so
  ## codegen still gets the plain zero-initialised slot.
  if n.kind != TagLit or n.exprKind != ExprX:
    transformed.takeTree n
    return
  transformed.addParLe(n.cursorTagId, n.info)                    # `(expr`
  n.into:
    while n.hasMore:
      if n.kind != TagLit or n.stmtKind != StmtsS:
        transformed.takeTree n         # not the leading stmts — pass through
        continue
      transformed.addParLe(n.cursorTagId, n.info)                # `(stmts`
      n.into:
        while n.hasMore:
          if n.kind != TagLit or n.stmtKind notin {LetS, VarS, CursorS}:
            transformed.takeTree n
            continue
          let info = n.info
          let k = n.stmtKind
          let local = takeLocal(n, SkipFinalParRi)
          let sym = local.name.symId
          let symInfo = local.name.info
          outerDest.addParLe(VarS, info)
          outerDest.addSymDef(sym, symInfo)
          outerDest.addSubtree local.exported
          if markNoinit:
            outerDest.addParLe(PragmasS, info)
            outerDest.addParLe(NoinitP, info)
            outerDest.addParRi()
            if local.pragmas.kind == TagLit:  # keep any original pragmas too
              var p = local.pragmas
              p = sub(p) # peek only, never left
              while p.hasMore:
                outerDest.addSubtree p
                skip p
            outerDest.addParRi()
          else:
            outerDest.addSubtree local.pragmas
          outerDest.addSubtree local.typ
          outerDest.addDotToken()      # uninitialised
          outerDest.addParRi()
          # The hoisted decl is written raw to `outerDest`, bypassing the
          # `trLocal` path that registers a local's type with typenav. A later
          # `getType` on the moved RHS — e.g. a closure-field callee lowered to
          # `(call (tupat sym 0) …)` — must resolve `sym`'s declared type, so
          # register it here or that lookup bugs ("could not find symbol").
          tc.registerLocal(sym, cast[SymKind](k), local.typ)
          if local.val.kind != DotToken:
            transformed.addParLe(AsgnS, info)
            transformed.addSymUse(sym, symInfo)
            transformed.addSubtree local.val
            transformed.addParRi()
        transformed.addParRi(n.endInfo)  # closing `)` of stmts
    transformed.addParRi(n.endInfo)      # closing `)` of expr

proc trOr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n, c.goal):
    # `x or y`  <=> `if x: true else: y` <=> `if x: tmp = true else: tmp = y`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    n.into:

      var aa = Target(m: IsEmpty)
      trExpr c, dest, n, aa
      # Hoist any leading let/var decls in the RHS's stmt-list-expr to outer
      # scope so they remain visible after the `or` lowering — same idea as
      # `trAnd` below; see the comment there.
      var rhs = createTokenBuf(16)
      hoistDeclsFromExprX(c.typeCache, dest, rhs, n, markNoinit = c.goal == TowardsFinalIr)
      var rhsCursor = beginRead(rhs)
      copyIntoKind dest, IfS, info:
        copyIntoKind dest, ElifU, info:
          dest.addTarget aa                # if x
          copyIntoKind dest, StmtsS, info:
            copyIntoKind dest, AsgnS, info: # tmp = true
              dest.addSymUse tmp, info
              copyIntoKind dest, TrueX, info: discard
        copyIntoKind dest, ElseU, info:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, rhsCursor, tmp # tmp = y
      tar.t.addSymUse tmp, info
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trAnd(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n, c.goal):
    # `x and y` <=> `if x: y else: false` <=> `if x: tmp = y else: tmp = false`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    n.into:

      var aa = Target(m: IsEmpty)
      trExpr c, dest, n, aa
      # Hoist any `let`/`var` decls that live inside the RHS's stmt-list-expr
      # to the outer `dest` (alongside `tmp`) so they remain in scope for the
      # surrounding `if` body. The hoisted decls become `var` placeholders
      # and the original initialiser is rewritten into an `asgn` that runs
      # only when `x` is true (preserving short-circuit evaluation).
      var rhs = createTokenBuf(16)
      hoistDeclsFromExprX(c.typeCache, dest, rhs, n, markNoinit = c.goal == TowardsFinalIr)
      var rhsCursor = beginRead(rhs)
      copyIntoKind dest, IfS, info:
        copyIntoKind dest, ElifU, info:
          dest.addTarget aa                # if x
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, rhsCursor, tmp # tmp = y
        copyIntoKind dest, ElseU, info:
          copyIntoKind dest, StmtsS, info:
            # tmp = false
            copyIntoKind dest, AsgnS, info:
              dest.addSymUse tmp, info
              copyIntoKind dest, FalseX, info: discard
      tar.t.addSymUse tmp, info
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trExprLoop(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if tar.m in {IsEmpty, IsBound}:
    tar.m = IsAppend
  else:
    assert tar.m == IsAppend, toString(n, false) & " " & $tar.m
  tar.t.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      trExpr c, dest, n, tar
  tar.t.addParRi()

proc trAggregateValue(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  ## Bind a *call* in a value-position of an aggregate to a fresh cursor temp
  ## so the call evaluates at a deterministic textual point relative to
  ## sibling pre-statements (e.g. a sibling's `wasMoved`). Non-call
  ## expressions are pure reads and are passed through to `trExpr`
  ## unchanged.
  ##
  ## **The temp is a `cursor`, not a `let`.** The aggregate constructor
  ## that immediately consumes this temp is the rightful owner of the
  ## call result; declaring the temp as `let` would tell the destroyer
  ## to inject `=destroy(tmp)` at scope end, which double-frees the
  ## value already moved into the aggregate (the aggregate's field has
  ## the only live owning reference). Cursor semantics: the temp is a
  ## non-owning view that goes out of scope without cleanup, which is
  ## exactly what xelim needs here. Surfaced 2026-05-01 by self-host
  ## debugging — see `bug_self_host_nifconfig_destroy.md`.
  if n.kind != TagLit or n.exprKind notin CallKinds:
    trExpr c, dest, n, tar
    return

  let info = n.info
  let typ = getType(c, n)

  var childTar = Target(m: IsBound)
  trExpr c, dest, n, childTar

  let tmp = pool.syms.getOrIncl(tempSymName(c))
  dest.addParLe CursorS, info
  dest.addSymDef tmp, info
  dest.addEmpty2 info  # export marker, pragmas
  dest.copyTree typ
  dest.addTarget childTar
  dest.addParRi()

  tar.t.addSymUse tmp, info

proc trAggregate(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  ## Aggregate constructors (object / tuple / array / set / bracket /
  ## newobj) evaluate their value-children in **unspecified** order at the
  ## C level, while xelim hoists pre-statements of any complex child to the
  ## enclosing statement. If a sibling produces a `wasMoved`/asgn pre-stmt
  ## that mutates a location an earlier sibling reads from, that earlier
  ## sibling sees corrupted state. Concrete witness: the duplifier emits
  ## `(let tmp = s; wasMoved s; tmp)` for the last read of `s`; an earlier
  ## `=dup(s)` left inline reads the cleared `s` because `wasMoved` runs
  ## first.
  ##
  ## Whenever the aggregate has at least one complex child, bind every
  ## non-literal value-position to a temp so the temps' assignments are
  ## the sequence points and pre-statements of later children always come
  ## *after* the values of earlier children.
  if not isComplex(n, c.goal):
    trExprLoop c, dest, n, tar
    return

  if tar.m in {IsEmpty, IsBound}:
    tar.m = IsAppend
  else:
    assert tar.m == IsAppend

  let kind = n.exprKind
  tar.t.addParLe(n.cursorTagId, n.info)
  n.into:

    case kind
    of OconstrX, NewobjX:
      # `(oconstr T (kv field val INTLIT?)*)` — also accepts a leading
      # inheritance form `(oconstr T (oconstr ...) (kv ...)*)`.
      if n.hasMore:
        tar.t.takeTree n  # T
      while n.hasMore:
        if n.isTagLit and n.substructureKind == KvU:
          tar.t.addParLe(n.cursorTagId, n.info)  # `(kv`
          n.into:
            if n.hasMore:
              tar.t.takeTree n  # field key
            if n.hasMore:
              trAggregateValue c, dest, n, tar
            while n.hasMore:
              tar.t.takeTree n  # optional INTLIT (inheritance count)
            tar.t.addParRi(n.endInfo)  # closing `)` of kv
        else:
          # Inheritance-style first-child: another constructor expression.
          trExpr c, dest, n, tar
    of TupconstrX, AconstrX:
      # `(tupconstr T X+)`, `(aconstr T X*)` — type then values.
      if n.hasMore:
        tar.t.takeTree n
      while n.hasMore:
        trAggregateValue c, dest, n, tar
    of TupX, BracketX, CurlyX, SetconstrX, TabconstrX:
      # `(tup X+)`, `(bracket X*)`, `(curly X*)`, `(setconstr X*)`,
      # `(tabconstr X*)` — value list, no leading type.
      while n.hasMore:
        trAggregateValue c, dest, n, tar
    else:
      while n.hasMore:
        trExpr c, dest, n, tar

    tar.t.addParRi()

proc trExprCall(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if tar.m in {IsAppend, IsEmpty} and c.goal in {LowerCasts, TowardsFinalIr}:
    # bind to a temporary variable:
    let info = n.info
    let typ = getType(c, n)

    if isVoidType(typ):
      # can happen for `quit` used inside an expression context.
      trExprLoop c, dest, n, tar
      return

    # Process the call into a temporary buffer so that any nested let
    # declarations are emitted before this one starts:
    var nestedDest = createTokenBuf(30)
    var callTarget = Target(m: IsBound)
    trExprLoop c, nestedDest, n, callTarget

    # Emit nested statements first
    dest.add nestedDest

    # Now create the let binding for this call
    let tmp = pool.syms.getOrIncl(tempSymName(c))
    # `call() = 4` via a `var T` cannot be bound to a let variable
    # as the analysis in constracts_njvl is too simplistic.
    # It would produce: "Cannot reassign a let variable".
    if typ.typeKind == MutT:
      dest.addParLe VarS, info
    else:
      dest.addParLe LetS, info
    dest.addSymDef tmp, info
    dest.addEmpty info # no export marker
    # Mark these temporaries as (inline) so that the analysis
    # in contracts_njvl remembers the value. This is necessary
    # for borrow checking which is defined on the original source
    # code expressions!
    dest.copyIntoKind PragmasS, info:
      dest.copyIntoKind InlineP, info: discard
    dest.copyTree typ
    dest.addTarget callTarget
    dest.addParRi()

    tar.t.addSymUse tmp, info
  else:
    trExprLoop c, dest, n, tar

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # IMPORTANT: Stores into `tar` helper!
  var tar = Target(m: IsAppend)
  tar.t.copyInto n:
    while n.hasMore:
      trExpr c, dest, n, tar
  dest.addTarget tar

proc trCond(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target)

proc condNodeSafe(n: Cursor): bool =
  var n = n
  case n.kind
  of TagLit:
    if n.exprKind in CallKinds: return false
    if n.exprKind == CastX:
      # A passthrough copies the subtree VERBATIM, so it also skips whatever
      # else the current goal rewrites. `LowerCasts` binds a cast's source and
      # result to variables (`trCast`) and the NIFC backends rely on it — a
      # `cast[float64](x)` that slips through unlowered reaches arkham as a
      # float bit-reinterpret it cannot emit. Casts are rare in conditions, so
      # just decline the passthrough for them.
      return false
    if n.exprKind == ExprX:
      # A single-son `(expr val)` is a transparent wrapper — e.g. the `!=`
      # template expands to `(expr (not (== x y)))`, which is pure. Walk into
      # it; a multi-son `(expr (stmts …) val)` carries real statements (a hoist
      # would be needed) and stays complex. Mirrors `isComplex`.
      var probe = n
      probe = sub(probe) # into `(expr`; peek only, never left
      skip probe                # the (would-be sole) value son
      if probe.hasMore: return false
    elif n.stmtKind in {IfS, CaseS, TryS, BlockS, WhileS, ForS, StmtsS}:
      return false
    n.loopInto:
      if not condNodeSafe(n): return false
      skip n
    result = true
  else:
    result = true

const
  CondPassthroughGoals = {ElimExprs, TowardsFinalIr, LowerCasts}
    ## Goals whose consumer compiles a condition with a *two-target* condition
    ## compiler, i.e. can turn `a and b` straight into branches. finalir has
    ## `Cx`; NIFC has C's `&&`/`||` (gcc) and arkham's `emitCondE` (native).
    ## For those, materialising a bool here is pure loss: `assert p != nil and
    ## rem > 0` became a temp + an if/else diamond + a re-test — ~90 NIFC tokens
    ## and 9 x86 instructions where two compare-and-branches suffice. It also
    ## inflated tiny accessors (`nifcore.load`, `kind`) past the inliner's
    ## 100-token bound, so they stayed real calls.

proc condPassthroughSafe(n: Cursor): bool =
  ## True if an `and`/`or` condition subtree contains only short-circuit nodes
  ## and *pure* leaves that finalir can emit inline — no calls and no
  ## statement-expressions with actual statements. Such a tree is handed to
  ## finalir verbatim so its two-target condition compiler (Cx) can lower it to
  ## shared `(lab)`/`(jmp)` merges (linear). A subtree with a call in a leaf must
  ## instead keep the bool-temp lowering here, because short-circuit evaluation
  ## requires the call to be hoisted *into* the branch, which Cx does not do.
  if n.kind != TagLit: return false
  result = condNodeSafe(n)

proc takeStrippingTrivialExpr(dest: var TokenBuf; n: var Cursor) =
  ## Copy the condition subtree at `n` into `dest`, dropping the brackets of any
  ## single-son `(expr val)` wrapper. The `!=`, `>=`, `>`, `notin`, … templates
  ## expand to exactly `(expr (not (== x y)))` etc.; keeping that wrapper leaves
  ## the finalir condition compiler and the contract/nil analysis staring at a
  ## statement-expression instead of the pure `not (== …)` leaf they understand.
  if n.kind == TagLit and n.exprKind == ExprX:
    var probe = n
    probe = sub(probe) # peek only, never left
    skip probe
    if not probe.hasMore:             # single son ⇒ transparent wrapper
      n.into:                         # drop `(expr` and the matching `)`
        takeStrippingTrivialExpr(dest, n)
      return
  if n.kind == TagLit:
    dest.addParLe(n.cursorTagId, n.info)                        # `(tag`
    n.into:
      while n.hasMore:
        takeStrippingTrivialExpr(dest, n)
      dest.addParRi(n.endInfo)        # `)`
  else:
    dest.takeTree n

proc trCond(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  assert tar.m == IsEmpty
  if n.exprKind in {AndX, OrX, NotX, ExprX} and c.goal in CondPassthroughGoals and
     condPassthroughSafe(n):
    # Hand the short-circuit tree to the backend untouched. This has to happen
    # in the FIRST xelim run too (`ElimExprs`): the later `LowerCasts` run never
    # sees an `and`, because xelim1 has already turned it into a bool temp.
    # Safe for the passes in between (duplifier/destroyer) precisely because
    # `condPassthroughSafe` admits no calls and no statement-expressions, so
    # there is nothing for the mover to sink into the wrong branch — the case
    # the `isComplex` comment above warns about is an `and` in a *value*
    # position, which still gets the bool-temp lowering.
    #
    # `NotX`/`ExprX` are in the trigger set because that is how the condition
    # actually arrives: `assert p != nil and rem > 0` is `(not (and (expr …)
    # (expr …)))`, and matching only `AndX` never fires. A pure `not`/`expr`
    # tree with no `and` inside copies verbatim, which is what the fall-through
    # did anyway.
    takeStrippingTrivialExpr(tar.t, n)
    return
  if c.goal in {LowerCasts, TowardsFinalIr}:
    case n.exprKind
    of AndX:
      if c.goal in CondPassthroughGoals and condPassthroughSafe(n):
        takeStrippingTrivialExpr(tar.t, n)
      else:
        trAnd c, dest, n, tar
    of OrX:
      if c.goal in CondPassthroughGoals and condPassthroughSafe(n):
        takeStrippingTrivialExpr(tar.t, n)
      else:
        trOr c, dest, n, tar
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
       InfX, NeginfX, NanX, FalseX, TrueX, XorX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, OconstrX, AconstrX, BracketX,
       CurlyX, CurlyatX, OvfX, AddX, SubX, MulX, DivX, ModX,
       ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX,
       LeX, LtX, CastX, ConvX, CallX, CmdX, CchoiceX, OchoiceX,
       PragmaxX, QuotedX, HderefX, DdotX, HaddrX, NewrefX,
       NewobjX, TupX, TupconstrX, SetconstrX, TabconstrX, AshrX,
       BaseobjX, HconvX, DconvX, CallstrlitX, InfixX, PrefixX,
       HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX,
       InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
       FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
       DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
       Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX, PlussetX,
       MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX,
       InsetX, CardX, EmoveX, DestroyX, DupX, CopyX, WasmovedX,
       SinkhX, TraceX, InternalTypeNameX, InternalFieldPairsX,
       FailedX, IsX, EnvpX, KvX, ToClosureX, NoExpr:
      trExpr c, dest, n, tar
  else:
    trExpr c, dest, n, tar

proc mayBindToTemp(n: Cursor): bool =
  ## True when a later pass could want to bind part of `n` to a temporary.
  ##
  ## `condNodeSafe` already rejects calls, casts and statement-expressions. On
  ## top of that a *constructing* expression passes `WantOwner` down to its
  ## operands, which is what makes the duplifier bind them (`bindToTemp` /
  ## `genLastRead`), so those count as impure here too.
  ##
  ## Two callers, one question. `trWhile` asks it to decide whether the loop
  ## condition has to move into the body as a leading guard: a temp hoisted in
  ## front of a condition still sitting in the `while`'s slot lands *outside*
  ## the loop and is evaluated once instead of once per iteration.
  ## `trCondJump` asks it to decide whether a condition leaf needs its own
  ## `(scope …)`: the temp is hoisted in front of the leaf's guard, and without
  ## the scope it would outlive the condition it belongs to.
  if not condNodeSafe(n): return true
  var n = n
  case n.kind
  of TagLit:
    if n.exprKind in {OconstrX, NewobjX, NewrefX, AconstrX, TupconstrX,
                      SetconstrX, TabconstrX}:
      return true
    n.loopInto:
      if mayBindToTemp(n): return true
      skip n
    result = false
  else:
    result = false

# ---------------------------------------------------------------------------
# Two-target condition compiler (`Cx`)
#
# `doc/final_ir.md`, *Short-circuit conditions*: a short-circuit condition is
# compiled against a pair of targets rather than materialised as a value. The
# fall-through is one of the two targets and costs nothing, so only the other
# one becomes a real `(jmp L)`; merges are shared `(lab L)`s rather than
# duplicated arms, which is what keeps `(a or b) and c` linear instead of
# exponential.
#
# This replaces `trAnd`/`trOr`'s bool temp for CONDITIONS. That lowering built
#
#   (var `x bool)
#   (if (elif a (stmts … (asgn `x …))) (else (stmts (asgn `x false))))
#   (if (elif `x <then>) (else <else>))
#
# — a store followed immediately by a re-test of the same slot, plus a second
# diamond. Recovering the branch the source actually wrote takes jump threading
# and a proof that `x` is dead afterwards, which is the work the optimizers
# were being asked to redo on every `and`. `Cx` never creates the slot.
# ---------------------------------------------------------------------------

proc freshLabel(c: var Context): SymId =
  result = pool.syms.getOrIncl("`L." & $c.counter)
  inc c.counter

proc addJmp(dest: var TokenBuf; lab: SymId; info: NifLineInfo) =
  copyIntoKind dest, JmpS, info:
    dest.addSymUse lab, info

proc addLab(dest: var TokenBuf; lab: SymId; info: NifLineInfo) =
  copyIntoKind dest, LabS, info:
    dest.addSymDef lab, info

proc transparentExpr(n: Cursor): bool =
  ## `(expr val)` with exactly one son — what `!=`, `>=`, `notin`, … expand to.
  ## A multi-son `(expr (stmts …) val)` carries statements and is a leaf.
  if n.kind != TagLit or n.exprKind != ExprX: return false
  var probe = n
  probe = sub(probe) # peek only, never left
  skip probe
  result = not probe.hasMore

proc condSpineHasShortCircuit(n: Cursor): bool =
  ## Is there an `and`/`or` on the condition's *spine*? An `and` buried in an
  ## operand (`f(a and b)`) is a value, not control flow, and is none of `Cx`'s
  ## business.
  let k = n.exprKind
  if k in {AndX, OrX}:
    result = true
  elif k == NotX or (k == ExprX and transparentExpr(n)):
    var ch = n
    ch = sub(ch)
    result = condSpineHasShortCircuit(ch)
  else:
    result = false

const
  CondJumpGoals = {ElimExprs, LowerCasts}
    ## Goals whose consumer is the ordinary hexer→Leng pipeline.
    ## `TowardsFinalIr` is excluded because `finalir.nim` carries its own
    ## condition compiler and wants the `and`/`or` tree.

proc wantsCondJumps(c: Context; n: Cursor): bool =
  ## `Cx` is for exactly the conditions that used to materialise a bool: a
  ## short-circuit spine that the backend cannot take verbatim. A spine of pure
  ## leaves still goes to the backend as `(and …)`/`(or …)` — C's `&&`/`||` and
  ## arkham's `emitCondE` compile it to the same branches without growing the
  ## function past the inliner's token budget (see `CondPassthroughGoals`).
  c.goal in CondJumpGoals and
    condSpineHasShortCircuit(n) and
    not (c.goal in CondPassthroughGoals and condPassthroughSafe(n))

template trExprToLabel(c: var Context; dest: var TokenBuf; n: var Cursor;
                       labArg: SymId; jumpIfTrueArg, conditionalArg: bool) =
  ## `trExpr` with an `IsLabel` target: emit statements into `dest` that
  ## transfer to `(lab labArg)` exactly when `n` evaluates to `jumpIfTrueArg`,
  ## and fall through otherwise. (The parameters carry the `Arg` suffix so they
  ## cannot shadow the `Target` field names in the constructor below.)
  var labelTar = Target(m: IsLabel, lab: labArg, jumpIfTrue: jumpIfTrueArg,
                        conditional: conditionalArg)
  trExpr c, dest, n, labelTar

proc trCondJump(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  ## The `IsLabel` mode of `trExpr`. `trExpr` dispatches here on its first line
  ## and nothing else calls it, so no other `tar.m` site in this file can
  ## observe the mode.
  ##
  ##   Cjmp(a and b)(T, true)  = Cjmp(a)(z, false); Cjmp(b)(T, true);  (lab z)
  ##   Cjmp(a and b)(F, false) = Cjmp(a)(F, false); Cjmp(b)(F, false)
  ##   Cjmp(a or  b)(T, true)  = Cjmp(a)(T, true);  Cjmp(b)(T, true)
  ##   Cjmp(a or  b)(F, false) = Cjmp(a)(z, true);  Cjmp(b)(F, false); (lab z)
  ##   Cjmp(not a) (X, p)      = Cjmp(a)(X, not p)      -- a target swap
  ##
  ## Short-circuiting is not a special case here, it is the layout: an
  ## operand's own pre-statements are emitted *after* the guard that can skip
  ## it, so they run only on the paths that reach it.
  ##
  ## `tar.conditional` says whether this subtree is one the short-circuit can
  ## skip. The leftmost leaf of a spine always runs, so it is emitted exactly
  ## as the old `trIf` emitted a condition — no hoisting, no extra scope. Only
  ## the operands *behind* a guard need the treatment in the leaf branch below.
  ##
  ## Everything that is not `and` / `or` / `not` is a leaf, and a leaf is just
  ## `trExpr` into a value plus the guarded transfer — Appel's `unCx(Ex)`
  ## coercion. That is why this dispatches with `if`/`elif` over the three
  ## interesting kinds instead of enumerating every `ExprKind`: the fall-back
  ## is one branch, not a second copy of `trExpr`'s dispatch that has to be
  ## kept in sync with it.
  let info = n.info
  let target = tar.lab
  let jumpIfTrue = tar.jumpIfTrue
  let conditional = tar.conditional
  let k = n.exprKind
  if k == AndX:
    if jumpIfTrue:
      let z = freshLabel(c)
      n.into:
        trExprToLabel c, dest, n, z, false, conditional
        trExprToLabel c, dest, n, target, true, true
      addLab dest, z, info
    else:
      n.into:
        trExprToLabel c, dest, n, target, false, conditional
        trExprToLabel c, dest, n, target, false, true
  elif k == OrX:
    if jumpIfTrue:
      n.into:
        trExprToLabel c, dest, n, target, true, conditional
        trExprToLabel c, dest, n, target, true, true
    else:
      let z = freshLabel(c)
      n.into:
        trExprToLabel c, dest, n, z, true, conditional
        trExprToLabel c, dest, n, target, false, true
      addLab dest, z, info
  elif k == NotX:
    n.into:
      trExprToLabel c, dest, n, target, not jumpIfTrue, conditional
  elif transparentExpr(n):
    n.into:
      trExprToLabel c, dest, n, target, jumpIfTrue, conditional
  else:
    # leaf: its pre-statements land here, then the guarded transfer
    let leafStart = n
    var t0 = Target(m: IsEmpty)
    var scoped = false
    if not conditional:
      # Always evaluated: emit exactly what the plain `trIf` path emitted.
      trExpr c, dest, n, t0
    else:
      # Behind a guard. A `let` written INSIDE such an operand — `if k and (let
      # cc = f(); cc != 0): use(cc)` — stays visible in the arm bodies, so its
      # declaration has to leave the operand. `hoistDeclsFromExprX` splits it
      # into an uninitialised decl emitted at this level and an `(asgn …)` left
      # behind, so the *assignment* stays on the short-circuited path while the
      # name outlives it. (This is what `trAnd`/`trOr` do to their RHS, for the
      # same reason.)
      var operand = createTokenBuf(16)
      hoistDeclsFromExprX(c.typeCache, dest, operand, n)
      var opCursor = beginRead(operand)
      var pre = createTokenBuf(16)
      trExpr c, pre, opCursor, t0
      # The operand's statements get their own `(scope …)`: they are reached
      # only on the paths that get this far, and their temporaries must die
      # with the condition. Without it `if a or f(): …` destroyed `f()`'s
      # result at the end of the *proc*. The scope is also the region the
      # destroyer unwinds when the guard below jumps out of it.
      #
      # `mayBindToTemp`: the temporary is usually NOT produced here — the
      # duplifier mints it later and hoists it in front of the guard statement.
      # The scope has to be in place by then for it to land inside.
      scoped = pre.len > 0 or mayBindToTemp(leafStart)
      if scoped:
        dest.addParLe(ScopeS, info)
      dest.add pre
    copyIntoKind dest, IfS, info:
      copyIntoKind dest, ElifU, info:
        if jumpIfTrue:
          dest.addTarget t0
        else:
          copyIntoKind dest, NotX, info:
            dest.addTarget t0
        copyIntoKind dest, StmtsS, info:
          addJmp dest, target, info
    if scoped:
      dest.addParRi()

proc ifNeedsCondJumps(c: Context; n: Cursor): bool =
  ## Does any arm of this `if` carry a short-circuit condition that would
  ## otherwise materialise a bool? One is enough — the whole statement then
  ## goes to the flat layout, which also folds the `elif` chain for free.
  var it = n
  it = sub(it)
  while it.hasMore:
    if it.substructureKind == ElifU:
      var cond = it
      cond = sub(cond)
      if wantsCondJumps(c, cond): return true
    skip it
  result = false

proc trBranchBody(c: var Context; dest: var TokenBuf; n: var Cursor;
                  tar: Target; tmp: SymId) =
  ## One arm's body, emitted WITHOUT a scope of its own — the caller has
  ## already opened one that spans the arm's condition *and* its body (see
  ## `trIfFlat`).
  if tar.m != IsIgnored:
    trExprInto c, dest, n, tmp
  elif n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        trStmt c, dest, n
  else:
    trStmt c, dest, n

proc trIfFlat(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  ## `if c1: B1 elif c2: B2 else: B3` laid out as
  ##
  ##   (scope Cjmp(c1)(L1, false) B1); (jmp Lend)
  ##   (lab L1); (scope Cjmp(c2)(L2, false) B2); (jmp Lend)
  ##   (lab L2); (scope B3)
  ##   (lab Lend)
  ##
  ## Every merge is a shared `(lab)`, no arm is duplicated, and the `elif`
  ## chain needs no nesting — the flat form the optimizers can read directly.
  ##
  ## An arm's `(scope …)` covers its **condition and its body together**, and
  ## that is not cosmetic. A condition may declare a local (`elif (let d =
  ## load(x); d.ok):`), and in a flat layout a declaration sits in the same
  ## block as every other arm while only *some* paths reach it — so the
  ## destroyer's scope-exit `=destroy` would run on a slot that was never
  ## initialised. Entering the scope only on the paths that evaluate the
  ## condition is what keeps declaration, initialisation and destruction on
  ## the same set of paths. The guard's `(jmp Lnext)` leaves the scope, and
  ## `destroyer.trJmp` unwinds it on the way out.
  let info = n.info
  var tmp = SymId(0)
  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  # Emitted in one streaming pass, straight into `dest`: an arm falls out of
  # its `(scope …)` into `(jmp endLab)`, and the arm after it starts at the
  # false label the guard jumped to. The one thing an arm needs to know about
  # its successors is whether it *has* any: a last arm with no `else` falls
  # straight out of the `if`, so its false edge IS the end — it can share
  # `endLab` instead of planting a second label right next to it, and it needs
  # no `(jmp endLab)` either. One `skip` answers that, and `skip` is O(1) (the
  # tag token carries the subtree's jump), so the arms need neither a pre-pass
  # over the chain nor a `TokenBuf` each to be assembled from afterwards.
  let endLab = freshLabel(c)
  var needEndLab = false

  n.into:
    while n.hasMore:
      let binfo = n.info
      case n.substructureKind
      of ElifU:
        var lookahead = n
        skip lookahead
        let falseLab = if lookahead.hasMore: freshLabel(c) else: endLab
        dest.addParLe(ScopeS, binfo)
        c.typeCache.openScope()
        n.into:
          trExprToLabel c, dest, n, falseLab, false, false
          trBranchBody c, dest, n, tar, tmp
        c.typeCache.closeScope()
        dest.addParRi()
        if falseLab != endLab:
          addJmp dest, endLab, info
          needEndLab = true
        addLab dest, falseLab, info
      of ElseU:
        # Reached only through the preceding arm's false label, which was
        # already planted; the `if` ends here, so `endLab` lands after it.
        dest.addParLe(ScopeS, binfo)
        c.typeCache.openScope()
        n.into:
          trBranchBody c, dest, n, tar, tmp
        c.typeCache.closeScope()
        dest.addParRi()
        if needEndLab:
          addLab dest, endLab, info
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
         TypevarU, StaticTypevarU, EfldU, FldU, WhenU, TypevarsU, CaseU, OfU,
         StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
         UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU,
         ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
        # Bug: just copy the thing around
        takeTree dest, n

  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, n.endInfo

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # if cond: a elif condB: b else: c
  # -->
  # if cond: a else: (if condB: b else: c)
  if ifNeedsCondJumps(c, n):
    trIfFlat c, dest, n, tar
    return

  let info = n.info
  let head = n
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var toClose = 0
  var ifs = 0
  n.into:
    while n.hasMore:
      if ifs >= 1:
        dest.addParLe ElseU, info
        dest.addParLe StmtsS, info
        inc toClose, 2

      let info = n.info
      case n.substructureKind
      of ElifU:
        var t0 = Target(m: IsEmpty)
        n.into:
          trCond c, dest, n, t0

          dest.addParLe(head.cursorTagId, head.info)
          inc toClose
          inc ifs

          copyIntoKind dest, ElifU, info:
            dest.addTarget t0
            #copyIntoKind dest, StmtsS, info:
            if tar.m != IsIgnored:
              copyIntoKind dest, StmtsS, info:
                trExprInto c, dest, n, tmp
            else:
              trStmt c, dest, n
      of ElseU:
        n.into:
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
         TypevarU, StaticTypevarU, EfldU, FldU, WhenU, TypevarsU, CaseU, OfU,
         StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
         UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU,
         ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
        # Bug: just copy the thing around
        takeTree dest, n

  while toClose > 0:
    dest.addParRi()
    dec toClose

  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var t0 = Target(m: IsEmpty)
  n.into:
    trExpr c, dest, n, t0
    dest.addParLe CaseS, info
    dest.addTarget t0
    while n.hasMore:
      case n.substructureKind
      of OfU:
        copyInto(dest, n):
          takeTree dest, n # choices
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of ElseU:
        copyInto(dest, n):
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
         TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
         StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
         UnpacktupU, ExceptU, FinU, UncheckedU, GfldU, CallargsU,
         ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
        # Bug: just copy the thing around
        takeTree dest, n
    dest.addParRi(n.endInfo)
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n

    while n.hasMore:
      case n.substructureKind
      of ExceptU:
        copyInto(dest, n):
          takeTree dest, n # declarations
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of FinU:
        # The `finally` section never produces a value!
        copyInto(dest, n):
          trStmt c, dest, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
         TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, ElseU, TypevarsU,
         CaseU, OfU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
         UnpackflatU, UnpacktupU, UncheckedU, GfldU, CallargsU,
         ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
        # Bug: just copy the thing around
        takeTree dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.copyInto n:
    # `mayBindToTemp(n)`: an IMPURE condition becomes
    # `while true: <cond stmts>; if cond: body else: break` — the Final-IR
    # "loop has no condition, the condition is a leading body guard" shape
    # (`doc/final_ir.md`). It is not cosmetic: it is what makes "hoist a
    # sub-expression to the statement in front of it" unconditionally sound for
    # every later pass. With the condition left in the slot, a pass that needs a
    # temp for part of it (the duplifier's `bindToTemp`, the eraiser's
    # `canRaise` temp) would have to hoist that temp OUT of the loop, where it
    # is evaluated once instead of once per iteration — which is why those
    # passes used to wrap it into an `(expr (stmts ...) tmp)` and leave the
    # flattening to a follow-up xelim run instead.
    if isComplex(n, c.goal) or mayBindToTemp(n):
      dest.copyIntoKind TrueX, info: discard
      copyIntoKind dest, StmtsS, info:
        if wantsCondJumps(c, n):
          # Same deal as `trIfFlat`: a short-circuit guard compiles against a
          # jump target instead of a bool the `if` below would immediately
          # re-test. The exit is the FALL-THROUGH and the body is behind the
          # jump, because the only way out of a loop is `(break)` and a label
          # cannot be placed after the body without the fall-off-the-body edge
          # landing on it too:
          #
          #   Cjmp(cond)(bodyLab, true); (break); (lab bodyLab); <body>
          #
          # Everything stays inside the loop body's `(stmts …)`, so the jumps
          # are the same intra-list shape `trIfFlat` emits — no transfer ever
          # leaves the loop.
          let bodyLab = freshLabel(c)
          trExprToLabel c, dest, n, bodyLab, true, false
          dest.copyIntoKind BreakS, info:
            dest.addDotToken()
          addLab dest, bodyLab, info
          trStmt c, dest, n
        else:
          var tar = Target(m: IsEmpty)
          trCond c, dest, n, tar
          dest.copyIntoKind IfS, info:
            dest.copyIntoKind ElifU, info:
              dest.addTarget tar
              trStmt c, dest, n
            dest.copyIntoKind ElseU, info:
              copyIntoKind dest, StmtsS, info:
                dest.copyIntoKind BreakS, info:
                  dest.addDotToken()
    else:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      dest.addTarget tar
      trStmt c, dest, n

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let head = n.load()
  n.into:
    var tar = Target(m: IsEmpty)
    trExpr c, dest, n, tar # iterator call
    dest.addParLe(head.tagId, info)
    dest.addTarget tar
    takeTree dest, n # for loop variables
    trStmt c, dest, n
    dest.addParRi(n.endInfo)

proc trCoroFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## The iter call is consumed entirely by cps.nim's trCoroFor (it is
  ## rewritten to a wrapper call with extra args). Don't extract its result
  ## into a temp here — keep it verbatim so cps sees its original shape.
  let info = n.info
  let head = n.load()
  n.into:
    dest.addParLe(head.tagId, info)
    takeTree dest, n # iter call, verbatim
    trStmt c, dest, n # body
    dest.addParRi(n.endInfo)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var tmp = createTokenBuf(30)
  let kind = n.symKind
  copyInto tmp, n:
    let name = n.symId
    takeTree tmp, n # name
    takeTree tmp, n # export marker
    takeTree tmp, n # pragmas
    c.typeCache.registerLocal(name, kind, n)
    takeTree tmp, n # type
    var v = Target(m: IsBound)
    trExpr c, dest, n, v
    tmp.addTarget v
  dest.add tmp

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  let kind = n.symKind
  # An `{.assembler.}` body is a transliteration: source order IS the contract,
  # so no pass may hoist a subexpression into a temporary or otherwise reorder
  # it — the whole point is that every construct maps one-to-one to an
  # instruction. Take such a body verbatim and let the back end check it (see
  # `nativenif/doc/intrinsics.md` §8).
  let isAsm = hasPragma(asRoutine(decl, SkipExclBody).pragmas, AssemblerP)
  copyInto dest, n:
    let symId = n.symId
    let isConcrete = takeRoutineHeader(c.typeCache, dest, decl, n)
    if isConcrete and not isAsm:
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, kind, decl)
      c.typeCache.openScope()
      trStmt c, dest, n
      c.typeCache.closeScope()
    else:
      takeTree dest, n

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    takeTree dest, n # label or DotToken
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, n.info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n

  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, n.endInfo # `n` is already past the block

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.exprKind == ExprX:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      if tar.m == IsAppend:
        dest.addTarget tar
    else:
      takeTree dest, n
  of PragmaxS:
    copyInto(dest, n):
      takeTree dest, n  # pragmas
      trStmt c, dest, n  # body
  of IfS, WhenS:
    var tar = Target(m: IsIgnored)
    trIf c, dest, n, tar
  of CaseS:
    var tar = Target(m: IsIgnored)
    trCase c, dest, n, tar
  of TryS:
    var tar = Target(m: IsIgnored)
    trTry c, dest, n, tar

  of RetS, RaiseS, YldS:
    var tar = Target(m: IsEmpty)
    let head = n
    n.into:
      trExpr c, dest, n, tar
      dest.addParLe(head.cursorTagId, head.info)
      dest.addTarget tar
      dest.addParRi()

  of DiscardS:
    let head = n
    n.into:
      if c.goal in {LowerCasts, TowardsFinalIr}:
        if n.isDotToken:
          dest.takeTree n
        else:
          let typ = getType(c, n)
          var tar = Target(m: IsBound)
          trExpr c, dest, n, tar
          # we must bind the result to a temporary variable!
          let tmp = pool.syms.getOrIncl("`x." & $c.counter)
          inc c.counter
          let info = n.endInfo # the discard operand is consumed: `n` is at
                               # the (possibly elided) close
          dest.addParLe LetS, info
          dest.addSymDef tmp, info
          dest.addEmpty2 info # no export marker, no pragmas
          dest.copyTree typ
          dest.addTarget tar
          dest.addParRi()
      else:
        var tar = Target(m: IsEmpty)
        trExpr c, dest, n, tar
        dest.addParLe(head.cursorTagId, head.info)
        dest.addTarget tar
        dest.addParRi()

  of WhileS:
    trWhile c, dest, n
  of ForS:
    trFor c, dest, n
  of CoroforS:
    trCoroFor c, dest, n
  of CallKindsS, InclS, ExclS:
    trStmtCall c, dest, n
  of AsgnS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    # Peek at the LHS: if it is the `result` variable, do not extract a
    # call on the RHS to a temporary.  nj.nim's trAsgn handles the call
    # directly via trBoundExpr and emits the "was successful?" branching
    # after the store, which is both simpler and avoids borrow-checking
    # trouble caused by the extra temporary.
    # `lhsIsResult` is the shortcut that keeps the call in place when the lhs
    # is already a sym; both nj.nim and finalir.nim handle it via
    # `trBoundExpr` (a call binds directly to its destination — doc/final_ir.md).
    # `LowerCasts` always binds — the dce2 inliner wants every call to appear
    # as the value of a let/var binding.
    var lhsIsResult = false
    if c.goal == TowardsFinalIr:
      let peek = n.childCursor
      lhsIsResult = peek.kind == Symbol
    tar.t.copyInto n:
      trExpr c, dest, n, tar
      if c.goal in {LowerCasts, TowardsFinalIr}:
        if c.goal == TowardsFinalIr and lhsIsResult:
          tar.m = IsBound
        # else: tar.m stays IsAppend so trExprCall can bind
        trExpr c, dest, n, tar
      else:
        tar.m = IsBound
        trExpr c, dest, n, tar
    dest.addTarget tar

  of AsmS, DeferS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyInto n:
      while n.hasMore:
        trExpr c, dest, n, tar
    dest.addTarget tar
  of LocalDecls:
    trLocal c, dest, n
  of ProcS, FuncS, MethodS, ConverterS, IteratorS:
    trProc c, dest, n
  of BlockS:
    var tar = Target(m: IsIgnored)
    trBlock c, dest, n, tar
  of MacroS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
     IncludeS, ImportS, FromimportS, ImportexceptS,
     ExportS, CommentS, AssumeS, AssertS,
     PragmasS, ImportasS, ExportexceptS, BindS, MixinS, UsingS,
     LabS, JmpS:
    # `lab`/`jmp` are atoms for xelim: it PRODUCES them (`trCondJmp`) and never
    # has to look inside one — the operand is a label symbol.
    takeTree dest, n
  of ScopeS, StaticstmtS:
    c.typeCache.openScope()
    copyInto(dest, n):
      while n.hasMore:
        trStmt c, dest, n
    c.typeCache.closeScope()
  of StmtsS, UnpackdeclS:
    copyInto(dest, n):
      while n.hasMore:
        trStmt c, dest, n

proc isIntLike(tk: TypeKind): bool {.inline.} =
  tk in {IntT, UIntT, CharT, BoolT}

proc needsBitCast(destType: Cursor; srcType: Cursor): bool =
  ## Returns true when the cast requires memcpy for bit reinterpretation.
  ## Integer-to-integer and float-to-float casts can use a plain C cast.
  ## Integer-to-float (and vice versa) needs memcpy.
  let dtk = typeKind(destType)
  let stk = typeKind(srcType)
  if dtk == FloatT and stk == FloatT: return false
  if isIntLike(dtk) and isIntLike(stk): return false
  # One is float, the other is integer-like (or both are value types of
  # different families): need memcpy for correct bit reinterpretation.
  result = dtk in {IntT, UIntT, FloatT, CharT, BoolT} and
           stk in {IntT, UIntT, FloatT, CharT, BoolT}

proc trCast(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  let castStart = n # skip "cast" tag
  n = sub(n)

  var destTypeBuf = createTokenBuf(8)
  takeTree destTypeBuf, n # copy dest type, n now at srcExpr
  let destType = beginRead(destTypeBuf)

  let dtk = typeKind(destType)
  # Quick check: if dest is not a value type, skip getType on source entirely
  if dtk notin {IntT, UIntT, FloatT, CharT, BoolT}:
    var srcTarget = Target(m: IsEmpty)
    trExpr c, dest, n, srcTarget
    n = castStart; skip n
    tar.t.addParLe CastX, info
    tar.t.addSubtree destType
    tar.t.addTarget srcTarget
    tar.t.addParRi()
    return

  let srcType = getType(c, n)
  if not needsBitCast(destType, srcType):
    # Same-family cast (e.g. int-to-int) - use plain C cast
    var srcTarget = Target(m: IsEmpty)
    trExpr c, dest, n, srcTarget
    n = castStart; skip n
    tar.t.addParLe CastX, info
    tar.t.addSubtree destType
    tar.t.addTarget srcTarget
    tar.t.addParRi()
    return

  # Cross-family value type cast (e.g. int↔float):
  # lower to copyMem(addr dest, addr src, sizeof(DstType))
  var srcTarget = Target(m: IsEmpty)
  trExpr c, dest, n, srcTarget
  n = castStart; skip n

  # Ensure source is a variable
  var srcSym: SymId
  var srcCur = beginRead(srcTarget.t)
  if srcCur.kind == Symbol:
    srcSym = srcCur.symId
  else:
    srcSym = pool.syms.getOrIncl(tempSymName(c))
    copyIntoKind dest, VarS, info:
      dest.addSymDef srcSym, info
      dest.addDotToken() # export marker
      dest.copyIntoKind PragmasS, info:
        dest.copyIntoKind InlineP, info: discard
      copyTree dest, srcType
      dest.addTarget srcTarget # value

  # Create dest variable (uninitialized)
  let dstSym = pool.syms.getOrIncl(tempSymName(c))
  copyIntoKind dest, VarS, info:
    dest.addSymDef dstSym, info
    dest.addDotToken() # export marker
    dest.copyIntoKind PragmasS, info:
      dest.copyIntoKind InlineP, info: discard
    dest.addSubtree destType
    dest.addDotToken() # no initializer

  # Emit: copyMem(addr dstSym, addr srcSym, sizeof(DstType))
  let copyMemSym = pool.syms.getOrIncl("copyMem.0." & SystemModuleSuffix)
  copyIntoKind dest, CallX, info:
    dest.addSymUse copyMemSym, info
    dest.copyIntoKind AddrX, info:
      dest.addSymUse dstSym, info
    dest.copyIntoKind AddrX, info:
      dest.addSymUse srcSym, info
    dest.copyIntoKind SizeofX, info:
      dest.addSubtree destType

  tar.t.addSymUse dstSym, info

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # can have the dangerous `Expr` node which is the whole
  # reason for xelim's existence.
  if tar.m == IsLabel:
    # `Cx`: compiled against a jump target rather than materialised into a
    # value. `trCondJump` handles the mode completely and never passes it on,
    # so the rest of this file only ever sees the value-producing modes.
    trCondJump c, dest, n, tar
    return
  case n.kind
  of DotToken, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StrLit:
    takeTree tar.t, n
  of TagLit:
    case n.exprKind
    of ExprX:
      n.into:
        while n.hasMore:
          if not isLastSon(n):
            trStmt c, dest, n
          else:
            trExpr c, dest, n, tar
    of AndX:
      trAnd c, dest, n, tar
    of OrX:
      trOr c, dest, n, tar
    of CallKinds:
      trExprCall c, dest, n, tar
    of CastX:
      if c.goal == LowerCasts:
        trCast c, dest, n, tar
      else:
        trExprLoop c, dest, n, tar
    of OconstrX, NewobjX, TupconstrX, TupX, AconstrX, BracketX,
       CurlyX, SetconstrX, TabconstrX:
      trAggregate c, dest, n, tar
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
       InfX, NeginfX, NanX, FalseX, TrueX, XorX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, CurlyatX, OvfX, AddX, SubX, MulX,
       DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
       BitnotX, EqX, NeqX, LeX, LtX, ConvX, CchoiceX,
       OchoiceX, PragmaxX, QuotedX, HderefX, DdotX, HaddrX,
       NewrefX,
       AshrX, BaseobjX, HconvX, DconvX, CompilesX,
       DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, HighX, LowX,
       TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
       IsmainmoduleX, DefaultobjX, DefaulttupX,
       DefaultdistinctX, Delay0X, SuspendX, DoX, ArratX, TupatX,
       PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX,
       LtsetX, InsetX, CardX, EmoveX, DestroyX, DupX, CopyX,
       WasmovedX, SinkhX, TraceX, InternalTypeNameX,
       InternalFieldPairsX, FailedX, IsX, EnvpX, KvX, ToClosureX, NoExpr:
      # `if`/`case`/`try`/`block` bind their value to a temp unless the
      # target is `IsIgnored`. A void-typed one has no value to bind, and
      # `declareTemp` would emit `(var :tmp . . .)` with an empty type slot,
      # which lengcgen prints as `void tmp;`. Reaching here with a void type
      # is normal: lambdalifting wraps a closure call in
      # `(expr (stmts ...) (if ...))` to split on `env == nil`, so a
      # `proc()` call lands in expression position. `trExprCall` already
      # guards the same case for plain calls.
      if n.stmtKind in {IfS, CaseS, TryS, BlockS} and isVoidType(getType(c, n)):
        tar.m = IsIgnored
      case n.stmtKind
      of IfS:
        trIf c, dest, n, tar
      of CaseS:
        trCase c, dest, n, tar
      of TryS:
        trTry c, dest, n, tar
      of BlockS:
        trBlock c, dest, n, tar
      of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS,
         GletS, TletS, LetS, CursorS, PatternvarS, ProcS, FuncS,
         IteratorS, ConverterS, MethodS, MacroS, TemplateS,
         TypeS, EmitS, AsgnS, ScopeS, WhenS, BreakS, ContinueS,
         ForS, WhileS, CoroforS, RetS, YldS, StmtsS, PragmasS,
         PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
         FromimportS, ImportexceptS, ExportS, ExportexceptS,
         CommentS, DiscardS, RaiseS, UnpackdeclS, AssumeS,
         AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
         StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS,
         LabS, JmpS, NoStmt:
        trExprLoop c, dest, n, tar
  else:
    bug "unexpected ')' inside"

proc preRegisterRoutines(c: var Context; n: Cursor) =
  ## Register the signatures of all top-level routines in the type cache before
  ## the body walk. `getType` on a forward-referenced call otherwise falls back
  ## to `tryLoadSym` (the *original* sem output), which still holds the
  ## pre-lambdalifting types — e.g. a bare closure `proctype` where the lifted
  ## `(tuple <fn> (ref RootObj))` is expected. That is exactly what happens for
  ## the sem-inlined openarrays helpers (`toOpenArray`, `rawData`, `\5B\5D`)
  ## in tests/nimony/closures/…: the body's forward `(call rawData)` hoists
  ## into a cursor temp typed from the stale symbol, while the lifted `rawData`
  ## ret type is the tuple — the lengcgen C names then disagree.
  var it = n.sub()
  while it.hasMore:
    if it.stmtKind == StmtsS:
      preRegisterRoutines(c, it)
    elif it.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
      let decl = it
      var h = it.sub()
      let name = h
      if name.kind == SymbolDef:
        for i in 0 ..< BodyPos:
          if i == ParamsPos:
            c.typeCache.registerParams(name.symId, decl, h)
          skip h
    skip it

proc lowerExprs*(pass: var Pass; goal = ElimExprs) =
  var n = pass.n  # Extract cursor locally
  # Inherit the temp counter across passes via `pass.nextTemp` — `lowerExprs`
  # runs three times in `pipeline.transform` (xelim1, xelim2, xelim_final);
  # restarting from 0 each time produces colliding `\`x.<n>` SymIds whose
  # Lengc-emitted C names clash within a single function. `pool.syms.getOrIncl`
  # is identity-by-name, so two semantically distinct temps would otherwise
  # share an identifier.
  var c = Context(counter: pass.nextTemp, typeCache: createTypeCache(pass.bits), thisModuleSuffix: pass.moduleSuffix, goal: goal)
  c.typeCache.openScope()
  assert n.stmtKind == StmtsS, $n.kind
  preRegisterRoutines(c, n)
  pass.dest.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      trStmt c, pass.dest, n
  pass.dest.addParRi()
  c.typeCache.closeScope()
  pass.nextTemp = c.counter
  #echo "PRODUCED: ", pass.dest.toString(false)

when isMainModule:
  var owningBuf = createTokenBuf(300)
  let n = setupProgram("debug.txt", "debug.out", owningBuf)
  # A standalone debug driver: no target, so the host's width is stated.
  var pass = initPass(move owningBuf, "main", "xelim", sizeof(int)*8)
  lowerExprs(pass)
  echo pass.dest.toString(false)

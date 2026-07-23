# included in sem.nim

const LocalSigKinds = {LetY, VarY, GletY, GvarY, TletY, TvarY}
  ## let/var-family locals that get a signature-phase pass at toplevel
  ## (explicit-type only). See `localSigGuard` / nim-lang/nimony#1974.

proc signaturesMatch(forwardDecl: Cursor; implDecl: Cursor): bool =
  ## Check if two routine declarations have compatible signatures.
  ## Compares NIF tokens directly, mapping symbols back to identifiers.
  if forwardDecl.kind != ParLe or implDecl.kind != ParLe:
    return false
  let fwd = asRoutine(forwardDecl)
  let impl = asRoutine(implDecl)
  # Compare generic params (typevars)
  if not sameTreesButIgnoreSymIds(fwd.typevars, impl.typevars):
    return false
  # Compare params
  if not sameTreesButIgnoreSymIds(fwd.params, impl.params):
    return false
  # Compare return type
  if not sameTreesButIgnoreSymIds(fwd.retType, impl.retType):
    return false
  return true

proc addForwardDecl*(c: var SemContext; symId: SymId) =
  ## Register a forward declaration candidate.
  let lit = symToIdent(symId)
  c.forwardDecls.mgetOrPut(lit, @[]).add symId

proc findMatchingForwardDecl*(c: var SemContext; symId: SymId; implDecl: Cursor): SymId =
  ## Find a forward declaration with matching signature.
  ## Only checks the forwardDecls set, not the full scope.
  result = NoSymId
  let lit = symToIdent(symId)
  let candidates = c.forwardDecls.getOrDefault(lit)
  for fwdSym in candidates:
    if fwdSym == symId:
      continue  # skip self
    let res = tryLoadSym(fwdSym)
    if res.status == LacksNothing:
      if signaturesMatch(res.decl, implDecl):
        result = fwdSym
        return

proc processBodyStatements(c: var SemContext; dest: var TokenBuf; it: var Item;
                           lastSonInfo: var PackedLineInfo; beforeLastSon: var int) =
  ## Process all statements in the proc body, treating the last one as an expression.
  while it.n.hasMore:
    if not isLastSon(it.n):
      semStmt c, dest, it.n, false
    else:
      beforeLastSon = dest.len
      lastSonInfo = it.n.info
      semExpr c, dest, it, {AllowEmpty}

proc handleTemplateReturnType(c: var SemContext; dest: var TokenBuf; it: var Item;
                              lastSonInfo: PackedLineInfo; beforeLastSon: int) =
  ## Handle return type checking for templates.
  case c.routine.returnType.typeKind
  of UntypedT:
    discard "ok"
  of VoidT:
    typecheck(c, dest, lastSonInfo, it.typ, c.routine.returnType)
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, PtrT, ArrayT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
     TypedT, CstringT, PointerT, OrdinalT:
    commonType c, dest, it, beforeLastSon, c.routine.returnType

proc handleProcReturnType(c: var SemContext; dest: var TokenBuf; it: var Item;
                          lastSonInfo: PackedLineInfo; beforeLastSon: int) =
  ## Handle return type for regular procedures, transforming `expr` to `result = expr` if needed.
  if classifyType(c, it.typ) in {VoidT, UntypedT}:
    discard "ok"
  elif c.routine.resId != SymId(0):
    commonType c, dest, it, beforeLastSon, c.routine.returnType
    var prefix = [
      parLeToken(AsgnS, lastSonInfo),
      symToken(c.routine.resId, lastSonInfo)]
    dest.insert prefix, beforeLastSon
    dest.addParRi()
  else:
    commonType c, dest, it, beforeLastSon, c.routine.returnType

proc semProcBody(c: var SemContext; dest: var TokenBuf; itB: var Item) =
  ## Consumes the rest of the body's `(stmts)` scope; the caller entered it
  ## and closes it (see `withProcBody`).
  var it = Item(n: itB.n, typ: c.types.autoType)
  var lastSonInfo = itB.n.info
  var beforeLastSon = dest.len
  processBodyStatements(c, dest, it, lastSonInfo, beforeLastSon)
  if c.routine.kind == TemplateY:
    handleTemplateReturnType(c, dest, it, lastSonInfo, beforeLastSon)
  else:
    handleProcReturnType(c, dest, it, lastSonInfo, beforeLastSon)
  itB.n = it.n


proc exportMarkerBecomesNifTag(c: var SemContext; dest: var TokenBuf; insertPos: int; crucial: CrucialPragma) =
  assert crucial.magic.len > 0
  let info = dest[insertPos].info

  # built through `add` so the tree is properly sealed — a raw token array
  # carries no jump and `replace` would splice a broken subtree
  var nifTag = createTokenBuf(4)
  nifTag.add parLeToken(pool.tags.getOrIncl(crucial.magic), info)
  if crucial.bits != 0:
    nifTag.add intToken(pool.integers.getOrIncl(crucial.bits), info)
  nifTag.addParRi(info)
  dest.replace cursorAt(nifTag, 0), insertPos

proc semLocalValue(c: var SemContext; dest: var TokenBuf; it: var Item; crucial: CrucialPragma) =
  if ThreadvarP in crucial.flags:
    c.buildErr dest, it.n.info, "a `threadvar` cannot have an init value"
    skip it.n
  else:
    semExpr c, dest, it

proc isStaticConstraint(n: Cursor): bool =
  ## Detects the `static[T]` / `static T` / bare `static` constraint forms of a
  ## generic parameter before semchecking: nifler keeps `static` as a plain
  ## identifier in `static[T]` (giving `(at static T)`) but produces a
  ## `(static T)` type for the prefix form `static T`.
  if n.typeKind == StaticT: return true
  var n = n
  if n.exprKind == AtX: inc n
  n.kind == Ident and pool.strings[n.litId] == "static"

proc isStaticSugarHead(n: Cursor): bool =
  ## True when the `AtX` cursor `n` is the `static[T]` sugar `(at static T)`,
  ## i.e. its head is the literal `static` identifier — as opposed to an
  ## already-desugared element type that is itself an invocation such as
  ## `Shape[N]` (`(at Shape N)`).
  var head = n
  inc head # past `at`
  result = head.kind == Ident and pool.strings[head.litId] == "static"

proc semStaticTypevarType(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  ## Sem the element type of a value generic parameter (`N: static[int]`). The
  ## type slot of the resulting `staticTypevar` holds the plain element type;
  ## a `static` wrapper never enters the checked type system (issue #2089).
  let info = n.info
  if n.typeKind == StaticT:
    n.into:
      if not n.hasMore:
        c.buildErr dest, info, "`static` requires an explicit element type, e.g. `static[int]`"
      else:
        discard semLocalType(c, dest, n)
  elif n.exprKind == AtX and isStaticSugarHead(n):
    # the `static[T]` sugar: nifler renders it as `(at static T)`. An `AtX` whose
    # head is *not* the `static` identifier is an already-desugared element type
    # that is itself a generic instantiation (e.g. `Shape[N]` -> `(at Shape N)`),
    # handled by the `else` branch below.
    n.into:
      skip n # the `static` identifier
      if not n.hasMore:
        c.buildErr dest, info, "`static` requires an explicit element type, e.g. `static[int]`"
      else:
        discard semLocalType(c, dest, n)
        if n.hasMore:
          c.buildErr dest, info, "`static` takes a single element type"
          while n.hasMore: skip n
  elif n.kind == Ident and pool.strings[n.litId] == "static":
    # bare `static` identifier
    skip n
    c.buildErr dest, info, "`static` requires an explicit element type, e.g. `static[int]`"
  else:
    # already desugared (re-semming a `staticTypevar` declaration): the slot
    # holds the plain element type
    discard semLocalType(c, dest, n)

proc semLocal(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  var kind = kind
  if kind == TypevarY:
    # `N: static[int]` declares a *value* generic parameter. Detect the
    # `static` constraint before the symbol is declared so that both the
    # symbol kind and the declaration tag become `staticTypevar`.
    var constraint = n
    inc constraint # tag
    skip constraint # name
    skip constraint # export marker
    skip constraint # pragmas
    if isStaticConstraint(constraint):
      kind = StaticTypevarY
  let declStart = dest.len
  let entryCursor = n  # saved for the signature-phase rollback below
  dest.add n
  var delayed = default(DelayedSym)
  var crucial = default(CrucialPragma)
  n.into:
    delayed = handleSymDef(c, dest, n, kind) # 0
    let beforeExportMarker = dest.len
    wantExportMarker c, dest, n # 1
    crucial = CrucialPragma(sym: delayed.s.name)
    semPragmas c, dest, n, crucial, kind # 2
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, dest, beforeExportMarker, crucial
    if delayed.status == OkExistingFresh and InjectP in crucial.flags:
      # symbol is injected, add it to scope
      delayed.status = OkNew

    var beforeType = -1

    case kind
    of TypevarY:
      discard semLocalType(c, dest, n, InGenericConstraint)
      wantDot c, dest, n
    of StaticTypevarY:
      semStaticTypevarType c, dest, n
      wantDot c, dest, n
    of ParamY, LetY, VarY, ConstY, CursorY, PatternvarY, ResultY, FldY, GfldY, GletY, TletY, GvarY, TvarY:
      beforeType = dest.len
      if n.kind == DotToken:
        # no explicit type given:
        inc n # 3
        let orig = n
        var it = Item(n: n, typ: c.types.autoType)
        if kind == ConstY:
          withNewScope c:
            semConstExpr c, dest, it # 4
        elif kind == ParamY and it.n.kind == DotToken:
          if c.usingStmtMap.hasKey(delayed.lit):
            it.typ = c.usingStmtMap.getOrQuit(delayed.lit)
            dest.takeToken it.n # empty value slot
          elif c.routine.kind in {TemplateY, MacroY}:
            it.typ = c.types.untypedType
            dest.takeToken it.n # empty value slot
          else:
            # A parameter without a type and without a default value. Produce an
            # error *type* (mirroring how an unresolved named type is handled) and
            # leave the value slot empty. The error node must end up in the type
            # slot, not be appended after the empty value: an extra node there
            # makes the param over-full and crashes a later phase (issue #2012).
            var errBuf = createTokenBuf(4)
            c.buildErr errBuf, it.n.info, "type or init value expected"
            it.typ = typeToCursor(c, errBuf, 0)
            dest.takeToken it.n # empty value slot
        else:
          semLocalValue c, dest, it, crucial # 4
        n = it.n
        let typ = skipModifier(it.typ)
        if classifyType(c, typ) == VoidT:
          # point at the declared name (the decl's close info is elided under vpr):
          c.buildErr dest, delayed.info, "expression '" & asNimCode(orig) & "' has no type (or is ambiguous)"
        insertType c, dest, typ, beforeType
      else:
        let typ = semLocalType(c, dest, n) # 3
        if n.kind == DotToken:
          # empty value
          takeToken dest, n
        elif c.phase == SemcheckSignatures and kind in LocalSigKinds:
          if hasErrorSince(dest, beforeType):
            # The explicit type did not resolve in the signature phase — e.g.
            # it is injected by a template that only expands in the body phase
            # (tests/nimony/templates/tinject.nim). Abandon the early signature:
            # roll back everything emitted for this decl and leave it verbatim
            # for phase 3. The bail happens before `addSym`/`publish`, and the
            # `handleSymDef` Ident path adds nothing to `freshSyms`, so the only
            # residue is a never-referenced SymId — the rollback is otherwise
            # complete.
            dest.shrink declStart
            n = entryCursor
            takeTree dest, n
            return
          # Signature pass for a toplevel let/var with an explicit type:
          # the symbol and its type are established above; carry the init
          # value verbatim instead of semchecking it. Skipping `semExpr`
          # here avoids phase-2 macro expansion and forward references to
          # not-yet-signatured procs (the reasons toplevel let/var were
          # body-only). Phase 3 re-sems this decl fully. See `localSigGuard`.
          takeTree dest, n
        else:
          var it = Item(n: n, typ: typ)
          if kind == ConstY:
            withNewScope c:
              semConstExpr c, dest, it # 4
          else:
            semLocalValue c, dest, it, crucial # 4
          n = it.n
          if kind != PatternvarY:
            patchType c, dest, it.typ, beforeType
      if kind == ParamY:
        compatAnnotateVarargsParam c, dest, beforeType
    else:
      bug "semLocal"

    if beforeType != -1:
      let hasError = c.addSymForwardError delayed

      if hasError:
        var valueCursor = cursorAt(dest, beforeType)
        skip valueCursor # skips types
        let newValuePos = cursorToPosition(dest, valueCursor)
        var valueBuf = createTokenBuf()
        valueBuf.addSubtree valueCursor
        endRead(dest)
        shrink dest, newValuePos

        let orig = beginRead(valueBuf)
        c.buildErr dest, delayed.info, "attempt to redeclare: " & pool.strings[delayed.lit], orig

    else:
      c.addSym dest, delayed
    dest.addParRi(n.endInfo)

  # `setTag` (not `parLeToken` overwrite): the decl's ParLe is sealed by
  # now, retagging must preserve its jump field under `-d:virtualParRi`.
  if kind == LetY:
    if ThreadvarP in crucial.flags:
      setTag(dest[declStart], TagId(TletS))
    elif GlobalP in crucial.flags or c.currentScope.kind == ToplevelScope:
      setTag(dest[declStart], TagId(GletS))
  elif kind == VarY:
    if ThreadvarP in crucial.flags:
      setTag(dest[declStart], TagId(TvarS))
    elif GlobalP in crucial.flags or c.currentScope.kind == ToplevelScope:
      setTag(dest[declStart], TagId(GvarS))
  elif kind == GfldY:
    setTag(dest[declStart], TagId(GfldY))
  elif kind == StaticTypevarY:
    # the input tree used the `typevar` tag; the declaration's static-ness
    # lives in the tag, so retag it
    setTag(dest[declStart], TagId(StaticTypevarY))
  if kind notin {FldY, GfldY}:
    publish c, dest, delayed.s.name, declStart

proc semLocal(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind) =
  let info = it.n.info
  semLocal c, dest, it.n, kind
  producesVoid c, dest, info, it.typ

proc recordDeferredLocal*(c: var SemContext; n: Cursor) =
  ## Record a deferred toplevel let/var, keyed by its identifier, so a later
  ## signature-phase `when` can resolve it on demand. See `localSigGuard`.
  var name = n
  inc name  # past the (let/var tag
  if name.kind == Ident:
    c.deferredLocals[name.litId] = n

proc typeSymsAvailable(c: var SemContext; n: var Cursor): bool =
  ## Linear scan over an unsemmed type expression: true iff every name it
  ## references is already available in the signature phase — each `Ident`
  ## is declared in the current scopes/imports and each `Symbol` (e.g.
  ## template-injected) loads to a non-typevar decl. A generic `T` is *not*
  ## available (it still needs instantiation), so `var x: T` stays deferred
  ## while a fully-concrete `seq[int]` resolves. Consumes the subtree on
  ## `true`; on `false` the search stops immediately and `n` is left
  ## mid-subtree (callers pass a copy and abandon it).
  ##
  ## This is the atom-visiting analog of the `linearScan` traversal primitive
  ## (nim-lang/nimony#2064): a whole-subtree walk that, unlike `linearScan`
  ## (which stops at each `ParLe`), must also inspect the leaf `Ident`/`Symbol`
  ## tokens. When that primitive lands on master, fold this onto it.
  case n.kind
  of Ident:
    if not isDeclared(c, n.litId): return false
    inc n
  of Symbol:
    let res = tryLoadSym(n.symId)
    if res.status != LacksNothing or res.decl.tagEnum == TypevarTagId:
      return false
    inc n
  of ParLe:
    n.loopInto:
      if not typeSymsAvailable(c, n): return false
  else:
    inc n
  result = true

proc resolveDeferredLocal(c: var SemContext; ident: StrId): bool =
  ## On-demand signature resolution for a deferred toplevel let/var: sem just
  ## its signature into a throwaway buffer so the symbol becomes visible
  ## (scope + prog.mem) to a signature-phase `when` condition. The allocated
  ## symbol is remembered in `onDemandResolved` so that phase 3 reuses it (via
  ## `handleSymDef`) instead of redeclaring the decl — keeping the body-phase
  ## output equivalent to deferring normally. See `semIdentImpl`,
  ## nim-lang/nimony#1974.
  if not c.deferredLocals.hasKey(ident): return false
  # `getOrDefault` (not `[]`) to avoid the raising `Table.[]` in effect-checked
  # code; `hasKey` above already guarantees the key is present.
  var decl = c.deferredLocals.getOrDefault(ident, default(Cursor))
  c.deferredLocals.del ident  # resolve at most once
  # A plain `if` (not `case`) because this is a partial map over the handful of
  # local-decl kinds; `case n.stmtKind` would demand an `else`, which the source
  # validator forbids (it enforces exhaustive enumeration for tag discriminators).
  let sk = decl.stmtKind
  let kind =
    if sk == LetS: LetY
    elif sk == VarS: VarY
    elif sk == GletS: GletY
    elif sk == GvarS: GvarY
    elif sk == TletS: TletY
    elif sk == TvarS: TvarY
    else: return false
  # Availability gate (review, nim-lang/nimony#1974): resolve early only when
  # every symbol the explicit type references is already available in the
  # signature phase; otherwise leave the decl fully deferred, so the `when`
  # reference degrades to a clean "undeclared". The rollback in `semLocal`
  # below remains as the safety net for types that pass the scan but still
  # fail to sem.
  var typ = decl
  inc typ   # past the (let/var tag
  skip typ  # name
  skip typ  # export marker
  skip typ  # pragmas
  if typ.kind == DotToken: return false  # inferred: needs the init value
  if not typeSymsAvailable(c, typ): return false
  var scratch = createTokenBuf()
  # Force the signature phase so `semLocal` takes its value-verbatim path
  # (the `when` condition is folded at a temporarily-forced body phase).
  let savedPhase = c.phase
  c.phase = SemcheckSignatures
  semLocal(c, scratch, decl, kind)
  c.phase = savedPhase
  if scratch.len > 1 and scratch[1].kind == SymbolDef:
    c.onDemandResolved[ident] = scratch[1].symId
    result = true
  else:
    # The type did not resolve in the signature phase (e.g. template-injected);
    # `semLocal` rolled its output back. Leave the decl fully deferred.
    result = false

proc semEnumOrdinalValue(c: var SemContext; dest: var TokenBuf; n: var Cursor): xint =
  let info = n.info
  let oldPhase = c.phase
  c.phase = SemcheckBodies
  let before = dest.len
  result = evalConstIntExpr(c, dest, n, c.types.autoType)
  if not isNaN(result):
    var err = false
    let valI = asSigned(result, err)
    if not err:
      dest.shrink before
      dest.addIntLit(valI, info)
    else:
      let valU = asUnsigned(result, err)
      if not err:
        dest.shrink before
        dest.addUIntLit(valU, info)
  c.phase = oldPhase

proc semEnumField(c: var SemContext; dest: var TokenBuf; n: var Cursor; state: var EnumTypeState) =
  let declStart = dest.len
  var delayed = default(DelayedSym)
  dest.add n
  n.into:
    delayed = handleSymDef(c, dest, n, EfldY) # 0
    if delayed.status == OkExistingFresh:
      # XXX original nim always injects enum fields regardless of the enum sym itself,
      # this does the same here
      delayed.status = OkNew
    if state.declaredNames.contains delayed.lit:
      delayed.status = ErrRedef
    else:
      state.declaredNames.incl delayed.lit
    let beforeExportMarker = dest.len
    if n.kind == DotToken:
      if state.isExported:
        # if enum type is exported, enum field is exported
        dest.add identToken(pool.strings.getOrIncl("x"), n.info)
      else:
        dest.add n
      inc n # 1
    else:
      wantExportMarker c, dest, n # 1
    var crucial = CrucialPragma(sym: delayed.s.name)
    semPragmas c, dest, n, crucial, EfldY # 2
    if state.isBoolType and crucial.magic.len == 0:
      # bool type, set magic to fields if unset
      if state.thisValue == zero():
        crucial.magic = "false"
      else:
        crucial.magic = "true"
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, dest, beforeExportMarker, crucial
    if n.kind == DotToken or n.kind == Symbol:
      if state.isBoolType:
        dest.addParLe(BoolT, n.info)
        dest.addParRi()
      else:
        dest.add symToken(state.enumType, n.info)
      inc n # 3
    else:
      c.buildErr dest, n.info, "enum field's type must be empty"

    if n.kind == DotToken:
      # empty value
      let info = dest[declStart].info
      dest.add parLeToken(TupX, info)
      c.addXint dest, state.thisValue, info
      dest.add strToken(delayed.lit, info)
      dest.addParRi()
      inc n
    else:
      if n.kind == ParLe and n.exprKind == TupX:
        takeInto dest, n:
          let explicitValue = semEnumOrdinalValue(c, dest, n) # 4
          if explicitValue != state.thisValue:
            state.hasHole = true
            state.thisValue = explicitValue
          dest.add evalExpr(c, n)
      else:
        var ec = initEvalContext(addr c)
        var valueCursor = n
        let fieldValue = eval(ec, valueCursor)
        if fieldValue.kind == StringLit:
          dest.add parLeToken(TupX, n.info)
          c.addXint dest, state.thisValue, n.info
          dest.add fieldValue
          dest.addParRi()
          n = valueCursor
        else:
          dest.add parLeToken(TupX, n.info)
          let explicitValue = semEnumOrdinalValue(c, dest, n) # 4
          if explicitValue != state.thisValue:
            state.hasHole = true
            state.thisValue = explicitValue
          dest.add strToken(delayed.lit, n.endInfo)
          dest.addParRi()
    dest.addParRi(n.endInfo)
  if delayed.status == OkNew:
    addOverloadable(c.currentScope, delayed.lit, delayed.s)
  elif delayed.status == ErrRedef:
    c.buildErr dest, delayed.info, "attempt to redeclare: " & pool.strings[delayed.lit]
  publish c, dest, delayed.s.name, declStart

proc semGenericParam(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.substructureKind == TypevarU:
    semLocal c, dest, n, TypevarY
  elif n.substructureKind == StaticTypevarU:
    # re-semming an already desugared value generic parameter
    semLocal c, dest, n, StaticTypevarY
  else:
    buildErr c, dest, n.info, "expected 'typevar'"

proc semGenericParams(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == DotToken:
    takeToken dest, n
  elif n.substructureKind == TypevarsU:
    inc c.routine.inGeneric
    takeInto dest, n:
      while n.hasMore:
        semGenericParam c, dest, n
  elif n.typeKind == InvokeT:
    inc c.routine.inInst
    takeTree dest, n
  else:
    buildErr c, dest, n.info, "expected '.' or 'typevars'"

proc semParam(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.substructureKind == ParamU:
    semLocal c, dest, n, ParamY
  else:
    buildErr c, dest, n.info, "expected 'param'"

proc semParams(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == DotToken:
    takeToken dest, n
  elif n.substructureKind == ParamsU:
    takeInto dest, n:
      while n.hasMore:
        semParam c, dest, n
  else:
    buildErr c, dest, n.info, "expected '.' or 'params'"

proc addReturnResult(c: var SemContext; dest: var TokenBuf; resId: SymId; info: PackedLineInfo; bodyStart: int) =
  if resId != SymId(0):
    dest.reopenLastTree bodyStart # reopen the body `(stmts)`
    # maybe add `return result`:
    buildTree(dest, RetS, info):
      dest.addSymUse resId, info
    dest.addParRi() # add it back

proc semBorrow(c: var SemContext; dest: var TokenBuf; fn: StrId; beforeParams: int) =
  let signature = cursorAt(dest, beforeParams)
  var procBody = genBorrowedProcBody(c, fn, signature, signature.info)
  endRead(dest)
  var n = cursorAt(procBody, 0)
  var it = Item(n: n, typ: c.types.autoType)
  var resId = SymId(0)
  let bodyStart = dest.len
  dest.add it.n # `(stmts`
  it.n.into:
    resId = declareResult(c, dest, it.n.info)
    semProcBody c, dest, it
    dest.addParRi(it.n.endInfo)
  addReturnResult c, dest, resId, procBody[procBody.len - 1].info, bodyStart

proc getParamsType(c: var SemContext; dest: var TokenBuf; paramsAt: int): seq[TypeCursor] =
  result = @[]
  if dest[paramsAt].kind != DotToken:
    var n = cursorAt(dest, paramsAt)
    if n.substructureKind == ParamsU:
      inc n
      while n.hasMore:
        if n.symKind == ParamY:
          var local = takeLocal(n, SkipFinalParRi)
          result.add local.typ
        else:
          break
      endRead(dest)

proc getObjSymId(c: var SemContext; obj: TypeCursor): SymId =
  var obj = skipModifier(obj)
  while true:
    if obj.typeKind == InvokeT:
      inc obj
    else:
      break
  if obj.kind == Symbol:
    result = obj.symId
  else:
    result = SymId(0)

proc checkTypeHook(c: var SemContext; dest: var TokenBuf; params: seq[TypeCursor]; op: HookKind; info: PackedLineInfo) =
  var cond: bool
  case op
  of NoHook:
    return
  of DestroyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 1
    if not cond:
      buildErr c, dest, info, "signature for '=destroy' must be proc[T: object](x: T)"
  of TraceH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT and classifyType(c, params[1]) == PointerT
  of WasmovedH:
    cond = classifyType(c, c.routine.returnType) == VoidT and
        params.len == 1 and classifyType(c, params[0]) == MutT
  of CopyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of SinkhH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of DupH:
    cond = params.len == 1 and sameTrees(params[0], c.routine.returnType)

  if cond:
    let obj = getObjSymId(c, params[0])

    if obj == SymId(0):
      cond = false
    else:
      let res = tryLoadSym(obj)
      assert res.status == LacksNothing
      if res.decl.symKind == TypeY:
        let typeDecl = asTypeDecl(res.decl)

        if not (classifyType(c, typeDecl.body) == ObjectT):
          cond = false
      else:
        cond = false

  if not cond:
    case op
    of NoHook:
      discard
    of DestroyH:
      buildErr c, dest, info, "signature for '=destroy' must be proc[T: object](x: T)"
    of TraceH:
      buildErr c, dest, info, "signature for '=trace' must be proc[T: object](x: var T; env: pointer)"
    of WasmovedH:
      buildErr c, dest, info, "signature for '=wasMoved' must be proc[T: object](x: var T)"
    of CopyH:
      buildErr c, dest, info, "signature for '=copy' must be proc[T: object](x: var T; y: T)"
    of SinkhH:
      buildErr c, dest, info, "signature for '=sink' must be proc[T: object](x: var T; y: T)"
    of DupH:
      buildErr c, dest, info, "signature for '=dup' must be proc[T: object](x: T): T"

proc hookToAttachedOp(op: HookKind): AttachedOp =
  case op
  of DestroyH: attachedDestroy
  of WasmovedH: attachedWasMoved
  of CopyH: attachedCopy
  of SinkhH: attachedSink
  of DupH: attachedDup
  of TraceH, NoHook: attachedTrace

proc registerHook(c: var SemContext; obj: SymId, symId: SymId, op: HookKind; isGeneric: bool) =
  let attachedOp = hookToAttachedOp(op)
  # track per-type for embedding in type declarations:
  if not c.typeHooks.hasKey(obj):
    c.typeHooks[obj] = HooksPerType(a: default(array[AttachedOp, SymId]))
  c.typeHooks.getOrQuit(obj).a[attachedOp] = symId

proc getHookName(symId: SymId): string =
  result = pool.syms[symId]
  extractBasename(result)
  #result = result.normalize

proc semHook(c: var SemContext; dest: var TokenBuf; name: string; beforeParams: int; symId: SymId, info: PackedLineInfo): TypeCursor =
  let params = getParamsType(c, dest, beforeParams)
  case name
  of "=destroy":
    checkTypeHook(c, dest, params, DestroyH, info)
    result = params[0]
  of "=wasMoved":
    checkTypeHook(c, dest, params, WasmovedH, info)
    result = params[0]
  of "=trace":
    checkTypeHook(c, dest, params, TraceH, info)
    result = params[0]
  of "=copy":
    checkTypeHook(c, dest, params, CopyH, info)
    result = params[0]
  of "=sink":
    checkTypeHook(c, dest, params, SinkhH, info)
    result = params[0]
  of "=dup":
    checkTypeHook(c, dest, params, DupH, info)
    result = params[0]
  else:
    bug "unreachable"

proc hookToKind(name: string): HookKind =
  case name
  of "=destroy": DestroyH
  of "=wasMoved": WasmovedH
  of "=trace": TraceH
  of "=copy": CopyH
  of "=sink": SinkhH
  of "=dup": DupH
  else: NoHook

proc attachConverter(c: var SemContext; dest: var TokenBuf; symId: SymId;
                     declStart, beforeExportMarker, beforeGenericParams: int; info: PackedLineInfo) =
  let root = nominalRoot(c.routine.returnType)
  if root == SymId(0) and LenientConvertersFeature notin c.features:
    var errBuf = createTokenBuf(16)
    buildErr c, errBuf, info, "cannot attach converter to type " & typeToString(c.routine.returnType)
    dest.insert errBuf, declStart
  else:
    c.converters.mgetOrPut(root, @[]).add(symId)
    if dest[beforeExportMarker].kind != DotToken:
      # exported
      if not (dest[beforeGenericParams].kind == ParLe and
          pool.tags[dest[beforeGenericParams].tagId] == $InvokeT):
        # don't register instances
        c.converterIndexMap.add((root, symId))

proc attachMethod(c: var SemContext; dest: var TokenBuf; symId: SymId;
                  declStart, beforeParams, beforeGenericParams: int; info: PackedLineInfo) =
  if c.currentScope.up.kind != ToplevelScope:
    buildErr c, dest, info, "'method' is only allowed at top level"

  var params = cursorAt(dest, beforeParams)
  let paramsNode = params
  var root = SymId(0)
  var signature = StrId(0)
  if params.kind == ParLe:
    inc params
    if params.substructureKind == ParamU:
      inc params
      skip params, SkipName # name
      skip params, SkipExport # export marker
      skip params, SkipPragmas # pragmas
      root = getClass(params) # can be a generic instance symbol
      var methodName = pool.syms[symId]
      extractBasename methodName
      signature = pool.strings.getOrIncl(methodKey(methodName, paramsNode))
  if root == SymId(0) or not isObjectType(root):
    let typ = typeToString(params)
    dest.endRead()
    var errBuf = createTokenBuf(16)
    buildErr c, errBuf, info, "cannot attach method to type " & typ
    dest.insert errBuf, declStart
  else:
    dest.endRead()
    let methodIsInstance = dest[beforeGenericParams].kind == ParLe and dest[beforeGenericParams].tagId == TagId(InvokeT)
    var symToRegister = symId
    if methodIsInstance:
      symToRegister = dest[beforeGenericParams+1].symId

    # Also add to c.classes for vtable generation
    let methodEntry = MethodIndexEntry(fn: symToRegister, signature: signature)
    if c.classes.hasKey(root):
      c.classes.getOrQuit(root).methods.add methodEntry
    else:
      c.classes[root] = ClassEntry(methods: @[methodEntry])

proc hookThatShouldBeMethod(c: var SemContext; dest: var TokenBuf; hk: HookKind; beforeParams: int): bool =
  case hk
  of DestroyH, TraceH:
    result = false
    var params = cursorAt(dest, beforeParams)
    if params.kind == ParLe:
      inc params
      if params.substructureKind == ParamU:
        inc params
        skip params, SkipName # name
        skip params, SkipExport # export marker
        skip params, SkipPragmas # pragmas
        result = isInheritable(params, true)
    endRead(dest)
  else:
    result = false

proc handleForwardDeclarations(c: var SemContext; dest: var TokenBuf; declStart: int;
                               symId: SymId; crucial: CrucialPragma; hasBody: bool) =
  ## Handle forward declaration registration and matching during signature checking.
  ## On a successful match, the forward decl's `SymId` is added to
  ## `c.matchedForwardDecls`; `writeOutput` later strips its `(proc ...)`
  ## subtree from `dest` so it does not leak into the module's export index.
  if hasBody:
    # This is an implementation - look for matching forward declaration
    let implCursor = cursorAt(dest, declStart)
    let fwdDecl = findMatchingForwardDecl(c, symId, implCursor)
    endRead(dest)
    if fwdDecl != NoSymId:
      # Remove the forward declaration from prog.mem and scope
      if prog.mem.hasKey(fwdDecl):
        prog.mem.del(fwdDecl)
      # Also remove from scope so overload resolution doesn't find it
      let lit = symToIdent(fwdDecl)
      var scope = c.currentScope
      while scope != nil:
        scope.removeOverloadable(lit, fwdDecl)
        scope = scope.up
      # Mark the forward decl for removal from `dest` at writeOutput time —
      # we cannot splice now without invalidating positions other callers
      # are still holding (e.g. the impl's own `declStart`).
      c.matchedForwardDecls.incl fwdDecl
  elif {ImportcP, ImportcppP} * crucial.flags == {}:
    # This is a forward declaration - register it as a candidate
    addForwardDecl(c, symId)

proc attachSpecialProc(c: var SemContext; dest: var TokenBuf; kind: SymKind;
                       symId: SymId; declStart, beforeExportMarker, beforeGenericParams, beforeParams: int;
                       hk: HookKind; info: PackedLineInfo; isMagic: bool) =
  ## Attach converters, methods, or hooks that should become methods.
  if kind == ConverterY:
    attachConverter c, dest, symId, declStart, beforeExportMarker, beforeGenericParams, info
  elif kind == MethodY:
    attachMethod c, dest, symId, declStart, beforeParams, beforeGenericParams, info
  elif isMagic:
    # Magic hooks (`=destroy` etc. with a `.magic`) never become methods.
    # Their `(magicname)` tag also replaced the export marker, so the
    # `before*` positions can be stale — do not read them here.
    discard
  elif dest[beforeGenericParams].kind == ParLe and
      dest[beforeGenericParams].substructureKind == TypevarsU:
    # A generic hook is attached per instantiation (its typevars slot is
    # then an `(invoke ...)`), never as-is.
    discard
  elif hookThatShouldBeMethod(c, dest, hk, beforeParams):
    setTag(dest[declStart], TagId(MethodS)) # keeps the sealed jump
    dest[declStart] = withLineInfo(dest[declStart], info)
    attachMethod c, dest, symId, declStart, beforeParams, beforeGenericParams, info

proc untypedIsActive(c: SemContext; crucial: CrucialPragma): bool {.inline.} =
  result = UntypedP in crucial.flags or UntypedFeature in c.features

proc hasUntypedOrTypedParam(dest: var TokenBuf; beforeParams: int): bool =
  ## Check if any of the routine's params (already sem'd into dest at
  ## position `beforeParams`) carries `(untyped)` or `(typed)` as its type.
  ## Used to decide whether a macro body needs the special "skip user-side
  ## sem" path (which the plugin sub-compile re-sems with NimNode-typed
  ## params).
  result = false
  var n = cursorAt(dest, beforeParams)
  if n.substructureKind != ParamsU:
    endRead(dest)
    return
  n.loopInto:
    if n.substructureKind == ParamU:
      var p = n
      inc p
      # Skip name, exported marker, pragmas to land on the type slot.
      skip p; skip p; skip p
      if p.kind == ParLe and p.typeKind in {UntypedT, TypedT}:
        endRead(dest)
        return true
    skip n
  endRead(dest)

proc semBodyGenericInst(c: var SemContext; dest: var TokenBuf; it: var Item;
                        crucial: CrucialPragma; symId: SymId; beforeParams: int; hk: HookKind) =
  ## Process proc body for generic instantiation pass.
  if it.n.stmtKind != StmtsS:
    bug "(stmts) expected, but got ", it.n
  c.openScope() # open body scope
  var resId = SymId(0)
  let bodyStart = dest.len
  dest.add it.n
  it.n.into:
    # The body may already carry an explicit `(result ...)` declaration from a
    # module that was not compiled in untyped mode. In that case we must not
    # declare `result` a second time.
    if untypedIsActive(c, crucial) and it.n.stmtKind != ResultS:
      # for untyped generic procs, need to add result symbol now
      resId = declareResult(c, dest, it.n.info)
    semProcBody c, dest, it
    dest.addParRi(it.n.endInfo)
  c.closeScope() # close body scope
  c.closeScope() # close parameter scope
  if resId != SymId(0):
    addReturnResult c, dest, resId, it.n.endInfo, bodyStart
  if hk != NoHook:
    let params = getParamsType(c, dest, beforeParams)
    assert params.len >= 1
    let obj = getObjSymId(c, params[0])
    registerHook(c, obj, symId, hk, false)

proc semBodyCheckBody(c: var SemContext; dest: var TokenBuf; it: var Item;
                      kind: SymKind; crucial: CrucialPragma; symId: SymId;
                      beforeGenericParams, beforeParams: int; hookName: string; info: PackedLineInfo) =
  ## Process proc body for body checking pass.
  if it.n.stmtKind != StmtsS:
    bug "(stmts) expected, but got ", it.n
  c.openScope() # open body scope
  var resId = SymId(0)
  let bodyStart = dest.len
  if kind == MacroY and hasUntypedOrTypedParam(dest, beforeParams):
    # The user's macro takes an `untyped` / `typed` param. We can't
    # type-check uses like `body[0]` / `body.kind` at the user-side
    # because untyped has no NimNode methods. The plugin sub-compile
    # rewrites those param types to NimNode and does the real sem; here
    # we just store the body as raw AST. Downstream passes (derefs etc.)
    # already skip MacroS bodies. Side-effect: nested macro calls in this
    # body don't expand at user-side; they'd be re-resolved by the plugin
    # under the plugin's import scope.
    dest.takeTree it.n
  elif kind == MacroY:
    # Macro without untyped/typed params — sem normally so nested macro
    # calls expand at user-sem time (see project_macro_plugins.md).
    dest.add it.n
    it.n.into:
      if it.n.stmtKind != ResultS:
        resId = declareResult(c, dest, it.n.info)
      semProcBody c, dest, it
      dest.addParRi(it.n.endInfo)
  elif untypedIsActive(c, crucial) and c.routine.inGeneric > 0: # includes templates
    # should eventually be default for compat mode
    let mode = if kind == TemplateY: UntypedTemplate else: UntypedGeneric
    let dirty = kind == TemplateY and DirtyP in crucial.flags
    var ctx = createUntypedContext(addr c, mode, dirty)
    addParams(ctx, dest, beforeGenericParams)
    addParams(ctx, dest, beforeParams)
    semTemplBody ctx, dest, it.n
  else:
    dest.add it.n
    it.n.into:
      # Don't declare `result` again if the body already carries an explicit
      # `(result ...)` decl — happens when the proc body has already been
      # sem-checked once and is being re-fed through sem (e.g. via the
      # `executeExpr` sub-compile machinery).
      if it.n.stmtKind != ResultS:
        resId = declareResult(c, dest, it.n.info)
      semProcBody c, dest, it
      dest.addParRi(it.n.endInfo)
  c.closeScope() # close body scope
  c.closeScope() # close parameter scope
  addReturnResult c, dest, resId, it.n.endInfo, bodyStart
  let hk = hookToKind(hookName)
  if hk != NoHook:
    let objCursor = semHook(c, dest, hookName, beforeParams, symId, info)
    let obj = getObjSymId(c, objCursor)
    registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)

proc semEmptyBody(c: var SemContext; dest: var TokenBuf; it: var Item;
                  kind: SymKind; crucial: CrucialPragma; pass: PassKind;
                  symId: SymId; beforeParams: int; hookName: string; info: PackedLineInfo) =
  ## Handle proc with empty body (forward decl, .error, .borrow, or extern).
  if ErrorP in crucial.flags and pass in {checkGenericInst, checkBody}:
    let hk = hookToKind(hookName)
    if hk != NoHook:
      let objCursor = semHook(c, dest, hookName, beforeParams, symId, info)
      let obj = getObjSymId(c, objCursor)
      # Only register when signature is valid (obj != 0); checkTypeHook may have buildErr'd
      if obj != SymId(0):
        registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)
    takeToken dest, it.n
  elif BorrowP in crucial.flags and pass in {checkGenericInst, checkBody}:
    if kind notin {ProcY, FuncY, ConverterY, TemplateY, MethodY}:
      c.buildErr dest, it.n.info, ".borrow only valid for proc, func, converter, template or method"
    else:
      semBorrow(c, dest, symToIdent(symId), beforeParams)
    inc it.n # skip DotToken
  else:
    takeToken dest, it.n
  c.closeScope() # close parameter scope

proc semProcImpl(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind; pass: PassKind; newName = NoSymId) =
  let info = it.n.info
  let declStart = dest.len
  dest.add it.n
  it.n.into:
    let beforeName = dest.len

    var symId = SymId(0)
    var status = OkNew
    if it.n.kind == DotToken:
      symId = newName
      status = OkNew
      dest.add symdefToken(symId, it.n.info)
      inc it.n
    else:
      (symId, status) = declareOverloadableSym(c, dest, it, kind)

    let beforeExportMarker = dest.len
    wantExportMarker c, dest, it.n
    if it.n.kind == DotToken:
      takeToken dest, it.n
    else:
      buildErr c, dest, it.n.info, "TR pattern not implemented"
      skip it.n
    c.routine = createSemRoutine(kind, c.routine)
    # 'break' and 'continue' are valid in a template regardless of whether we
    # really have a loop or not:
    if kind == TemplateY:
      inc c.routine.inLoop
      inc c.routine.inGeneric

    try:
      c.openScope() # open parameter scope
      let beforeGenericParams = dest.len
      semGenericParams c, dest, it.n
      if c.routine.inGeneric > 0 and c.routine.parent.kind != NoSym and c.routine.parent.inGeneric == 0:
        c.genericInnerProcs.incl(symId)
      let beforeParams = dest.len
      semParams c, dest, it.n
      c.routine.returnType = semReturnType(c, dest, it.n)
      var crucial = CrucialPragma(sym: symId)
      semPragmas c, dest, it.n, crucial, kind
      c.routine.pragmas = crucial.flags
      c.routine.raisesType = crucial.raisesType
      if crucial.hasVarargs.isValid:
        addVarargsParameter c, dest, beforeParams, crucial.hasVarargs
      if crucial.magic.len > 0:
        exportMarkerBecomesNifTag c, dest, beforeExportMarker, crucial
      if status == OkExistingFresh and InjectP in crucial.flags:
        # symbol is injected, add it to scope
        let s = Sym(kind: kind, name: symId, pos: beforeName)
        var name = pool.syms[symId]
        extractBasename(name)
        # go up a scope for the parameter scope:
        c.currentScope.up.addOverloadable(pool.strings.getOrIncl(name), s)
      if it.n.kind == DotToken:
        takeToken dest, it.n
      else:
        buildErr c, dest, it.n.info, "`effects` must be empty"
        skip it.n

      # Forward declaration handling in signature checking phase:
      if pass == checkSignatures:
        handleForwardDeclarations(c, dest, declStart, symId, crucial, hasBody = it.n.kind != DotToken)

      publishSignature dest, symId, declStart
      let hookName = getHookName(symId)
      let hk = hookToKind(hookName)
      if status in {OkNew, OkExistingFresh}:
        attachSpecialProc(c, dest, kind, symId, declStart, beforeExportMarker,
                          beforeGenericParams, beforeParams, hk, info,
                          isMagic = crucial.magic.len > 0)
      let beforeBody = dest.len
      if it.n.kind != DotToken:
        case pass
        of checkGenericInst:
          semBodyGenericInst(c, dest, it, crucial, symId, beforeParams, hk)
        of checkBody:
          semBodyCheckBody(c, dest, it, kind, crucial, symId,
                           beforeGenericParams, beforeParams, hookName, info)
        of checkSignatures:
          dest.takeTree it.n
          c.closeScope() # close parameter scope
        of checkConceptProc:
          c.closeScope() # close parameter scope
          if it.n.kind == DotToken:
            inc it.n
          else:
            c.buildErr dest, it.n.info, "inside a `concept` a routine cannot have a body"
            skip it.n
      else:
        semEmptyBody(c, dest, it, kind, crucial, pass, symId, beforeParams, hookName, info)
      if c.routine.hasDefer:
        transformDefer dest, beforeBody
      dest.addParRi(it.n.endInfo)
    finally:
      c.routine = c.routine.parent
  if newName == NoSymId:
    producesVoid c, dest, info, it.typ
  publish c, dest, symId, declStart

  if kind == MacroY and pass == checkBody:
    let macroDecl = cursorAt(dest, declStart)
    let macroBinPath = compileMacroPlugin(c.g.config.nifcachePath, macroDecl, symId, info,
                                          c.commandLineArgs)
    endRead(dest)
    if macroBinPath.len > 0:
      c.compiledMacros[symId] = macroBinPath

proc findMacroInvocs(c: SemContext; n: Cursor; kind: SymKind): seq[Cursor] =
  # find all macro/template identifiers in pragmas to invoke them with parent proc definition
  result = newSeq[Cursor]()
  if kind in RoutineKinds:
    var n = asRoutine(n).pragmas
    if n.substructureKind == PragmasU:
      n = sub(n) # bound the walk; peek only, never left
      while n.hasMore:
        if n.exprKind == ErrX or n.substructureKind == KvU:
          skip n
        elif pragmaKind(n) != NoPragma or callConvKind(n) != NoCallConv or
            (IgnoreStyleFeature in c.features and n.kind == Ident and
             pragmaKindByStyle(n.litId) != NoPragma):
          # known builtin pragma — skip; under ignoreStyle this also covers
          # variants like `noInline` / `no_inline` so they don't get
          # mis-routed to the macro-invocation path below.
          skip n
        else:
          let hasParRi = n.kind == ParLe
          let start = n
          if n.exprKind == CallX:
            inc n
          let name = getIdent(n)
          if name != StrId(0) and
              not isCustomPragmaTemplate(c, name) and
              not (name in c.userPragmas and not hasParRi):
            result.add start
            n = start
            skip n
          else:
            skip n

proc copyExcept(dest: var TokenBuf; n: var Cursor; excl: seq[Cursor]; i: var int) =
  ## Copies the subtree at `n` while dropping the subtrees listed in `excl`
  ## (which must appear in document order).
  if i < excl.len and n == excl[i]:
    skip n
    inc i
  elif n.kind == ParLe:
    dest.add n
    n.into:
      while n.hasMore:
        copyExcept(dest, n, excl, i)
      dest.addParRi(n.endInfo)
  else:
    dest.takeToken n

proc transformMacroInvoc(c: var SemContext; dest: var TokenBuf; it: var Item; macroInvocsPos: seq[Cursor]) =
  # transform `proc foo() {.macrofoo, macrobar.}` to `macrobar: macrofoo: proc foo()`.
  var inBuf = createTokenBuf()

  # adds last one in macroInvocsPos to buf first as it is invoked last.
  let info = it.n.info
  for i in countdown(macroInvocsPos.len - 1, 0):
    inBuf.addParLe CallX, info
    var n = macroInvocsPos[i]
    if n.exprKind == CallX:
      n.into:
        assert n.kind == Ident
        inBuf.add n
        inc n
        while n.hasMore:
          inBuf.takeTree n
    else:
      assert n.kind == Ident
      inBuf.add n
    inBuf.addParLe StmtsS, info

  var n = it.n
  # copies the proc def to inBuf excepts all macros and templates to avoid
  # recursive macro invocations and invocations of them at unexpected places.
  var i = 0
  copyExcept(inBuf, n, macroInvocsPos, i)
  for i in 0 ..< macroInvocsPos.len:
    inBuf.addParRi  # close StmtsS
    inBuf.addParRi  # close CallX
  var it2 = Item(n: cursorAt(inBuf, 0), typ: c.types.autoType)
  #echo "macro invoc in: ", toString it2.n
  #let lastDestLen = dest.len
  semCall c, dest, it2, {}
  endRead inBuf
  #[
  if dest.len > lastDestLen:
    echo "macro invoc out: ", toString cursorAt(dest, lastDestLen)
    endRead dest
  else:
    echo "macro invoc out: empty"
  ]#
  it.n = n
  it.typ = it2.typ

proc semProc(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind; pass: PassKind) =
  let macroInvocsPos = findMacroInvocs(c, it.n, kind)
  if macroInvocsPos.len > 0:
    if pass == checkBody:
      transformMacroInvoc(c, dest, it, macroInvocsPos)
    else:
      dest.takeTree it.n
  elif it.n.firstSon.kind == DotToken:
    # anon routine
    let info = it.n.firstSon.info
    let name = identToSym(c, "`anonproc", ProcY)

    var anons = createTokenBuf()
    semProcImpl c, anons, it, kind, pass, name
    let exprStart = dest.len
    dest.add parLeToken(ExprX, info)
    dest.add parLeToken(StmtsS, info)
    let anonTypePos = dest.len
    dest.add anons
    dest.addParRi()
    dest.add symToken(name, info)
    dest.addParRi()
    let expected = it.typ
    it.typ = typeToCursor(c, dest, anonTypePos)
    commonType c, dest, it, exprStart, expected

  else:
    semProcImpl c, dest, it, kind, pass

proc semTypePragmas(c: var SemContext; dest: var TokenBuf; n: var Cursor; sym: SymId; beforeExportMarker: int): CrucialPragma =
  result = CrucialPragma(sym: sym)
  semPragmas c, dest, n, result, TypeY # 2
  if result.magic.len > 0:
    exportMarkerBecomesNifTag c, dest, beforeExportMarker, result

proc fitTypeToPragmas(c: var SemContext; dest: var TokenBuf; pragmas: CrucialPragma; typeStart: int) =
  if {ImportcP, ImportcppP} * pragmas.flags != {}:
    let typ = cursorAt(dest, typeStart)
    if isNominal(typ.typeKind):
      # ok
      endRead(dest)
    elif typ.typeKind in {IntT, UIntT, FloatT, CharT, PointerT}:
      let info = typ.info
      endRead(dest)
      let kind = if ImportcP in pragmas.flags: ImportcP else: ImportcppP
      var attrs = @[
        parLeToken(kind, info),
        strToken(pool.strings.getOrIncl(pragmas.externName), info),
        parRiToken(info)
      ]
      if HeaderP in pragmas.flags:
        assert pragmas.headerFileTok.kind == StringLit
        attrs.add parLeToken(HeaderP, info)
        attrs.add pragmas.headerFileTok
        attrs.addParRi(info)
      # Imported aliases of scalar builtins must override the C spelling
      # (`importc`/`importcpp` + optional `header`) rather than stack with
      # existing builtin attributes like `(importc "int")`. The payload
      # shape varies per type kind: `(i 32)` carries a single bit-size
      # literal, `(pointer)` is empty, `(pointer (notnil))` wraps a
      # nilness annotation. We preserve the kind-specific payload, drop
      # any pre-existing attrs, then attach the new ones.
      var rebuilt = createTokenBuf(8 + attrs.len)
      var t = typ
      let tk = typ.typeKind
      rebuilt.add t
      t = sub(t) # (i/u/f/c/pointer/cstring; `t` is a copy
      case tk
      of IntT, UIntT, FloatT, CharT:
        takeToken rebuilt, t # bit-size literal
      of PointerT, CstringT:
        if t.hasMore and t.substructureKind in {NotnilU, NilU, UncheckedU}:
          takeTree rebuilt, t
      else: discard
      while t.hasMore:
        skip t
      for tok in attrs:
        rebuilt.add tok
      rebuilt.addParRi()
      dest.replace cursorAt(rebuilt, 0), typeStart
    else:
      let err = "cannot import type " & typeToString(typ)
      let info = typ.info
      endRead(dest)
      c.buildErr dest, info, err

proc buildInnerObjDecl(c: var SemContext; decl: Cursor; sym: var SymId): TokenBuf =
  ## build inner object type declaration from full ref/ptr object decl
  result = createTokenBuf(64)

  # make anon object symbol from `sym` and set `sym` to it:
  var isGlobal = false
  let basename = extractBasename(pool.syms[sym], isGlobal)
  var objName = basename & ".Obj"
  if isGlobal: c.makeGlobalSym(objName)
  else: c.makeLocalSym(objName)
  sym = pool.syms.getOrIncl(objName)

  var n = decl
  result.add n # (type
  n.into:
    result.add symdefToken(sym, n.info)
    skip n, SkipName # name
    takeTree result, n # copy exported (?)
    takeTree result, n # copy typevars
    # ^ may need to build fresh identifiers
    takeTree result, n # copy pragmas
    assert n.typeKind in {RefT, PtrT}
    n.into: # (ref/ptr
      takeTree result, n # copy (object)
    result.addParRi(n.endInfo) # ) from type

proc invokeInnerObj(c: var SemContext; dest: var TokenBuf; genericsPos: int; objSym: SymId; info: PackedLineInfo) =
  var params = cursorAt(dest, genericsPos)
  if params.substructureKind == TypevarsU:
    # build an invocation of the inner object type; enterScope bounds the
    # walk (the typevars' close token is elided under -d:virtualParRi)
    params = sub(params)
    var invokeBuf = createTokenBuf(16)
    invokeBuf.buildTree InvokeT, info:
      invokeBuf.add symToken(objSym, info)
      while params.hasMore:
        let typevar = asTypevar(params).name
        if typevar.kind == SymbolDef:
          invokeBuf.add symToken(typevar.symId, typevar.info)
        else:
          # assume it was left as an identifier
          invokeBuf.addSubtree typevar
        skip params
    endRead(dest)
    dest.add invokeBuf
  else:
    # enough to use object sym directly
    endRead(dest)
    dest.add symToken(objSym, info)

proc semTypeSection(c: var SemContext; dest: var TokenBuf; n: var Cursor; outerRefOwner: SymId = SymId(0)) =
  let startCursor = n
  let declStart = dest.len
  var delayed = default(DelayedSym)
  var isEnumTypeDecl = false
  var isRefPtrObj = false
  var refOwner = SymId(0)
  var innerObjDecl = default(TokenBuf)
  dest.add n
  n.into:
    # name, export marker, generic params, pragmas, body
    delayed = handleSymDef(c, dest, n, TypeY) # 0
    let beforeExportMarker = dest.len
    let typeIsExported = handleExportMarker(c, dest, n) # 1

    let beforeGenerics = dest.len
    var isGeneric: bool
    let prevGeneric = c.routine.inGeneric
    let prevInst = c.routine.inInst
    if n.kind == DotToken:
      takeToken dest, n
      isGeneric = false
    else:
      let oldScopeKind = c.currentScope.kind
      openScope c
      semGenericParams c, dest, n
      # copy toplevel scope status for exported fields
      c.currentScope.kind = oldScopeKind
      isGeneric = true

    let crucial = semTypePragmas(c, dest, n, delayed.s.name, beforeExportMarker)
    if delayed.status == OkExistingFresh and InjectP in crucial.flags:
      # symbol is injected, add it to scope
      delayed.status = OkNew

    if c.phase == SemcheckSignatures or
        (delayed.status in {OkNew, OkExistingFresh} and
          c.phase != SemcheckTopLevelSyms):
      # body:
      if n.kind == DotToken:
        takeToken dest, n
      else:
        let typeStart = dest.len
        case n.typeKind
        of EnumT, HoleyEnumT:
          semEnumType(c, dest, n, delayed.s.name, beforeExportMarker, crucial.size)
          isEnumTypeDecl = true
        of RefT, PtrT:
          var obj = n
          inc obj
          if obj.typeKind == ObjectT:
            isRefPtrObj = true
            refOwner = delayed.s.name # outer type sym before mutation
            var objSym = delayed.s.name
            innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
            dest.add n # ref/ptr tag
            n.into:
              invokeInnerObj(c, dest, beforeGenerics, objSym, n.info)
              skip n
              dest.addParRi(n.endInfo)
          else:
            semLocalTypeImpl c, dest, n, InTypeSection, typeIsExported, delayed.s.name
        of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
           TemplateT, ObjectT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, ArrayT, VarargsT,
           StaticT, TupleT, AnumT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
           DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
           UntypedT, TypedT, CstringT, PointerT, OrdinalT:
          semLocalTypeImpl c, dest, n, InTypeSection, typeIsExported, delayed.s.name
        fitTypeToPragmas(c, dest, crucial, typeStart)
    else:
      if n.typeKind in {RefT, PtrT}:
        var obj = n
        inc obj
        if obj.typeKind == ObjectT:
          # handle these here too for better forward decls
          isRefPtrObj = true
          refOwner = delayed.s.name
          var objSym = delayed.s.name
          innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
          dest.add n # ref/ptr tag
          n.into:
            invokeInnerObj(c, dest, beforeGenerics, objSym, n.info)
            skip n
            dest.addParRi(n.endInfo)
      if not isRefPtrObj: # body not already handled
        if isGeneric:
          # capture typevars for instantiation of forward declared types to work
          var ctx = createUntypedContext(addr c, UntypedForwardGeneric)
          addParams(ctx, dest, beforeGenerics)
          semTemplBody ctx, dest, n # body
        else:
          dest.takeTree n # body
    if isGeneric:
      closeScope c
      c.routine.inGeneric = prevGeneric # revert increase by semGenericParams
      c.routine.inInst = prevInst

    c.addSym dest, delayed
    dest.addParRi(n.endInfo)

  publish c, dest, delayed.s.name, declStart

  if isEnumTypeDecl and c.routine.kind != TemplateY:
    var enumTypeDecl = tryLoadSym(delayed.s.name)
    assert enumTypeDecl.status == LacksNothing
    var pending = createTokenBuf()
    genEnumToStrProc(c, pending, enumTypeDecl.decl)

    var dollorProcDecl = beginRead(pending)
    var it = Item(n: dollorProcDecl, typ: c.types.autoType)
    # semchecking is needed for publishing signature
    # and transforming `ret ...` into `ret result` for `controlflow.nim`
    semExpr(c, dest, it)

  if isRefPtrObj:
    if c.phase > SemcheckTopLevelSyms:
      var topLevelDest = createTokenBuf(64)
      var topLevelRead = beginRead(innerObjDecl)
      var phase = SemcheckTopLevelSyms
      swap c.phase, phase
      semTypeSection c, topLevelDest, topLevelRead, refOwner
      swap c.phase, phase
      innerObjDecl = topLevelDest
    if c.phase > SemcheckSignatures:
      # need to go through signature phase if not applied yet since decl already has sym
      var sigDest = createTokenBuf(64)
      var sigRead = beginRead(innerObjDecl)
      var phase = SemcheckSignatures
      swap c.phase, phase
      semTypeSection c, sigDest, sigRead, refOwner
      swap c.phase, phase
      innerObjDecl = sigDest
    var decl = beginRead(innerObjDecl)
    semTypeSection c, dest, decl, refOwner

proc addTupleAccess(buf: var TokenBuf; lvalue: SymId; i: int; info: PackedLineInfo) =
  buf.add parLeToken(TupatX, info)
  buf.add symToken(lvalue, info)
  buf.addIntLit(i, info)
  buf.addParRi()

proc semUnpackDecl(c: var SemContext; dest: var TokenBuf; it: var Item) =
  case c.phase
  of SemcheckTopLevelSyms:
    dest.takeTree it.n
    return
  of SemcheckSignaturesInProgress, SemcheckSignatures:
    var kindTag = it.n
    while kindTag.stmtKind == UnpackdeclS:
      inc kindTag # unpackdecl tag
      skip kindTag, SkipValue # value
      assert kindTag.substructureKind == UnpacktupU
      inc kindTag # unpacktup tag
    let kind = kindTag.symKind
    if kind != ConstY:
      dest.takeTree it.n
      return
  of SemcheckBodiesInProgress, SemcheckBodies: discard

  let info = it.n.info
  it.n.into: # skip tag
    var tup = Item(n: it.n, typ: c.types.autoType)
    let tupInfo = tup.n.info
    var tupBuf = createTokenBuf(16)
    semExpr c, tupBuf, tup
    it.n = tup.n
    let tupleType = skipModifier(tup.typ)
    if tupleType.typeKind != TupleT:
      c.buildErr dest, tupInfo, "expected tuple for tuple unpacking"
      while it.n.hasMore: skip it.n
    else:
      if it.n.substructureKind != UnpacktupU:
        error "illformed AST: `unpacktup` inside `unpackdecl` expected, got ", it.n
      it.n.into: # skip unpacktup tag
        var kindTag = it.n
        while kindTag.stmtKind == UnpackdeclS:
          # skip nested unpacks as well
          inc kindTag # unpackdecl tag
          skip kindTag, SkipValue # value
          assert kindTag.substructureKind == UnpacktupU
          inc kindTag # unpacktup tag
        let kind = kindTag.symKind
        let tmpName = identToSym(c, "`tmptup", kind)

        # build local for tuple:
        let tmpStart = dest.len
        dest.buildTree kind, info:
          dest.add symdefToken(tmpName, info) # 0: name
          dest.addDotToken() # 1: export
          dest.addDotToken() # 2: pragma
          dest.addSubtree tupleType # 3: type
          dest.add tupBuf # 4: value
        publish c, dest, tmpName, tmpStart

        # iterate over unpacktup:
        var declBuf = createTokenBuf(32)
        var i = 0
        while it.n.hasMore:
          let declInfo = it.n.info
          if it.n.stmtKind == UnpackdeclS:
            takeInto declBuf, it.n:
              assert it.n.kind == DotToken # value
              inc it.n
              declBuf.addTupleAccess(tmpName, i, declInfo)
              takeTree declBuf, it.n
          else:
            takeInto declBuf, it.n:
              takeTree declBuf, it.n # 0: name
              takeTree declBuf, it.n # 1: export
              takeTree declBuf, it.n # 2: pragma
              takeTree declBuf, it.n # 3: type
              assert it.n.kind == DotToken # value
              inc it.n
              declBuf.addTupleAccess(tmpName, i, declInfo) # 4: value
          var decl = cursorAt(declBuf, 0)
          semStmt c, dest, decl, false
          endRead(declBuf)
          declBuf.shrink 0
          inc i
  producesVoid c, dest, info, it.typ

proc semUsing(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    while n.hasMore:
      assert n.substructureKind == FldU
      takeInto dest, n:
        var ident = StrId(0)
        if n.kind == Ident:
          ident = n.litId
          takeToken dest, n
        else:
          c.buildErr dest, n.info, "identifier is expected", n
          skip n

        # export marker
        if n.kind == DotToken:
          takeToken dest, n
        else:
          c.buildErr dest, n.info, "identifiers under using statements cannot be exported", n
          skip n

        # pragma
        # currently no pragmas can be used in using statements
        if n.kind == DotToken:
          takeToken dest, n
        elif n.substructureKind == PragmasU:
          c.buildErr dest, n.info, "using statements supports no pragmas", n
          skip n
        else:
          c.buildErr dest, n.info, "illformed AST inside using statement", n
          skip n

        let typ = semLocalType(c, dest, n)
        if ident != StrId(0):
          c.usingStmtMap[ident] = typ

        if n.kind == DotToken:
          takeToken dest, n
        else:
          c.buildErr dest, n.info, "illformed AST inside using statement", n
          skip n

proc semDo(c: var SemContext; dest: var TokenBuf; it: var Item; pass: PassKind) =
  let info = it.n.info
  var anons = createTokenBuf()
  it.n.into:
    # transform the do notation to an anon proc
    anons.addParLe(ProcS, info)
    anons.addEmpty info # name
    anons.addEmpty3 info # export, pattern, typevars
    anons.takeTree it.n  # params
    anons.takeTree it.n  # return type
    anons.addEmpty info  # pragma
    anons.addEmpty info  # effects
    anons.takeTree it.n  # body
    anons.addParRi(it.n.endInfo)
  var anonIt = Item(n: beginRead(anons), typ: it.typ)
  semProc c, dest, anonIt, ProcY, pass
  endRead anons
  it.typ = anonIt.typ

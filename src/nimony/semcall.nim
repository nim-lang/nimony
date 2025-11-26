# included in sem.nim

proc fetchCallableType(c: var SemContext; n: Cursor; s: Sym): TypeCursor =
  if s.kind == NoSym:
    let s = getIdent(n)
    if s != StrId(0):
      c.buildErr n.info, "undeclared identifier: " & pool.strings[s]
    else:
      c.buildErr n.info, "undeclared identifier"
    result = c.types.autoType
  else:
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      var d = res.decl
      if s.kind.isLocal:
        skipToLocalType d
      result = d
    else:
      c.buildErr n.info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      result = c.types.autoType

proc pickBestMatch(c: var SemContext; m: openArray[Match]; flags: set[SemFlag] = {}): int =
  result = -1
  var other = -1
  for i in 0..<m.len:
    if not m[i].err:
      if result < 0:
        result = i
      else:
        case cmpMatches(m[result], m[i], preferIterators = PreferIterators in flags)
        of NobodyWins:
          other = i
          #echo "ambiguous ", pool.syms[m[result].fn.sym], " vs ", pool.syms[m[i].fn.sym]
        of FirstWins:
          discard "result remains the same"
        of SecondWins:
          result = i
          other = -1
  if other >= 0: result = -2 # ambiguous

type MagicCallKind = enum
  NonMagicCall, MagicCall, MagicCallNeedsSemcheck

proc addFn(c: var SemContext; fn: FnCandidate; fnOrig: Cursor; m: var Match): MagicCallKind =
  result = NonMagicCall
  if fn.fromConcept and fn.sym != SymId(0):
    c.dest.add identToken(symToIdent(fn.sym), fnOrig.info)
  elif fn.kind in RoutineKinds:
    assert fn.sym != SymId(0)
    let res = tryLoadSym(fn.sym)
    if res.status == LacksNothing:
      var n = res.decl
      inc n # skip the symbol kind
      if n.kind == SymbolDef:
        inc n # skip the SymbolDef
        if n.kind == ParLe:
          if n.exprKind in {DefinedX, DeclaredX, AstToStrX, CompilesX, TypeofX,
              LowX, HighX, AddrX, EnumToStrX, DefaultObjX, DefaultTupX, DefaultdistinctX,
              ArrAtX, DerefX, TupatX, SizeofX, InternalTypeNameX, IsX}:
            # magic needs semchecking after overloading
            result = MagicCallNeedsSemcheck
          else:
            result = MagicCall
          # ^ export marker position has a `(`? If so, it is a magic!
          let info = c.dest[c.dest.len-1].info
          copyKeepLineInfo c.dest[c.dest.len-1], n.load # overwrite the `(call` node with the magic itself
          inc n
          if n.kind == IntLit:
            if pool.integers[n.intId] == TypedMagic:
              # use type of first param
              var paramType = fn.typ
              assert paramType.typeKind in RoutineTypes
              inc paramType
              for i in 1..4: skip paramType
              assert paramType.substructureKind == ParamsU
              inc paramType
              assert paramType.symKind == ParamY
              paramType = asLocal(paramType).typ
              if m.inferred.len != 0:
                paramType = instantiateType(c, paramType, m.inferred)
              removeModifier(paramType)
              let typeStart = c.dest.len
              c.dest.addSubtree paramType
              # op type line info does not matter, strip it for better output:
              for tok in typeStart ..< c.dest.len:
                c.dest[tok].info = info
            else:
              c.dest.add n
            inc n
          if n.kind != ParRi:
            bug "broken `magic`: expected ')', but got: ", n
    if result == NonMagicCall:
      c.dest.add symToken(fn.sym, fnOrig.info)
  else:
    c.dest.addSubtree fnOrig

proc typeofCallIs(c: var SemContext; it: var Item; beforeCall: int; returnType: TypeCursor) {.inline.} =
  let expected = it.typ
  it.typ = returnType
  commonType c, it, beforeCall, expected

proc semTemplateCall(c: var SemContext; it: var Item; fnId: SymId; beforeCall: int;
                     m: Match) =
  var expandedInto = createTokenBuf(30)

  let s = fetchSym(c, fnId)
  let res = declToCursor(c, s)
  if res.status == LacksNothing:
    let args = cursorAt(c.dest, beforeCall + 2)
    let firstVarargMatch = cursorAt(c.dest, beforeCall + 2 + m.firstVarargPosition)
    expandTemplate(c, expandedInto, res.decl, args, firstVarargMatch, addr m.inferred, c.dest[beforeCall].info)
    # We took 2 cursors, so we have to do the `endRead` twice too:
    endRead(c.dest)
    endRead(c.dest)
    shrink c.dest, beforeCall
    expandedInto.addParRi() # extra token so final `inc` doesn't break
    var a = Item(n: cursorAt(expandedInto, 0), typ: c.types.autoType)
    let aInfo = a.n.info
    inc c.routine.inInst
    semExpr c, a
    # make sure template body expression matches return type, mirrored with `semProcBody`:
    let returnType =
      if m.inferred.len == 0 or m.returnType.kind == DotToken:
        m.returnType
      else:
        instantiateType(c, m.returnType, m.inferred)
    case returnType.typeKind
    of UntypedT:
      # untyped return type ignored, maybe could be handled in commonType
      discard
    of VoidT:
      typecheck(c, aInfo, a.typ, returnType)
    else:
      commonType c, a, beforeCall, returnType
    dec c.routine.inInst
    # now match to expected type:
    it.kind = a.kind
    typeofCallIs c, it, beforeCall, a.typ
  else:
    c.buildErr it.n.info, "could not load symbol: " & pool.syms[fnId] & "; errorCode: " & $res.status

type
  FnCandidates = object
    a: seq[FnCandidate]
    marker: HashSet[SymId]

proc addUnique(c: var FnCandidates; x: FnCandidate) =
  if not containsOrIncl(c.marker, x.sym):
    c.a.add x

iterator findConceptsInConstraint(typ: Cursor): Cursor =
  var typ = typ
  var nested = 0
  while true:
    if typ.kind == ParRi:
      inc typ
      dec nested
    elif typ.kind == Symbol:
      let section = getTypeSection typ.symId
      if section.body.typeKind == ConceptT:
        yield section.body
      inc typ
    elif typ.typeKind == AndT:
      inc typ
      inc nested
    else:
      skip typ
    if nested == 0: break

proc maybeAddConceptMethods(c: var SemContext; fn: StrId; typevar: SymId; cands: var FnCandidates) =
  let res = tryLoadSym(typevar)
  assert res.status == LacksNothing
  let local = asLocal(res.decl)
  if local.kind == TypevarY and local.typ.kind != DotToken:
    for concpt in findConceptsInConstraint(local.typ):
      var ops = concpt
      inc ops  # (concept
      skip ops # .
      skip ops # .
      skip ops #   (typevar Self ...)
      if ops.stmtKind == StmtsS:
        inc ops
        while ops.kind != ParRi:
          let sk = ops.symKind
          if sk in RoutineKinds:
            var prc = ops
            inc prc # (proc
            if prc.kind == SymbolDef and sameIdent(prc.symId, fn):
              var d = ops
              #skipToParams d
              cands.addUnique FnCandidate(kind: sk, sym: prc.symId, typ: d, fromConcept: true)
          skip ops

proc hasAttachedParam(params: Cursor; typ: SymId): bool =
  result = false
  var params = params
  assert params.substructureKind == ParamsU
  inc params
  while params.kind != ParRi:
    let param = takeLocal(params, SkipFinalParRi)
    let root = nominalRoot(param.typ)
    if root != SymId(0) and root == typ:
      return true

proc addTypeboundOps(c: var SemContext; fn: StrId; s: SymId; cands: var FnCandidates) =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  let decl = asTypeDecl(res.decl)
  if decl.kind == TypeY:
    let moduleSuffix = extractModule(pool.syms[s])
    if moduleSuffix == "" or
        # with --noSystem, magic types can have the system module suffix
        # without the system module being loaded
        # just ignore symbols from the system module,
        # their bound ops should be in scope anyway
        moduleSuffix == SystemModuleSuffix:
      discard
    elif moduleSuffix == c.thisModuleSuffix:
      # XXX probably redundant over normal lookup but `OchoiceX` does not work yet
      # do not use cache, check symbols from toplevel scope:
      for topLevelSym in topLevelSyms(c, fn):
        let res = tryLoadSym(topLevelSym)
        assert res.status == LacksNothing
        let routine = asRoutine(res.decl)
        if routine.kind in RoutineKinds and hasAttachedParam(routine.params, s):
          cands.addUnique FnCandidate(kind: routine.kind, sym: topLevelSym, typ: routine.params)
    else:
      if (s, fn) in c.cachedTypeboundOps:
        for fnSym in c.cachedTypeboundOps[(s, fn)]:
          let res = tryLoadSym(fnSym)
          assert res.status == LacksNothing
          let routine = asRoutine(res.decl)
          cands.addUnique FnCandidate(kind: routine.kind, sym: fnSym, typ: routine.params)
      else:
        var ops: seq[SymId] = @[]
        for topLevelSym in loadSyms(moduleSuffix, fn):
          let res = tryLoadSym(topLevelSym)
          assert res.status == LacksNothing
          let routine = asRoutine(res.decl)
          if routine.kind in RoutineKinds and hasAttachedParam(routine.params, s):
            ops.add topLevelSym
            cands.addUnique FnCandidate(kind: routine.kind, sym: topLevelSym, typ: routine.params)
        c.cachedTypeboundOps[(s, fn)] = ops
  elif decl.kind == TypevarY:
    maybeAddConceptMethods c, fn, s, cands

type
  CallState = object
    beforeCall: int
    fn: Item
    fnKind: SymKind
    fnName: StrId
    callNode: PackedToken
    dest, genericDest: TokenBuf
    args: seq[CallArg]
    hasGenericArgs, hasNamedArgs: bool
    flags: set[SemFlag]
    source: TransformedCallSource
      ## type of expression the call was transformed from
    argsScopeClosed: bool

proc closeArgsScope(c: var SemContext; cs: var CallState; merge = true) =
  assert not cs.argsScopeClosed, "args scope already closed"
  if merge:
    commitShadowScope(c.currentScope)
  else:
    rollbackShadowScope(c.currentScope)
  cs.argsScopeClosed = true

proc untypedCall(c: var SemContext; it: var Item; cs: var CallState) =
  closeArgsScope c, cs, merge = false
  c.dest.add cs.callNode
  c.dest.addSubtree cs.fn.n
  for a in cs.args:
    # XXX call semTemplBody for orig instead?
    c.dest.addSubtree a.n
  # untyped propagates to the result type:
  typeofCallIs c, it, cs.beforeCall, c.types.untypedType
  takeParRi c, it.n

proc semConvFromCall(c: var SemContext; it: var Item; cs: CallState) =
  let beforeExpr = c.dest.len
  let info = cs.callNode.info
  var destType = cs.fn.typ
  if destType.typeKind == TypedescT: inc destType
  if destType.typeKind in {SinkT, LentT} and cs.args[0].typ.typeKind == TypedescT:
    var nullary = destType
    inc nullary
    if nullary.kind == ParRi:
      # sink T/lent T call
      var typeBuf = createTokenBuf(16)
      typeBuf.add destType
      typeBuf.addSubtree cs.args[0].n
      typeBuf.addParRi()
      var item = Item(n: beginRead(typeBuf), typ: it.typ)
      semLocalTypeExpr(c, item)
      takeParRi c, it.n
      it.typ = item.typ
      return
  c.dest.add parLeToken(ConvX, info)
  c.dest.copyTree destType
  semConvArg(c, destType, Item(n: cs.args[0].n, typ: cs.args[0].typ), info, beforeExpr)
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

proc semObjConstr(c: var SemContext, it: var Item)

proc semObjConstrFromCall(c: var SemContext; it: var Item; cs: CallState) =
  skipParRi it.n
  var objBuf = createTokenBuf()
  objBuf.add parLeToken(OconstrX, cs.callNode.info)
  objBuf.addSubtree cs.fn.n
  objBuf.addParRi()
  var objConstr = Item(n: cursorAt(objBuf, 0), typ: it.typ)
  semObjConstr c, objConstr
  it.typ = objConstr.typ

proc buildCallSource(buf: var TokenBuf; cs: CallState) =
  case cs.source
  of RegularCall:
    buf.add cs.callNode
    buf.addSubtree cs.fn.n
    for a in cs.args:
      buf.addSubtree a.n
  of MethodCall:
    assert cs.args.len >= 1
    buf.add cs.callNode
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    buf.addParRi()
    buf.addSubtree cs.fn.n
    for i in 1 ..< cs.args.len:
      buf.addSubtree cs.args[i].n
  of DotCall:
    assert cs.args.len == 1
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    buf.addSubtree cs.fn.n
  of SubscriptCall:
    buf.addParLe(AtX, cs.callNode.info)
    for a in cs.args:
      buf.addSubtree a.n
  of DotAsgnCall:
    assert cs.args.len == 2
    buf.addParLe(AsgnS, cs.callNode.info)
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    var callee = cs.fn.n
    let nameId = takeIdent(callee)
    assert nameId != StrId(0)
    var name = pool.strings[nameId]
    assert name[^1] == '='
    name.setLen name.len - 1
    buf.add identToken(pool.strings.getOrIncl(name), cs.callNode.info)
    buf.addParRi()
    buf.addSubtree cs.args[1].n
  of SubscriptAsgnCall:
    buf.addParLe(AsgnS, cs.callNode.info)
    buf.addParLe(AtX, cs.callNode.info)
    let valueIndex = cs.args.len - 1
    for i in 0 ..< valueIndex:
      buf.addSubtree cs.args[i].n
    buf.addParRi()
    buf.addSubtree cs.args[valueIndex].n
  buf.addParRi()

proc considerTypeboundOps(c: var SemContext; m: var seq[Match]; fnName: StrId; args: openArray[CallArg], genericArgs: Cursor, hasNamedArgs: bool) =
  # scope extension: procs attached to argument types are also considered
  # If the type is Typevar and it has attached
  # a concept, use the concepts symbols too:
  if fnName != StrId(0):
    # XXX maybe only trigger for open symchoice/ident callee, but the latter is not tracked
    var candidates = FnCandidates(marker: initHashSet[SymId]())
    # mark already matched symbols so that they don't get added:
    for i in 0 ..< m.len:
      if m[i].fn.sym != SymId(0):
        candidates.marker.incl m[i].fn.sym
    # add attached ops for each arg:
    for arg in args:
      let root = nominalRoot(arg.typ, allowTypevar = true)
      if root != SymId(0):
        addTypeboundOps c, fnName, root, candidates
    # now match them:
    for candidate in candidates.a:
      m.add createMatch(addr c)
      sigmatchNamedArgs(m[^1], candidate, args, genericArgs, hasNamedArgs)

proc addArgsInstConverters(c: var SemContext; m: var Match; origArgs: openArray[CallArg]) =
  if not (m.genericConverter or m.checkEmptyArg or m.insertedParam):
    c.dest.add m.args
  else:
    m.args.addParRi()
    var f = m.fn.typ
    if f.typeKind in RoutineTypes:
      inc f # skip ParLe
      for i in 1..4: skip f
    assert f.substructureKind == ParamsU
    inc f # "params"
    var arg = beginRead(m.args)
    var i = 0
    while arg.kind != ParRi:
      if m.insertedParam and arg.kind == DotToken:
        let param = asLocal(f)
        assert param.val.kind != DotToken
        var defaultValueBuf = createTokenBuf(30)
        var defaultValue = Item(n: param.val, typ: c.types.autoType)
        instantiateExprIntoBuf(c, defaultValueBuf, defaultValue, m.inferred)
        let prevErr = m.err
        swap m.args, c.dest
        typematch(m, param.typ, defaultValue)
        swap m.args, c.dest
        if m.err and not prevErr:
          c.typeMismatch arg.info, defaultValue.typ, param.typ
        inc arg
      elif m.checkEmptyArg and (isEmptyContainer(arg) or isEmptyOpenArrayCall(arg)):
        let isCall = arg.exprKind in CallKinds
        let start = c.dest.len
        if isCall:
          takeToken c, arg
          takeTree c, arg
        let isDoubleCall = arg.exprKind in CallKinds # `@` call inside `toOpenArray` call case
        if isDoubleCall:
          takeToken c, arg
          takeTree c, arg
        takeToken c, arg
        if containsGenericParams(arg):
          c.dest.addSubtree instantiateType(c, arg, m.inferred)
          skip arg
        else:
          takeTree c, arg
        takeParRi c, arg
        if isDoubleCall:
          takeParRi c, arg
        if isCall:
          takeParRi c, arg
          # instantiate `@`/`toOpenArray` call, done by semchecking:
          var callBuf = createTokenBuf(c.dest.len - start)
          for tok in start ..< c.dest.len:
            callBuf.add c.dest[tok]
          c.dest.shrink start
          var call = Item(n: beginRead(callBuf), typ: c.types.autoType)
          semCall c, call, {}
      elif m.genericConverter:
        var nested = 0
        while true:
          case arg.exprKind
          of HconvX:
            takeToken c, arg
            c.dest.takeTree arg # skip type
            inc nested
          of BaseobjX:
            takeToken c, arg
            # genericConverter is reused for object conversions to generic types
            if containsGenericParams(arg):
              c.dest.addSubtree instantiateType(c, arg, m.inferred)
              skip arg
            else:
              takeTree c, arg
            c.dest.takeTree arg # skip intlit
            inc nested
          of HderefX, HaddrX:
            takeToken c, arg
            inc nested
          else:
            break
        if arg.exprKind == HcallX:
          let convInfo = arg.info
          takeToken c, arg
          inc nested
          if arg.kind == Symbol:
            let sym = arg.symId
            takeToken c, arg
            let res = tryLoadSym(sym)
            if res.status == LacksNothing and res.decl.symKind == ConverterY:
              let routine = asRoutine(res.decl)
              if isGeneric(routine):
                let conv = FnCandidate(kind: routine.kind, sym: sym, typ: routine.params)
                var convMatch = createMatch(addr c)
                sigmatch convMatch, conv, [CallArg(n: arg, typ: origArgs[i].typ)], emptyNode(c)
                # ^ could also use origArgs[i] directly but commonType would have to keep the expression alive
                assert not convMatch.err
                buildTypeArgs(convMatch)
                if convMatch.err:
                  # adding type args errored
                  buildErr c, convInfo, getErrorMsg(convMatch)
                elif c.routine.inGeneric == 0:
                  let inst = c.requestRoutineInstance(conv.sym, convMatch.typeArgs, convMatch.inferred, convInfo)
                  c.dest[c.dest.len-1].setSymId inst.targetSym
                else:
                  # in generics, cannot instantiate yet
                  c.dest.shrink c.dest.len-1
                  c.dest.addParLe(AtX, convInfo)
                  c.dest.add symToken(conv.sym, convInfo)
                  c.dest.add convMatch.typeArgs
                  c.dest.addParRi()
        while true:
          case arg.kind
          of ParLe: inc nested
          of ParRi: dec nested
          else: discard
          takeToken c, arg
          if nested == 0: break
      else:
        takeTree c, arg
      skip f # should not be parri
      inc i
    assert f.kind == ParRi

proc tryConverterMatch(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: CallArg): bool =
  ## looks for a converter from `arg` to `f`, returns `true` if found and
  ## sets `convMatch` to the match to the converter
  result = false
  let root = nominalRoot(f)
  if root == SymId(0) and not c.g.config.compat: return
  var converters = c.converters.getOrDefault(root)
  if root != SymId(0) and c.g.config.compat:
    converters.add c.converters.getOrDefault(SymId(0))
  var convMatches: seq[Match] = @[]
  for conv in items converters:
    # f(a)
    # --> f(conv(a)) ?
    # conv's return type must match `f`.
    # conv's input type must match `a`.
    let res = tryLoadSym(conv)
    assert res.status == LacksNothing
    var fn = asRoutine(res.decl)
    assert fn.kind == ConverterY

    var inputMatch = createMatch(addr c)
    let candidate = FnCandidate(kind: fn.kind, sym: conv, typ: fn.params)

    var isEmptyOpenArray = false
    if arg.typ.typeKind == AutoT and isEmptyContainer(arg.n) and
        # normal overload of `toOpenArray` for arrays:
        (pool.syms[conv] == "toOpenArray.0." & SystemModuleSuffix or
          # normal overload of `toOpenArray` for seqs:
          pool.syms[conv] == "toOpenArray.1." & SystemModuleSuffix):
      # infer generic params of openarray converter, then match instantiated empty array/seq arg:
      isEmptyOpenArray = true
      var returnTypeMatch = createMatch(addr c)
      var returnType = candidate.typ
      skip returnType # get to return type
      typematch(returnTypeMatch, returnType, Item(n: emptyNode(c), typ: f))
      # if for some reason the openarray type doesn't match the converter:
      if classifyMatch(returnTypeMatch) notin {EqualMatch, GenericMatch}:
        continue
      inputMatch.inferred = returnTypeMatch.inferred

    # first match the input argument of `conv` so that the unification algorithm works as expected:
    sigmatch(inputMatch, candidate, [arg], emptyNode(c))
    if classifyMatch(inputMatch) notin {EqualMatch, GenericMatch, SubtypeMatch}:
      continue
    # use inputMatch.returnType here so the caller doesn't have to instantiate it again:
    if inputMatch.inferred.len != 0 and containsGenericParams(inputMatch.returnType):
      inputMatch.returnType = instantiateType(c, inputMatch.returnType, inputMatch.inferred)
    if isEmptyOpenArray:
      # argument is some empty array/seq literal populated with
      # toOpenArray's generic param as the type,
      # instantiate the type in the literal relative to the converter's generic params
      # so that only the generic params of the full call remain (if any exist)
      var instArgBuf = createTokenBuf(16)
      var argToInst = beginRead(inputMatch.args)
      assert isEmptyContainer(argToInst)
      let isCall = argToInst.exprKind in CallKinds
      if isCall:
        takeToken instArgBuf, argToInst
        takeTree instArgBuf, argToInst # call symbol
      takeToken instArgBuf, argToInst # array constructor tag
      instArgBuf.addSubtree instantiateType(c, argToInst, inputMatch.inferred)
      skip argToInst
      takeParRi instArgBuf, argToInst # array constructor
      if isCall:
        takeParRi instArgBuf, argToInst # call
      inputMatch.args = instArgBuf

    let dest = inputMatch.returnType
    var callBuf = createTokenBuf(16) # dummy call node to use for matching dest type
    callBuf.add parLeToken(HcallX, arg.n.info)
    callBuf.add symToken(conv, arg.n.info)
    callBuf.add inputMatch.args
    callBuf.addParRi()
    var newArg = Item(n: beginRead(callBuf), typ: dest)
    var fMatch = f
    var destMatch = createMatch(addr c)
    typematch(destMatch, fMatch, newArg)
    if classifyMatch(destMatch) in {EqualMatch, GenericMatch}:
      if isEmptyOpenArray:
        inputMatch.checkEmptyArg = true
        # make argument type `auto` so sigmatch can identify it and match it
        # needed if `f` is generic, since we don't know the generic parameters yet
        inputMatch.returnType = c.types.autoType
      elif isGeneric(fn):
        inputMatch.genericConverter = true
      convMatches.add inputMatch
  let idx = pickBestMatch(c, convMatches)
  if idx >= 0:
    result = true
    convMatch = convMatches[idx]

proc varargsHasConverter(t: Cursor): bool =
  var t = t
  assert t.typeKind == VarargsT
  inc t
  skip t
  result = t.kind != ParRi

proc tryVarargsConverter(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: CallArg): bool =
  result = false
  var baseType = f
  assert baseType.typeKind == VarargsT
  inc baseType
  var conv = baseType
  skip conv
  assert conv.kind != ParRi

  var callBuf = createTokenBuf(16)
  callBuf.addParLe(HcallX, arg.n.info)
  callBuf.addSubtree conv
  callBuf.addSubtree arg.n
  callBuf.addParRi()
  var call = beginRead(callBuf)
  var it = Item(n: call, typ: c.types.autoType)
  var destBuf = createTokenBuf(16)
  swap c.dest, destBuf
  semCall c, it, {} # might error
  swap c.dest, destBuf
  it.n = beginRead(destBuf)

  var match = createMatch(addr c)
  typematch(match, baseType, it)
  let matchKind = classifyMatch(match)
  if matchKind >= GenericMatch:
    if matchKind == GenericMatch:
      match.genericConverter = true
    result = true
    convMatch = ensureMove(match)

proc resolveOverloads(c: var SemContext; it: var Item; cs: var CallState) =
  let genericArgs =
    if cs.hasGenericArgs: cursorAt(cs.genericDest, 0)
    else: emptyNode(c)

  var m: seq[Match] = @[]
  if cs.fn.n.exprKind in {OchoiceX, CchoiceX}:
    var f = cs.fn.n
    inc f
    while f.kind != ParRi:
      if f.kind == Symbol:
        let sym = f.symId
        let s = fetchSym(c, sym)
        let typ = fetchCallableType(c, f, s)
        if typ.typeKind in RoutineTypes:
          let candidate = FnCandidate(kind: s.kind, sym: sym, typ: typ)
          m.add createMatch(addr c)
          sigmatchNamedArgs(m[^1], candidate, cs.args, genericArgs, cs.hasNamedArgs)
      else:
        buildErr c, cs.fn.n.info, "`choice` node does not contain `symbol`"
      inc f
    considerTypeboundOps(c, m, cs.fnName, cs.args, genericArgs, cs.hasNamedArgs)
    if m.len == 0:
      # symchoice contained no callable symbols and no typebound ops
      assert cs.fnName != StrId(0)
      buildErr c, cs.fn.n.info, "attempt to call routine: '" & pool.strings[cs.fnName] & "'"
  elif cs.fn.n.kind == Ident:
    # error should have been given above already:
    # buildErr c, fn.n.info, "attempt to call undeclared routine"
    discard
  elif cs.fn.typ.typeKind == TypedescT and cs.args.len == 1:
    closeArgsScope c, cs
    semConvFromCall c, it, cs
    return
  elif cs.fn.typ.typeKind == TypedescT and cs.args.len == 0:
    closeArgsScope c, cs
    semObjConstrFromCall c, it, cs
    return
  else:
    # Keep in mind that proc vars are a thing:
    let sym = if cs.fn.n.kind == Symbol: cs.fn.n.symId else: SymId(0)
    let typ = cs.fn.typ
    if typ.typeKind in RoutineTypes:
      let candidate = FnCandidate(kind: cs.fnKind, sym: sym, typ: typ)
      m.add createMatch(addr c)
      sigmatchNamedArgs(m[^1], candidate, cs.args, genericArgs, cs.hasNamedArgs)
      considerTypeboundOps(c, m, cs.fnName, cs.args, genericArgs, cs.hasNamedArgs)
    elif sym != SymId(0):
      # non-callable symbol, look up all overloads
      assert cs.fnName != StrId(0)
      var choiceBuf = createTokenBuf(16)
      swap c.dest, choiceBuf
      discard buildSymChoice(c, cs.fnName, cs.fn.n.info, FindAll)
      swap c.dest, choiceBuf
      # could store choiceBuf in CallState but cs.fn should not outlive it
      cs.fn = Item(n: beginRead(choiceBuf), typ: c.types.autoType, kind: CchoiceY)
      resolveOverloads(c, it, cs)
      return
    else:
      buildErr c, cs.fn.n.info, "cannot call expression of type " & typeToString(typ)
  var idx = pickBestMatch(c, m, cs.flags)

  if idx < 0:
    # try converters
    var matchAdded = false
    let L = m.len
    var csArgsOrig: seq[CallArg] = @[]
    if cs.hasNamedArgs:
      csArgsOrig = move cs.args
    for mi in 0 ..< L:
      if not m[mi].err: continue
      var newMatch = createMatch(addr c)
      var newArgs: seq[CallArg] = @[]
      var newArgBufs: seq[TokenBuf] = @[] # to keep alive
      var param = skipProcTypeToParams(m[mi].fn.typ)
      if cs.hasNamedArgs:
        cs.args = orderArgs(newMatch, param, csArgsOrig)
      assert param.isParamsTag
      inc param
      var ai = 0
      var anyConverters = false
      while param.kind != ParRi:
        # varargs not handled yet
        if ai >= cs.args.len: break
        let f = asLocal(param).typ
        let isVarargs = f.typeKind == VarargsT
        if not isVarargs:
          skip param
        var arg = cs.args[ai]
        var convMatch = default(Match)
        if isVarargs and varargsHasConverter(f) and tryVarargsConverter(c, convMatch, f, arg):
          anyConverters = true
          # match already built call, just use it
          let bufPos = newArgBufs.len
          newArgBufs.add ensureMove(convMatch.args)
          var baseType = f
          inc baseType
          if convMatch.genericConverter:
            # can just instantiate here
            baseType = instantiateType(c, baseType, convMatch.inferred)
          newArgs.add CallArg(n: beginRead(newArgBufs[bufPos]), typ: baseType)
        elif tryConverterMatch(c, convMatch, f, arg):
          anyConverters = true
          var argBuf = createTokenBuf(16)
          argBuf.add parLeToken(HcallX, arg.n.info)
          argBuf.add symToken(convMatch.fn.sym, arg.n.info)
          if convMatch.checkEmptyArg:
            # empty openarray converter
            newMatch.checkEmptyArg = true
          elif convMatch.genericConverter:
            # instantiate after match
            newMatch.genericConverter = true
          argBuf.add convMatch.args
          argBuf.addParRi()
          let bufPos = newArgBufs.len
          newArgBufs.add ensureMove(argBuf)
          newArgs.add CallArg(n: beginRead(newArgBufs[bufPos]), typ: convMatch.returnType)
        else:
          newArgs.add arg
        inc ai
      if anyConverters:
        sigmatch(newMatch, m[mi].fn, newArgs, genericArgs)
        m.add newMatch
        matchAdded = true
    if matchAdded: # m.len != L
      idx = pickBestMatch(c, m, cs.flags)

  if idx >= 0:
    c.dest.add cs.callNode
    let finalFn = m[idx].fn
    # only merge symbols defined in args to scope if we did not match a macro/template:
    closeArgsScope c, cs, merge = finalFn.kind notin {MacroY, TemplateY}
    let isMagic = c.addFn(finalFn, cs.fn.n, m[idx])
    addArgsInstConverters(c, m[idx], cs.args)
    takeParRi c, it.n
    buildTypeArgs(m[idx])

    if m[idx].err:
      # adding args or type args may have errored
      if finalFn.sym != SymId(0) and
          # overload of `@` with empty array param:
          pool.syms[finalFn.sym] == "@.1." & SystemModuleSuffix and
          (AllowEmpty in cs.flags or isSomeSeqType(it.typ) or isSomeOpenArrayType(it.typ)):
        # empty seq will be handled, either by `commonType` now or
        # the call this is an argument of in the case of AllowEmpty
        typeofCallIs c, it, cs.beforeCall, c.types.autoType
      else:
        buildErr c, cs.callNode.info, getErrorMsg(m[idx])
    elif finalFn.kind == TemplateY:
      if c.templateInstCounter <= MaxNestedTemplates:
        c.expanded.addSymUse finalFn.sym, cs.callNode.info
        inc c.templateInstCounter
        withErrorContext c, cs.callNode.info:
          semTemplateCall c, it, finalFn.sym, cs.beforeCall, m[idx]
        dec c.templateInstCounter
      else:
        buildErr c, cs.callNode.info, "recursion limit exceeded for template expansions"
    elif isMagic == MagicCallNeedsSemcheck:
      # semcheck produced magic expression
      var magicExprBuf = createTokenBuf(c.dest.len - cs.beforeCall)
      magicExprBuf.addUnstructured cursorAt(c.dest, cs.beforeCall)
      endRead(c.dest)
      c.dest.shrink cs.beforeCall
      var magicExpr = Item(n: cursorAt(magicExprBuf, 0), typ: it.typ)
      semExpr c, magicExpr, cs.flags
      it.typ = magicExpr.typ
    elif finalFn.kind == IteratorY and PreferIterators notin cs.flags:
      buildErr c, cs.callNode.info, "Iterators can be called only in `for` statements"
    elif m[idx].inferred.len > 0:
      var matched = m[idx]
      let returnType: Cursor
      if isMagic == NonMagicCall and c.routine.inGeneric == 0:
        let inst = c.requestRoutineInstance(finalFn.sym, matched.typeArgs, matched.inferred, cs.callNode.info)
        c.dest[cs.beforeCall+1].setSymId inst.targetSym
        var instReturnType = createTokenBuf(16)
        swap c.dest, instReturnType
        var subsReturnType = inst.returnType
        returnType = semReturnType(c, subsReturnType)
        swap c.dest, instReturnType
      else:
        if isMagic == NonMagicCall and cs.hasGenericArgs:
          # add back explicit generic args since we cannot instantiate
          var invokeBuf = createTokenBuf(16)
          invokeBuf.addParLe(AtX, cs.fn.n.info)
          invokeBuf.add symToken(finalFn.sym, cs.fn.n.info)
          var genericArgsRead = genericArgs
          while genericArgsRead.kind != ParRi:
            takeTree invokeBuf, genericArgsRead
          invokeBuf.addParRi()
          replace c.dest, beginRead(invokeBuf), cs.beforeCall+1
        if matched.returnType.kind == DotToken:
          returnType = matched.returnType
        else:
          returnType = instantiateType(c, matched.returnType, matched.inferred)
      typeofCallIs c, it, cs.beforeCall, returnType
    else:
      var returnType = m[idx].returnType

      var returnTypeBuf = createTokenBuf()
      swap c.dest, returnTypeBuf
      returnType = semReturnType(c, returnType)
      swap c.dest, returnTypeBuf

      typeofCallIs c, it, cs.beforeCall, returnType

  else:
    skipParRi it.n
    # do not add symbols defined in args on failed match:
    closeArgsScope c, cs, merge = false
    var errored = createTokenBuf(4)
    buildCallSource errored, cs
    let erroredN = cursorAt(errored, 0)
    var errorMsg: string
    if idx == -2:
      errorMsg = "ambiguous call: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
    elif cs.source in {DotCall, DotAsgnCall} and cs.fnName != StrId(0):
      errorMsg = "undeclared field: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
      if cs.args.len != 0: # just to be safe
        errorMsg.add " for type "
        errorMsg.add typeToString(cs.args[0].typ)
    elif m.len > 0:
      errorMsg = "Type mismatch at [position]\n"
      errorMsg.add asNimCode erroredN
      for i in 0..<m.len:
        errorMsg.add "\n"
        addErrorMsg errorMsg, m[i]
        let res = tryLoadSym(m[i].fn.sym)
        assert res.status == LacksNothing
        errorMsg.add " (declared in " & res.decl.info.infoToStr & ")"
    else:
      errorMsg = "undeclared identifier: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
    buildErr c, cs.callNode.info, errorMsg, erroredN

proc getFnIdent(c: var SemContext): StrId =
  var n = beginRead(c.dest)
  result = takeIdent(n)
  endRead(c.dest)

proc findMagicInSyms(syms: Cursor): ExprKind =
  var syms = syms
  result = NoExpr
  var nested = 0
  while true:
    case syms.kind
    of Symbol:
      let res = tryLoadSym(syms.symId)
      if res.status == LacksNothing:
        var n = res.decl
        inc n # skip the symbol kind
        if n.kind == SymbolDef:
          inc n # skip the SymbolDef
          if n.kind == ParLe:
            result = n.exprKind
            if result != NoExpr: break
    of ParLe:
      if syms.exprKind notin {OchoiceX, CchoiceX}: break
      inc nested
    of ParRi:
      dec nested
    else: break
    if nested == 0: break
    inc syms

proc unoverloadableMagicCall(c: var SemContext; it: var Item; cs: var CallState; magic: ExprKind) =
  let nifTag = parLeToken(magic, cs.callNode.info)
  if cs.args.len != 0:
    # keep args after if they were produced by dotcall:
    cs.dest.replace fromBuffer([nifTag]), 0
  else:
    cs.dest.shrink 0
    cs.dest.add nifTag
  while it.n.kind != ParRi:
    # add all args in call:
    takeTree cs.dest, it.n
  takeParRi cs.dest, it.n
  var magicCall = Item(n: beginRead(cs.dest), typ: it.typ)
  semExpr c, magicCall, cs.flags
  it.typ = magicCall.typ

proc semCall(c: var SemContext; it: var Item; flags: set[SemFlag]; source: TransformedCallSource = RegularCall) =
  var cs = CallState(
    beforeCall: c.dest.len,
    callNode: it.n.load(),
    dest: createTokenBuf(16),
    source: source,
    flags: {InTypeContext, AllowEmpty, PreferIterators}*flags
  )
  inc it.n
  # open temp scope for args, has to be closed after matching:
  openShadowScope(c.currentScope)
  swap c.dest, cs.dest
  cs.fn = Item(n: it.n, typ: c.types.autoType)
  var argIndexes: seq[int] = @[]
  if cs.fn.n.exprKind == AtX:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {KeepMagics, AllowUndeclared} # don't consider all overloads
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    var maybeRoutine = lhs.n
    if maybeRoutine.exprKind in {OchoiceX, CchoiceX}:
      inc maybeRoutine
    if maybeRoutine.kind == Symbol:
      let res = tryLoadSym(maybeRoutine.symId)
      assert res.status == LacksNothing
      if isRoutine(res.decl.symKind) and isGeneric(asRoutine(res.decl)):
        cs.hasGenericArgs = true
        cs.genericDest = createTokenBuf(16)
        swap c.dest, cs.genericDest
        while cs.fn.n.kind != ParRi:
          semLocalTypeImpl c, cs.fn.n, AllowValues
        takeParRi c, cs.fn.n
        swap c.dest, cs.genericDest
        it.n = cs.fn.n
        c.dest.addSubtree lhs.n
        cs.fn.typ = lhs.typ
        cs.fn.kind = lhs.kind
        cs.fnName = getFnIdent(c)
    if not cs.hasGenericArgs:
      semBuiltinSubscript(c, cs.fn, lhs)
      cs.fnName = getFnIdent(c)
      it.n = cs.fn.n
  elif cs.fn.n.exprKind == DotX:
    let dotStart = c.dest.len
    let dotInfo = cs.fn.n.info
    # read through the dot expression first:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    let lhsOrig = cs.fn.n
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {AllowModuleSym}
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    let fieldNameCursor = cs.fn.n
    let fieldName = takeIdent(cs.fn.n)
    # skip optional inheritance depth:
    if cs.fn.n.kind == IntLit:
      inc cs.fn.n
    skipParRi cs.fn.n
    it.n = cs.fn.n
    # now interpret the dot expression:
    let dotState = tryBuiltinDot(c, cs.fn, lhs, fieldName, dotInfo, {KeepMagics, AllowUndeclared, AllowOverloads})
    if dotState == FailedDot or
        # also ignore non-proc fields:
        (dotState == MatchedDotField and cs.fn.typ.typeKind notin RoutineTypes):
      cs.source = MethodCall
      # turn a.b(...) into b(a, ...)
      # first, delete the output of `tryBuiltinDot`:
      c.dest.shrink dotStart
      # sem b:
      cs.fn = Item(n: fieldNameCursor, typ: c.types.autoType)
      semExpr c, cs.fn, {KeepMagics, AllowUndeclared, AllowOverloads}
      cs.fnName = getFnIdent(c)
      # add a as argument:
      let lhsIndex = c.dest.len
      c.dest.addSubtree lhs.n
      argIndexes.add lhsIndex
      cs.args.add CallArg(typ: lhs.typ, orig: lhsOrig) # n will be set by argIndexes
  else:
    semExpr(c, cs.fn, {KeepMagics, AllowUndeclared, AllowOverloads})
    cs.fnName = getFnIdent(c)
    it.n = cs.fn.n
  if c.g.config.compat and cs.fnName in c.unoverloadableMagics:
    # transform call early before semchecking arguments
    let syms = beginRead(c.dest)
    let magic = findMagicInSyms(syms)
    endRead(c.dest)
    if magic != NoExpr:
      swap c.dest, cs.dest
      unoverloadableMagicCall(c, it, cs, magic)
      return
  when defined(debug):
    let oldDebugAllowErrors = c.debugAllowErrors
    if cs.fnName in c.unoverloadableMagics:
      c.debugAllowErrors = true
  cs.fnKind = cs.fn.kind
  var skipSemCheck = false
  while it.n.kind != ParRi:
    let argOrig = it.n
    var arg = Item(n: it.n, typ: c.types.autoType)
    argIndexes.add c.dest.len
    let named = arg.n.substructureKind == VvU
    if named:
      cs.hasNamedArgs = true
      takeToken c, arg.n
      takeTree c, arg.n
    semExpr c, arg, {AllowEmpty}
    if named:
      takeParRi c, arg.n
    if arg.typ.typeKind == UntypedT:
      skipSemCheck = true
    it.n = arg.n
    cs.args.add CallArg(typ: arg.typ, orig: argOrig) # n will be set by argIndexes
  when defined(debug):
    c.debugAllowErrors = oldDebugAllowErrors
  assert cs.args.len == argIndexes.len
  swap c.dest, cs.dest
  cs.fn.n = beginRead(cs.dest)
  for i in 0 ..< cs.args.len:
    cs.args[i].n = cursorAt(cs.dest, argIndexes[i])
  if skipSemCheck:
    untypedCall c, it, cs
  else:
    resolveOverloads c, it, cs
  assert cs.argsScopeClosed

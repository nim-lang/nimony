import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import hexer_context, passes
import ".." / nimony / [nimony_model, programs, decls, typenav]
import ".." / finalir / finalir_model
import duplifier


proc createDecl(e: var EContext; dest: var TokenBuf; destSym: SymId;
        typ: var Cursor; value: var Cursor;
        info: NifLineInfo; kind: StmtKind; needsAddr: bool) =
  assert typ.hasMore
  dest.addParLe kind, info
  dest.addSymDef(destSym, info)
  dest.addDotToken()
  dest.addDotToken()
  takeTree(dest, typ)
  if needsAddr:
    dest.copyIntoKind HaddrX, info:
      takeTree(dest, value)
  else:
    takeTree(dest, value)
  dest.addParRi()

proc createTupleAccess(left: TokenBuf; i: int; info: NifLineInfo): TokenBuf =
  result = createTokenBuf()
  result.addParLe(TupatX, info)
  result.add left
  result.addIntLit(i, info)
  result.addParRi()

proc getForVars(e: var EContext, forVars: Cursor): seq[Cursor] =
  result = @[]
  var forVars = forVars
  if forVars.substructureKind notin {UnpackflatU, UnpacktupU}:
    error e, "`unpackflat` or `unpacktup` expected, but got: ", forVars
  forVars = sub(forVars) # unpackflat/unpacktup; peek only, never left
  while forVars.hasMore:
    result.add forVars
    skip forVars

proc connectSingleExprToLoopVar(e: var EContext; dest: var TokenBuf; c: var Cursor;
          forVar: Cursor; res: var Table[SymId, SymId]) =
  let local = asLocal(forVar)
  let destSym = local.name.symId
  let info = local.name.info
  case c.kind
  of Symbol:
    let val = c.symId
    res[destSym] = val
    inc c, SkipName
  else:
    var typ = local.typ
    # Fresh SymId per yield expansion
    let freshSym = pool.symId("`ii." & $e.getTmpId)
    res[destSym] = freshSym
    createDecl(e, dest, freshSym, typ, c, info, VarS, needsAddr=false)

proc unpackTupleAccess(e: var EContext; dest: var TokenBuf; forVar: Cursor;
                       left: TokenBuf; i: int; info: NifLineInfo; typ: Cursor;
                       needsAddr: bool; res: var Table[SymId, SymId]) =
  assert typ.hasMore
  let local = asLocal(forVar)
  # A fresh symbol PER YIELD, and a mapping entry so the body reads it —
  # exactly what `connectSingleExprToLoopVar` does for the single-variable
  # case. Re-using the for-loop variable's own symbol declared it once per
  # `yield`, so an iterator with two of them emitted two `(let :f.0 …)` for the
  # same name in one proc. The C backend keeps them apart by scope; the native
  # one resolves a symbol by name, so the second declaration took over the
  # first's storage and `for (f, l) in it()` read `f` from the wrong yield.
  let destSym = local.name.symId
  let freshSym = pool.symId("`ii." & $e.getTmpId)
  res[destSym] = freshSym
  var tupBuf = createTupleAccess(left, i, info)
  var tup = beginRead(tupBuf)
  var localTyp = local.typ
  createDecl(e, dest, freshSym, localTyp, tup, info, LetS, needsAddr)

proc startTupleAccess(s: SymId; info: NifLineInfo; needsDeref: bool): TokenBuf =
  result = createTokenBuf()
  if needsDeref:
    result.copyIntoKind HderefX, info:
      result.addSymUse(s, info)
  else:
    result.addSymUse(s, info)

proc createYieldMapping(e: var EContext; dest: var TokenBuf; c: var Cursor, vars: Cursor, yieldType: Cursor): Table[SymId, SymId] =
  result = initTable[SymId, SymId]()

  let forVars = getForVars(e, vars)

  if forVars.len == 1:
    connectSingleExprToLoopVar(e, dest, c, forVars[0], result)
  else:
    if c.isTagLit and c.exprKind == TupX:
      c.into:
        var i = 0
        while c.hasMore:
          connectSingleExprToLoopVar(e, dest, c, forVars[i], result)
          inc i
    else:
      let tmpId: SymId
      let info: NifLineInfo
      var typ = yieldType.skipModifier()
      let needsDeref = yieldType.typeKind in {LentT, MutT}
      assert typ.typeKind == TupleT
      if c.isSymbol:
        tmpId = c.symId
        info = c.info
        inc c, SkipName
      else:
        tmpId = pool.symId("`ii." & $e.getTmpId)
        info = c.info
        var typCur = yieldType
        createDecl(e, dest, tmpId, typCur, c, info, LetS, needsAddr=false)

      typ = sub(typ) # skips tuple; peek only, never left
      for i in 0..<forVars.len:
        let isKvU = typ.substructureKind == KvU
        var kvStart = default(Cursor)
        if isKvU:
          kvStart = typ; typ = sub(typ) # skip tag
          skip typ # skip name

        if forVars[i].substructureKind in {UnpacktupU, UnpackflatU}:
          var counter = 0
          var unpackCursor = forVars[i]
          unpackCursor = sub(unpackCursor) # peek only, never left
          var left = startTupleAccess(tmpId, info, needsDeref)
          # The yielded element may itself be wrapped in `var`/`lent`/etc.
          # (e.g. `pairs(seq[T])` yields `(int, var T)`). Peel any modifier
          # before walking into the inner tuple, and remember to close it at
          # the end so the outer `typ` cursor advances past the whole element.
          # Modifier-wrapped element types lower to a pointer, so the access
          # path needs an `hderef` around `tmp[i]` before indexing into the
          # inner tuple.
          let hasModifier = typ.isTagLit and typ.typeKind in TypeModifiers
          var leftTupleAccess = createTupleAccess(left, i, info)
          var modStart = default(Cursor)
          if hasModifier:
            var deref = createTokenBuf()
            deref.copyIntoKind HderefX, info:
              deref.add leftTupleAccess
            leftTupleAccess = deref
            modStart = typ; typ = sub(typ)
          assert typ.typeKind == TupleT
          typ.into:
            # When we deref'd a `var`/`lent`/... element above, the resulting
            # tuple-field accesses are by-value but the for-vars are still typed
            # as `var T`/`lent T`/... — sem propagates the outer modifier to
            # every unpacked sub-var. Pass `needsAddr` through so each `let sym
            # = (tupat ...)` is wrapped in `(haddr ...)`.
            let innerNeedsAddr = needsDeref or hasModifier
            while unpackCursor.hasMore:
              unpackTupleAccess(e, dest, unpackCursor, leftTupleAccess, counter, info, typ, innerNeedsAddr, result)
              inc counter
              skip unpackCursor
              skip typ
          if hasModifier:
            typ = modStart; skip typ
        else:
          var left = startTupleAccess(tmpId, info, needsDeref)
          unpackTupleAccess(e, dest, forVars[i], left, i, info, typ, needsDeref, result)
          skip typ

        if isKvU:
          typ = kvStart; skip typ

proc transformStmt(e: var EContext; dest: var TokenBuf; c: var Cursor)
proc transformForFir(e: var EContext; dest: var TokenBuf; c: var Cursor)

proc replaceSymbol(e: var EContext; dest: var TokenBuf; c: var Cursor; relations: var Table[SymId, SymId]) =
  case c.kind
  of DotToken:
    dest.addSubtree c
    inc c
  of TagLit:
    case c.stmtKind
    of VarS, LetS, CursorS, PatternvarS:
      takeInto dest, c:
        let oldName = c.symId
        let newName = pool.symId("`lf." & $e.instId)
        inc e.instId
        relations[oldName] = newName
        dest.addSymDef(newName, c.info)
        inc c
        while c.hasMore:
          replaceSymbol(e, dest, c, relations)
    of PragmasS:
      # Pragma lists declare no locals to rename, so copy them verbatim instead
      # of descending. This also keeps us from treating a `(cursor)` *pragma* as
      # a `cursor` declaration — the two share a tag — which would misread the
      # following `)` as the decl's name.
      dest.takeTree c
    of CallS, CmdS, GvarS, TvarS, ConstS, ResultS, GletS, TletS,
        ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
        TemplateS, TypeS, BlockS, EmitS, AsgnS, ScopeS, IfS,
        WhenS, BreakS, ContinueS, ForS, WhileS, CoroforS, CaseS,
        RetS, YldS, StmtsS, PragmaxS, InclS, ExclS,
        IncludeS, ImportS, ImportasS, FromimportS, ImportexceptS,
        ExportS, ExportexceptS, CommentS, DiscardS, TryS, RaiseS,
        UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS,
        PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, LabS, JmpS, NoStmt:
      if c.substructureKind == KvU:
        # In KvU: first element is field name, don't substitute it
        takeInto dest, c:
          dest.takeTree c
          while c.hasMore:
            replaceSymbol(e, dest, c, relations)
      elif c.exprKind in {DotX, DdotX}:
        takeInto dest, c:
          replaceSymbol(e, dest, c, relations)
          while c.hasMore:
            dest.takeTree c
      else:
        takeInto dest, c:
          while c.hasMore:
            replaceSymbol(e, dest, c, relations)
  of Symbol:
    let s = c.symId
    if relations.hasKey(s):
      dest.addSymUse(relations.getOrQuit(s), c.info)
    else:
      dest.addSubtree c
    inc c
  else:
    takeTree(dest, c)

proc rewriteYieldsAndCopy(e: var EContext; dest: var TokenBuf;
                           c: var Cursor; resultSym: SymId) =
  ## Walk one subtree from `c` into `dest`, rewriting each `(yld v)` (where v
  ## is not the dot-token) into the sequence `(asgn resultSym v); (yld .)`.
  ## Nested proc/iter/template/macro/type decls are passed through verbatim
  ## (they have their own yield contexts, if any).
  case c.kind
  of TagLit:
    let sk = c.stmtKind
    if sk == YldS:
      let info = c.info
      let headTag = c.cursorTagId
      c.into: # past yld tag
        if c.isDotToken:
          # bare yield (void return) — leave as-is
          dest.addParLe(headTag, info)
          dest.takeTree c # the dot token
        else:
          # (yld v) ⇒ (asgn resultSym v) ; (yld .)
          dest.copyIntoKind AsgnS, info:
            dest.addSymUse resultSym, info
            dest.takeTree c # v
          dest.addParLe(headTag, info)
          dest.addDotToken()
        dest.addParRi(c.endInfo) # close original yld
    elif sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                TemplateS, TypeS}:
      dest.takeTree c
    else:
      takeInto dest, c:
        while c.hasMore:
          rewriteYieldsAndCopy(e, dest, c, resultSym)
  else:
    dest.takeTree c

proc rewriteClosureIter(e: var EContext; dest: var TokenBuf;
                        c: var Cursor; retType: Cursor) =
  ## Inject `(result :synth . . T .)` at the head of a `.closure` iterator's
  ## body and rewrite every `(yld v)` to `(asgn synth v); (yld .)`. With this,
  ## destroyer sees a typed asgn to a `result` local and injects the proper
  ## `=destroy old; =copy/move new` hooks. cps.nim's existing ResultS handling
  ## then lifts `synth` to `(deref env.result.0)`.
  let synthResultSym = pool.symId(
    "`coroResult." & $getTmpId(e) & "." & e.main)

  dest.addParLe(c.cursorTagId, c.info) # IteratorS tag
  let iterStart = c
  c = sub(c)
  for _ in 0..<BodyPos:
    dest.takeTree c

  # Now at body. Either DotToken (forward decl) or a (stmts ...).
  if c.isDotToken:
    dest.takeTree c
    dest.addParRi(c.endInfo)
    c = iterStart; skip c
    return
  if c.stmtKind != StmtsS:
    dest.takeTree c
    dest.addParRi(c.endInfo)
    c = iterStart; skip c
    return

  dest.addParLe(c.cursorTagId, c.info) # body's StmtsS opening
  c.into:

    let info = c.info
    dest.copyIntoKind ResultS, info:
      dest.addSymDef synthResultSym, info
      dest.addDotToken() # exported
      dest.addDotToken() # pragmas
      dest.copyTree retType
      dest.addDotToken() # value

    # Inline the body's own for-loops first: `transformStmt`'s IteratorS case
    # routes a `.closure`/`.passive` iter decl straight here, so nothing else
    # ever descends into its body. An inner `for i in a..b:` would survive as
    # a `(for ...)` into duplifier, which then trips over the for-var that no
    # pass ever declared ("could not find symbol: i.0").
    # Inlining must run before the yield rewrite: the inline iterator's
    # expansion copies the loop body once per inner yield, and each copy may
    # contain an outer `(yld ...)` that still needs rewriting.
    var bodyBuf = createTokenBuf()
    while c.hasMore:
      transformStmt(e, bodyBuf, c)

    var body = beginRead(bodyBuf)
    while body.hasMore:
      rewriteYieldsAndCopy(e, dest, body, synthResultSym)

    dest.addParRi(c.endInfo) # close body stmts
  dest.addParRi(c.endInfo) # close iter decl
  c = iterStart; skip c

# ----------------------------------------------------------------------------
# The pipeline lowers the module before this pass, so a `for` arrives as
#
#   (for <iter call> <vars> (scope <body> (continue .)))  (lab :exit)
#
# with every `break` already a `(jmp exit)` and every source-level `continue` a
# `jmp` to a label in front of the back-edge. What is left to do per `yield` is
# a copy of the body without the back-edge. The iterator's own body
# comes from `tryLoadSym`, i.e. from a module nif that is not lowered yet, so
# it is lowered here. Every copy declares its own locals and labels: a
# lowering run and a body copy both reuse names, and one routine must not.

proc collectDefs(e: var EContext; n: Cursor; mapping: var Table[SymId, SymId]) =
  ## A fresh name for every local and label `n` declares. Labels have to be
  ## known up front: a `jmp` comes before its `lab`.
  var n = n
  if not n.isTagLit: return
  let sk = n.stmtKind
  case sk
  of VarS, LetS, CursorS, PatternvarS, ResultS, LabS:
    let d = n.childCursor
    if d.kind == SymbolDef:
      mapping[d.symId] = pool.symId("`ii." & $e.getTmpId)
  of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
     PragmasS:
    return # nested declarations own their names
  else: discard
  n = sub(n)   # peek only, never left
  while n.hasMore:
    collectDefs(e, n, mapping)
    skip n

proc copyFreshened(dest: var TokenBuf; c: var Cursor; mapping: Table[SymId, SymId]) =
  ## `copyWithMapping`, renaming the declarations in `mapping` as well.
  case c.kind
  of TagLit:
    if c.exprKind in {DotX, DdotX}:
      dest.addParLe(c.cursorTagId, c.info)
      c.into:
        copyFreshened(dest, c, mapping) # object expression
        while c.hasMore:
          dest.takeTree c # field selector + optional depth/access token
        dest.addParRi(c.endInfo)
    elif c.substructureKind == KvU:
      dest.addParLe(c.cursorTagId, c.info)
      c.into:
        dest.takeTree c # field name
        while c.hasMore:
          copyFreshened(dest, c, mapping)
        dest.addParRi(c.endInfo)
    elif c.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                        TemplateS, TypeS, PragmasS}:
      dest.takeTree c
    else:
      dest.addParLe(c.cursorTagId, c.info)
      c.into:
        while c.hasMore:
          copyFreshened(dest, c, mapping)
        dest.addParRi(c.endInfo)
  of Symbol:
    dest.addSymUse(mapping.getOrDefault(c.symId, c.symId), c.info)
    inc c
  of SymbolDef:
    dest.addSymDef(mapping.getOrDefault(c.symId, c.symId), c.info)
    inc c
  else:
    dest.addSubtree c
    inc c

proc inlineForBody(e: var EContext; dest: var TokenBuf; c: var Cursor;
                   mapping: Table[SymId, SymId]) =
  ## One statement of a `for` body: `copyFreshened`, with every nested `for`
  ## inlined afresh for each copy of the enclosing body.
  if c.stmtKind == ForS:
    var forBuf = createTokenBuf()
    copyFreshened(forBuf, c, mapping)
    var f = beginRead(forBuf)
    transformForFir(e, dest, f)
  elif c.kind != TagLit or c.exprKind in {DotX, DdotX} or
      c.substructureKind == KvU or
      c.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                     TemplateS, TypeS, PragmasS}:
    copyFreshened(dest, c, mapping)
  else:
    dest.addParLe(c.cursorTagId, c.info)
    c.into:
      while c.hasMore:
        inlineForBody(e, dest, c, mapping)
      dest.addParRi(c.endInfo)

proc emitForBody(e: var EContext; dest: var TokenBuf; body: Cursor;
                 mapping: Table[SymId, SymId]; prefix: TokenBuf) =
  ## `(scope <prefix> <body without its back-edge>)`. The back-edge is the
  ## body's last statement and its only `continue`: the lowering spells a
  ## source-level one as a `jmp` to a label in front of it.
  var b = body
  let info = b.info
  dest.addParLe ScopeS, info
  dest.add prefix
  if b.stmtKind in {StmtsS, ScopeS}:
    b = sub(b)   # peek only, never left
    while b.hasMore:
      var probe = b
      skip probe
      if b.stmtKind == ContinueS and not probe.hasMore:
        skip b, ContinueS # falling off the copy does it
      else:
        inlineForBody(e, dest, b, mapping)
  else:
    inlineForBody(e, dest, b, mapping)
  dest.addParRi()

proc inlineIteratorBodyFir(e: var EContext; dest: var TokenBuf;
                           c: var Cursor; forStmt: ForStmt; yieldType: Cursor) =
  if c.isTagLit:
    if c.stmtKind == YldS:
      c.into: # skips yield
        var mapping = createYieldMapping(e, dest, c, forStmt.vars, yieldType)
        collectDefs(e, forStmt.body, mapping)
        emitForBody(e, dest, forStmt.body, mapping, createTokenBuf(0))
    elif c.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                        TemplateS, TypeS}:
      dest.takeTree c
    else:
      takeInto dest, c:
        while c.hasMore:
          inlineIteratorBodyFir(e, dest, c, forStmt, yieldType)
  else:
    takeTree(dest, c)

proc emitCoroForFir(e: var EContext; dest: var TokenBuf; forStmt: ForStmt) =
  ## `emitCoroFor` for Final IR input: the body is a `scope`, its `continue`s
  ## jump to its end, and no block is needed for `break`.
  var iterCur = forStmt.iter
  if iterCur.exprKind == HderefX:
    inc iterCur
  if iterCur.exprKind notin CallKinds:
    error e, "closure iterator must be invoked directly in a for-loop, got: ",
      forStmt.iter
  let info = iterCur.info
  let forVars = getForVars(e, forStmt.vars)
  let forLoopVarSym: SymId
  if forVars.len == 1:
    dest.copyTree forVars[0]
    forLoopVarSym = asLocal(forVars[0]).name.symId
  else:
    var symProbe = iterCur
    inc symProbe # past Call tag
    if not symProbe.isSymbol:
      error e, "closure iterator call must target a symbol, got: ", iterCur
    let res = tryLoadSym(symProbe.symId)
    if res.status != LacksNothing:
      error e, "could not load closure-iter sym: " & pool.symString(symProbe.symId)
    let routine = asRoutine(res.decl, SkipInclBody)
    var retType = routine.retType
    if retType.typeKind in {MutT, LentT}:
      inc retType
    forLoopVarSym = pool.symId("`coroTup." & $getTmpId(e))
    dest.copyIntoKind LetS, info:
      dest.addSymDef forLoopVarSym, info
      dest.addDotToken() # exported
      dest.addDotToken() # pragmas
      dest.copyTree retType
      dest.addDotToken() # no initializer — iter writes through slot

  dest.addParLe CoroforS, info
  var callCur = forStmt.iter
  if callCur.exprKind == HderefX:
    inc callCur
  dest.addParLe(callCur.cursorTagId, callCur.info)
  callCur = sub(callCur) # drained below; the close is synthesized
  while callCur.hasMore:
    dest.takeTree callCur
  dest.copyIntoKind HaddrX, info:
    dest.addSymUse forLoopVarSym, info
  dest.addParRi() # close iter call

  var prefix = createTokenBuf(16)
  if forVars.len > 1:
    for i, fv in forVars.pairs:
      let local = asLocal(fv)
      prefix.addParLe fv.stmtKind, fv.info
      prefix.addSymDef local.name.symId, fv.info
      prefix.addDotToken() # exported
      prefix.addDotToken() # pragmas
      prefix.copyTree local.typ
      prefix.copyIntoKind TupatX, info:
        prefix.addSymUse forLoopVarSym, info
        prefix.addIntLit i, info
      prefix.addParRi() # close decl
  emitForBody(e, dest, forStmt.body, initTable[SymId, SymId](), prefix)
  dest.addParRi() # close corofor

proc inlineIteratorFir(e: var EContext; dest: var TokenBuf; forStmt: ForStmt) =
  var iter = forStmt.iter
  if iter.exprKind == HderefX:
    inc iter
  assert iter.exprKind in CallKinds
  inc iter
  var iterDecl = default(Cursor)
  var iterSym = SymId(0)
  if iter.kind == Symbol:
    iterSym = iter.symId
    let res = tryLoadSym(iterSym)
    if res.status == LacksNothing and res.decl.stmtKind == IteratorS:
      iterDecl = res.decl
  if cursorIsNil(iterDecl):
    emitCoroForFir(e, dest, forStmt)
    return
  let routine = asRoutine(iterDecl, SkipInclBody)
  if hasPragma(routine.pragmas, ClosureP) or hasPragma(routine.pragmas, PassiveP):
    emitCoroForFir(e, dest, forStmt)
    return
  var w = createTokenBuf(64)
  w.addParLe StmtsS, forStmt.iter.info
  var params = routine.params
  params = sub(params) # (params; peek only, never left
  inc iter # name
  var relationsMap = initTable[SymId, SymId]()
  var paramCount = 0
  while params.hasMore:
    let param = asLocal(params)
    var typ = param.typ
    let name = param.name
    let newName = pool.symId("`lf." & $e.instId)
    inc e.instId
    createDecl(e, w, newName, typ, iter, name.info,
               if constructsValue(iter): VarS else: CursorS, needsAddr=false)
    relationsMap[name.symId] = newName
    inc paramCount
    skip params
  var body = routine.body
  replaceSymbol(e, w, body, relationsMap)
  w.addParRi()

  # Every module is published lowered, so an iterator body arrives in the
  # shape this pass wants, whichever module declared it.
  var lowered = ensureMove w
  # Fresh names for what the body declares. Not for the parameter
  # declarations: they are named already, and their values are the caller's
  # arguments, whose symbols a lowering run of its own may well reuse.
  var freshBuf = createTokenBuf(lowered.len)
  var lc = beginRead(lowered)
  freshBuf.addParLe(lc.cursorTagId, lc.info)
  lc.into:
    for i in 0 ..< paramCount:
      freshBuf.takeTree lc
    var fresh = initTable[SymId, SymId]()
    var probe = lc
    while probe.hasMore:
      collectDefs(e, probe, fresh)
      skip probe
    while lc.hasMore:
      copyFreshened(freshBuf, lc, fresh)
  freshBuf.addParRi()
  var inner = createTokenBuf(freshBuf.len)
  var fc = beginRead(freshBuf)
  transformStmt(e, inner, fc) # the iterator's own `for`s
  var ic = beginRead(inner)
  ic.into: # the wrapper's `stmts`: its children go straight into the scope
    while ic.hasMore:
      inlineIteratorBodyFir(e, dest, ic, forStmt, routine.retType)

proc transformForFir(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  ## The whole expansion is one `scope`: the iterator's parameters and locals
  ## die at its end, and the `(lab exit)` the lowering put after the `for`
  ## stays where every `break` expects it.
  let forStmt = asForStmt(c)
  dest.addParLe ScopeS, c.info
  inlineIteratorFir(e, dest, forStmt)
  dest.addParRi()
  skip c

proc transformStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  case c.kind
  of DotToken:
    dest.addSubtree c
    inc c
  of TagLit:
    case c.stmtKind
    of StmtsS:
      takeInto dest, c:
        while c.hasMore:
          transformStmt(e, dest, c)
    of ForS:
      transformForFir(e, dest, c)
    of IteratorS:
      let routine = asRoutine(c, SkipExclBody)
      let iterSym = routine.name.symId
      let isClosureIter = hasPragma(routine.pragmas, ClosureP) or
                          hasPragma(routine.pragmas, PassiveP)
      let isGeneric = routine.typevars.substructureKind == TypevarsU
      if isClosureIter and not isGeneric:
        # Inject `result: T` + rewrite `(yld v)` so destroyer/duplifier see
        # a typed asgn and inject =destroy/=copy hooks. cps.nim then lifts
        # `result` to `*env.result.0`. Generic closure-iter templates pass
        # through unchanged — only concrete instances need this rewrite.
        # `.passive` iters share the same lowering as `.closure` iters here.
        rewriteClosureIter(e, dest, c, routine.retType)
      elif isClosureIter:
        # Generic template: pass through verbatim; cps.nim also leaves it
        # alone so the dangling generic decl never references a coro frame
        # type that nobody defines.
        dest.takeTree c
      elif isLocalDecl(iterSym):
        var buf = createTokenBuf()
        takeTree(buf, c)
        publish iterSym, buf
      else:
        skip(c, SkipFull)
    of TemplateS:
      dest.takeTree c
    of FuncS, ProcS, ConverterS, MethodS:
      takeInto dest, c:
        takeTree(dest, c) # name
        takeTree(dest, c) # exported
        takeTree(dest, c) # pattern
        let isGeneric = c.substructureKind == TypevarsU
        for i in 3..<BodyPos:
          takeTree(dest, c)
        let oldTmpId = e.tmpId
        e.tmpId = 0
        if isGeneric:
          takeTree(dest, c)
        else:
          transformStmt(e, dest, c)
        e.tmpId = oldTmpId
    of VarS, LetS, CursorS, PatternvarS, ResultS:
      # We transform `var x {.cursor.} = y` into `cursor x = y` here because
      # this is the first step of the backend pipeline.
      let before = dest.len
      var hasCursorPragma = false
      takeInto dest, c:
        for i in 0..<LocalValuePos:
          if i == LocalPragmasPos:
            if hasPragma(c, CursorP):
              hasCursorPragma = true
          takeTree(dest, c)
        transformStmt(e, dest, c)
      if hasCursorPragma:
        # `setTag` keeps an already-sealed jump intact; `parLeToken` would
        # reset it
        setTagAt(dest, before, cast[TagId](CursorS))
    of GvarS, GletS, TvarS, TletS, ConstS:
      takeInto dest, c:
        for i in 0..<LocalValuePos:
          takeTree(dest, c)
        transformStmt(e, dest, c)
    of TypeS:
      # Type bodies contain field decls and field-level pragmas (e.g. the
      # `.cursor` annotation on a field). Their tags overlap with
      # statement tags but their layout is not statement-shaped, so
      # descending into them with `transformStmt` mis-parses them.
      # Type bodies don't need iterator transformations.
      dest.takeTree c
    of CallS, CmdS, MacroS, EmitS, AsgnS, ScopeS, IfS, WhileS, BreakS,
        ContinueS, BlockS,
        WhenS, CaseS, RetS, YldS, PragmasS, PragmaxS, InclS,
        ExclS, IncludeS, ImportS, ImportasS, FromimportS,
        ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
        TryS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
        InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS,
        UsingS, AsmS, DeferS, CoroforS, LabS, JmpS, NoStmt:
      takeInto dest, c:
        while c.hasMore:
          transformStmt(e, dest, c)
  else:
    takeTree(dest, c)

proc elimForLoops*(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  transformStmt(e, dest, c)

import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import hexer_context
import ".." / nimony / [nimony_model, programs, decls, typenav]
import duplifier


proc hasContinueStmt(c: Cursor): bool =
  var c = c
  var nested = 0
  result = false
  while true:
    case c.kind
    of EofToken:
      break
    of ParLe:
      case c.stmtKind
      of ContinueS:
        result = true
        break
      of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS,
          GletS, TletS, LetS, CursorS, PatternvarS, ProcS, FuncS,
          IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
          BlockS, EmitS, AsgnS, ScopeS, IfS, WhenS, BreakS, ForS,
          WhileS, CoroforS, CaseS, RetS, YldS, StmtsS, PragmasS,
          PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
          FromimportS, ImportexceptS, ExportS, ExportexceptS,
          CommentS, DiscardS, TryS, RaiseS, UnpackdeclS, AssumeS,
          AssertS, CallstrlitS, InfixS, PrefixS, HcallS, StaticstmtS,
          BindS, MixinS, UsingS, AsmS, DeferS, NoStmt:
        inc nested
        inc c
    of ParRi:
      dec nested
      inc c
    else:
      inc c

    if nested == 0:
      break

proc createDecl(e: var EContext; dest: var TokenBuf; destSym: SymId;
        typ: var Cursor; value: var Cursor;
        info: PackedLineInfo; kind: StmtKind; needsAddr: bool) =
  assert typ.hasMore
  dest.addParLe kind, info
  dest.add symdefToken(destSym, info)
  dest.addDotToken()
  dest.addDotToken()
  takeTree(dest, typ)
  if needsAddr:
    dest.copyIntoKind HaddrX, info:
      takeTree(dest, value)
  else:
    takeTree(dest, value)
  dest.addParRi()

proc createTupleAccess(left: TokenBuf; i: int; info: PackedLineInfo): TokenBuf =
  result = createTokenBuf()
  result.add parLeToken(TupatX, info)
  result.add left
  result.addIntLit(i, info)
  result.addParRi()

proc getForVars(e: var EContext, forVars: Cursor): seq[Cursor] =
  result = @[]
  var forVars = forVars
  if forVars.substructureKind notin {UnpackflatU, UnpacktupU}:
    error e, "`unpackflat` or `unpacktup` expected, but got: ", forVars
  inc forVars # unpackflat/unpacktup
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
    inc c
  else:
    var typ = local.typ
    createDecl(e, dest, destSym, typ, c, info, VarS, needsAddr=false)

proc unpackTupleAccess(e: var EContext; dest: var TokenBuf; forVar: Cursor; left: TokenBuf; i: int; info: PackedLineInfo; typ: Cursor; needsAddr: bool) =
  assert typ.hasMore
  let local = asLocal(forVar)
  let symId = local.name.symId
  var tupBuf = createTupleAccess(left, i, info)
  var tup = beginRead(tupBuf)
  var localTyp = local.typ
  createDecl(e, dest, symId, localTyp, tup, info, LetS, needsAddr)
  endRead(tupBuf)

proc startTupleAccess(s: SymId; info: PackedLineInfo; needsDeref: bool): TokenBuf =
  result = createTokenBuf()
  if needsDeref:
    result.copyIntoKind HderefX, info:
      result.add symToken(s, info)
  else:
    result.add symToken(s, info)

proc createYieldMapping(e: var EContext; dest: var TokenBuf; c: var Cursor, vars: Cursor, yieldType: Cursor): Table[SymId, SymId] =
  result = initTable[SymId, SymId]()

  let forVars = getForVars(e, vars)

  if forVars.len == 1:
    connectSingleExprToLoopVar(e, dest, c, forVars[0], result)
  else:
    if c.kind == ParLe and c.exprKind == TupX:
      inc c
      var i = 0
      while c.hasMore:
        connectSingleExprToLoopVar(e, dest, c, forVars[i], result)
        inc i
      skipParRi(e, c)
    else:
      let tmpId: SymId
      let info: PackedLineInfo
      var typ = yieldType.skipModifier()
      let needsDeref = yieldType.typeKind in {LentT, MutT}
      assert typ.typeKind == TupleT
      if c.kind == Symbol:
        tmpId = c.symId
        info = c.info
        inc c
      else:
        tmpId = pool.syms.getOrIncl("`ii." & $e.getTmpId)
        info = c.info
        var typCur = yieldType
        createDecl(e, dest, tmpId, typCur, c, info, LetS, needsAddr=false)

      inc typ # skips tuple
      for i in 0..<forVars.len:
        let isKvU = typ.substructureKind == KvU
        if isKvU:
          inc typ # skip tag
          skip typ # skip name

        if forVars[i].substructureKind in {UnpacktupU, UnpackflatU}:
          var counter = 0
          var unpackCursor = forVars[i]
          inc unpackCursor
          var left = startTupleAccess(tmpId, info, needsDeref)
          # The yielded element may itself be wrapped in `var`/`lent`/etc.
          # (e.g. `pairs(seq[T])` yields `(int, var T)`). Peel any modifier
          # before walking into the inner tuple, and remember to close it at
          # the end so the outer `typ` cursor advances past the whole element.
          # Modifier-wrapped element types lower to a pointer, so the access
          # path needs an `hderef` around `tmp[i]` before indexing into the
          # inner tuple.
          let hasModifier = typ.kind == ParLe and typ.typeKind in TypeModifiers
          var leftTupleAccess = createTupleAccess(left, i, info)
          if hasModifier:
            var deref = createTokenBuf()
            deref.copyIntoKind HderefX, info:
              deref.add leftTupleAccess
            leftTupleAccess = deref
            inc typ
          assert typ.typeKind == TupleT
          inc typ
          # When we deref'd a `var`/`lent`/... element above, the resulting
          # tuple-field accesses are by-value but the for-vars are still typed
          # as `var T`/`lent T`/... — sem propagates the outer modifier to
          # every unpacked sub-var. Pass `needsAddr` through so each `let sym
          # = (tupat ...)` is wrapped in `(haddr ...)`.
          let innerNeedsAddr = needsDeref or hasModifier
          while unpackCursor.hasMore:
            unpackTupleAccess(e, dest, unpackCursor, leftTupleAccess, counter, info, typ, innerNeedsAddr)
            inc counter
            skip unpackCursor
            skip typ
          skipParRi(typ)
          if hasModifier:
            skipParRi(typ)
        else:
          var left = startTupleAccess(tmpId, info, needsDeref)
          unpackTupleAccess(e, dest, forVars[i], left, i, info, typ, needsDeref)
          skip typ

        if isKvU:
          skipParRi(typ)

proc transformBreakStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  dest.add c
  inc c
  if c.kind == DotToken and e.breaks.len > 0 and e.breaks[^1] != SymId(0):
    let lab = e.breaks[^1]
    dest.add symToken(lab, c.info)
  else:
    assert c.kind in {DotToken, Symbol}
    dest.add c
  inc c
  takeParRi dest, c

proc transformContinueStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  if e.continues.len > 0 and e.continues[^1] != SymId(0):
    dest.add tagToken("break", c.info)
    inc c
    let lab = e.continues[^1]
    dest.add symToken(lab, c.info)
  else:
    dest.add c
    inc c
    dest.addDotToken()
  inc c # dotToken
  takeParRi dest, c

proc transformForStmt(e: var EContext; dest: var TokenBuf; c: var Cursor)
proc transformStmt(e: var EContext; dest: var TokenBuf; c: var Cursor)

proc copyWithMapping(dest: var TokenBuf; c: var Cursor; mapping: Table[SymId, SymId]) =
  ## Copy one subtree from `c` into `dest`, applying `mapping` to every
  ## Symbol reference. Symbol DEFINITIONS (SymbolDef) and structure are
  ## preserved verbatim — no fresh names, no recursive transformation.
  ## Used by `inlineLoopBody` to buffer a for-stmt before handing it to
  ## `transformForStmt`, so that nested for-stmts inside are inlined fresh
  ## (with distinct labels) on every yield expansion of the outer iterator.
  case c.kind
  of ParLe:
    var depth = 0
    while true:
      case c.kind
      of ParLe:
        dest.add c
        inc c
        inc depth
      of ParRi:
        dest.add c
        inc c
        dec depth
        if depth == 0: break
      of Symbol:
        let s = c.symId
        if mapping.hasKey(s):
          dest.add symToken(mapping.getOrQuit(s), c.info)
        else:
          dest.add c
        inc c
      of EofToken:
        bug "unexpected EOF in copyWithMapping"
      else:
        dest.add c
        inc c
  of Symbol:
    let s = c.symId
    if mapping.hasKey(s):
      dest.add symToken(mapping.getOrQuit(s), c.info)
    else:
      dest.add c
    inc c
  else:
    dest.add c
    inc c

proc inlineLoopBody(e: var EContext; dest: var TokenBuf; c: var Cursor; mapping: var Table[SymId, SymId]; fromForloop = false) =
  case c.kind
  of Symbol:
    let s = c.symId
    if mapping.hasKey(s):
      dest.add symToken(mapping.getOrQuit(s), c.info)
    else:
      dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of BreakS:
      transformBreakStmt(e, dest, c)
    of ContinueS:
      transformContinueStmt(e, dest, c)
    of ForS:
      # Buffer the for-stmt with the outer iterator's symbol substitutions
      # applied, but DO NOT pre-inline nested for-stmts and DO NOT generate
      # fresh local names: those decisions happen per-yield-expansion when
      # `transformForStmt` below drives `inlineIterator`. Pre-inlining here
      # would bake nested labels and locals into the buffer, and a multi-
      # yield outer iterator would then emit the same labels/locals from
      # each yield expansion (duplicate `forStmtLabel.N` in C output).
      var forStmtBuf = createTokenBuf()
      swap dest, forStmtBuf
      copyWithMapping(dest, c, mapping)
      swap dest, forStmtBuf
      var forCursor = beginRead(forStmtBuf)
      transformForStmt(e, dest, forCursor)
      endRead(forStmtBuf)
    of WhileS:
      dest.add c
      inc c
      inlineLoopBody(e, dest, c, mapping)
      e.breaks.add SymId(0)
      e.continues.add SymId(0)
      inlineLoopBody(e, dest, c, mapping)
      takeParRi(dest, c)
      discard e.breaks.pop()
      discard e.continues.pop()
    of BlockS:
      dest.takeToken(c)
      if c.kind == SymbolDef:
        e.breaks.add c.symId
      else:
        e.breaks.add SymId(0)
      dest.takeToken(c)
      inlineLoopBody(e, dest, c, mapping)
      discard e.breaks.pop
      takeParRi(dest, c)
    of StmtsS:
      if fromForloop:
        c.into:
          while c.hasMore:
            inlineLoopBody(e, dest, c, mapping)
      else:
        dest.add c
        c.into:
          while c.hasMore:
            inlineLoopBody(e, dest, c, mapping)
        dest.addParRi()
    of VarS, LetS, CursorS, PatternvarS, ResultS:
      dest.add c
      inc c
      let oldName = c.symId
      let freshLocal = pool.syms.getOrIncl("`ii." & $e.getTmpId)
      mapping[oldName] = freshLocal
      dest.add symdefToken(freshLocal, c.info) # name

      inc c
      # export marker:
      dest.takeTree c
      # pragmas:
      dest.takeTree c
      # type:
      dest.takeTree c
      # value:
      inlineLoopBody(e, dest, c, mapping)
      dest.takeParRi(c)
    of CallS, CmdS, GvarS, TvarS, ConstS, GletS, TletS, ProcS,
        FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS,
        TypeS, EmitS, AsgnS, ScopeS, IfS, WhenS, CaseS, RetS,
        YldS, PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS,
        ImportasS, FromimportS, ImportexceptS, ExportS,
        ExportexceptS, CommentS, DiscardS, TryS, RaiseS,
        UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS,
        PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, CoroforS, NoStmt:
      if c.substructureKind == KvU:
        # In KvU: first element is field name, don't substitute it
        dest.add c
        inc c
        dest.takeTree c
        while c.hasMore:
          inlineLoopBody(e, dest, c, mapping)
        takeParRi(dest, c)
      elif c.exprKind in {DotX, DdotX}:
        dest.add c
        inc c
        inlineLoopBody(e, dest, c, mapping)
        while c.hasMore:
          dest.takeTree c
        takeParRi(dest, c)
      else:
        dest.add c
        inc c
        e.loop(dest, c):
          inlineLoopBody(e, dest, c, mapping)
  else:
    takeTree(dest, c)

proc inlineIteratorBody(e: var EContext; dest: var TokenBuf;
      c: var Cursor; forStmt: ForStmt; yieldType: Cursor) =
  case c.kind
  of ParLe:
    case c.stmtKind
    of StmtsS:
      dest.add c
      c.into:
        while c.hasMore:
          inlineIteratorBody(e, dest, c, forStmt, yieldType)
      dest.addParRi()
    of YldS:
      dest.add tagToken($BlockS, c.info)
      dest.addDotToken()
      dest.add tagToken("stmts", c.info)

      let loopBodyHasContinueStmt = hasContinueStmt(forStmt.body)
      if loopBodyHasContinueStmt:
        let lab = pool.syms.getOrIncl("continueLabel." & $getTmpId(e))
        dest.add tagToken($BlockS, c.info)
        dest.add symdefToken(lab, c.info)
        dest.add tagToken("stmts", c.info)
        e.continues.add lab

      inc c # skips yield
      var mapping = createYieldMapping(e, dest, c, forStmt.vars, yieldType)
      var body = forStmt.body
      inlineLoopBody(e, dest, body, mapping, true)

      if loopBodyHasContinueStmt:
        discard e.continues.pop()
        dest.addParRi() # stmts
        dest.addParRi()

      dest.addParRi()
      dest.addParRi()
      skipParRi(e, c)
    of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS, GletS,
        TletS, LetS, CursorS, PatternvarS, ProcS, FuncS, IteratorS,
        ConverterS, MethodS, MacroS, TemplateS, TypeS, BlockS,
        EmitS, AsgnS, ScopeS, IfS, WhenS, BreakS, ContinueS, ForS,
        WhileS, CoroforS, CaseS, RetS, PragmasS, PragmaxS, InclS,
        ExclS, IncludeS, ImportS, ImportasS, FromimportS,
        ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
        TryS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
        InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, NoStmt:
      dest.add c
      inc c
      e.loop(dest, c):
        inlineIteratorBody(e, dest, c, forStmt, yieldType)
  else:
    takeTree(dest, c)

proc replaceSymbol(e: var EContext; dest: var TokenBuf; c: var Cursor; relations: var Table[SymId, SymId]) =
  case c.kind
  of DotToken:
    dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of VarS, LetS, CursorS, PatternvarS:
      dest.add c
      inc c
      let oldName = c.symId
      let newName = pool.syms.getOrIncl("`lf." & $e.instId)
      inc e.instId
      relations[oldName] = newName
      dest.add symdefToken(newName, c.info)
      inc c
      e.loop(dest, c):
        replaceSymbol(e, dest, c, relations)
    of CallS, CmdS, GvarS, TvarS, ConstS, ResultS, GletS, TletS,
        ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
        TemplateS, TypeS, BlockS, EmitS, AsgnS, ScopeS, IfS,
        WhenS, BreakS, ContinueS, ForS, WhileS, CoroforS, CaseS,
        RetS, YldS, StmtsS, PragmasS, PragmaxS, InclS, ExclS,
        IncludeS, ImportS, ImportasS, FromimportS, ImportexceptS,
        ExportS, ExportexceptS, CommentS, DiscardS, TryS, RaiseS,
        UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS,
        PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, NoStmt:
      if c.substructureKind == KvU:
        # In KvU: first element is field name, don't substitute it
        dest.add c
        inc c
        dest.takeTree c
        while c.hasMore:
          replaceSymbol(e, dest, c, relations)
        takeParRi(dest, c)
      elif c.exprKind in {DotX, DdotX}:
        dest.add c
        inc c
        replaceSymbol(e, dest, c, relations)
        while c.hasMore:
          dest.takeTree c
        takeParRi(dest, c)
      else:
        dest.add c
        inc c
        e.loop(dest, c):
          replaceSymbol(e, dest, c, relations)
  of Symbol:
    let s = c.symId
    if relations.hasKey(s):
      dest.add symToken(relations.getOrQuit(s), c.info)
    else:
      dest.add c
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
  of ParLe:
    let sk = c.stmtKind
    if sk == YldS:
      let info = c.info
      let head = c.load()
      inc c # past yld tag
      if c.kind == DotToken:
        # bare yield (void return) — leave as-is
        dest.add head
        dest.takeToken c # the dot token
        dest.takeParRi c
      else:
        # (yld v) ⇒ (asgn resultSym v) ; (yld .)
        dest.copyIntoKind AsgnS, info:
          dest.addSymUse resultSym, info
          dest.takeTree c # v
        dest.add head
        dest.addDotToken()
        dest.takeParRi c # close original yld
    elif sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                TemplateS, TypeS}:
      dest.takeTree c
    else:
      dest.takeToken c
      while c.hasMore:
        rewriteYieldsAndCopy(e, dest, c, resultSym)
      dest.takeParRi c
  else:
    dest.takeToken c

proc rewriteClosureIter(e: var EContext; dest: var TokenBuf;
                        c: var Cursor; retType: Cursor) =
  ## Inject `(result :synth . . T .)` at the head of a `.closure` iterator's
  ## body and rewrite every `(yld v)` to `(asgn synth v); (yld .)`. With this,
  ## destroyer sees a typed asgn to a `result` local and injects the proper
  ## `=destroy old; =copy/move new` hooks. cps.nim's existing ResultS handling
  ## then lifts `synth` to `(deref env.result.0)`.
  let synthResultSym = pool.syms.getOrIncl(
    "`coroResult." & $getTmpId(e) & "." & e.main)

  dest.takeToken c # IteratorS tag
  for _ in 0..<BodyPos:
    dest.takeTree c

  # Now at body. Either DotToken (forward decl) or a (stmts ...).
  if c.kind == DotToken:
    dest.takeToken c
    dest.takeParRi c
    return
  if c.stmtKind != StmtsS:
    dest.takeTree c
    dest.takeParRi c
    return

  dest.takeToken c # body's StmtsS opening

  let info = c.info
  dest.copyIntoKind ResultS, info:
    dest.addSymDef synthResultSym, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.copyTree retType
    dest.addDotToken() # value

  while c.hasMore:
    rewriteYieldsAndCopy(e, dest, c, synthResultSym)

  dest.takeParRi c # close body stmts
  dest.takeParRi c # close iter decl

proc emitCoroFor(e: var EContext; dest: var TokenBuf; forStmt: ForStmt) =
  ## Lower `for x in closureIter(args): body` into a `(corofor ...)` tag.
  ## cps.nim later expands the corofor into the real trampoline; this proc
  ## emits a pure-structural shape with no CPS-runtime symbols.
  ##
  ## Output (placed inside transformForStmt's existing outer block):
  ##   (var :forLoopVar T .)                                     <-- sibling
  ##   (corofor
  ##     <iter-call verbatim>
  ##     (block :coroInner.N (stmts <user-body>)))
  ##
  ## The for-loop var lives at the outer block's scope, NOT inside corofor's
  ## body. This is essential for correct hook semantics: with the iter's
  ## yield-write injecting `=destroy old; =copy new` on caller's slot,
  ## per-iteration destroyer-injected `=destroy(forLoopVar)` would
  ## double-destroy. By hoisting the decl, destroyer only injects `=destroy`
  ## once at end-of-outer-block, after the trampoline.
  ##
  ## The inner block exists so `continue` can rewrite to `break coroInner`,
  ## which cps.nim's expansion preserves so the trampoline can run
  ## `it = advance(it)` after the body block ends.
  var iterCur = forStmt.iter
  if iterCur.exprKind == HderefX:
    # Iter returns var/lent — peel the deref; the call expression itself is
    # what we lower. cps.nim's frame layout already wraps the synthesized
    # `result` field in `ptr`, so the slot type for a `var T`-returning iter
    # is `ptr (mut T)` and yield-writes lower to address-stores via existing
    # sem/desugar machinery.
    inc iterCur
  if iterCur.exprKind notin CallKinds:
    error e, "closure iterator must be invoked directly in a for-loop, got: ",
      forStmt.iter
  let info = iterCur.info
  let forVars = getForVars(e, forStmt.vars)

  let innerLab = pool.syms.getOrIncl("`coroInner." & $getTmpId(e))
  e.continues.add innerLab

  # `forLoopVarSym` is the symbol whose `addr` is passed to the iter. For the
  # single-var case it's the user's for-var itself. For multi-var (tuple
  # unpacking) it's a synthesised hidden tuple-typed local; per-iteration the
  # body opens with `(let userVar Ti (tupat hidden i))` bindings so the
  # user-body sees its symbols pointing at fresh copies of the components.
  let forLoopVarSym: SymId
  if forVars.len == 1:
    # Hoist the for-loop var declaration to the outer block scope (sibling
    # of corofor) — see proc docstring.
    dest.copyTree forVars[0]
    forLoopVarSym = asLocal(forVars[0]).name.symId
  else:
    # Multi-var: allocate a hidden tuple var whose type is the iter's
    # yield-type. cps.nim's frame wraps it in `ptr`, so this is what the
    # iter writes through.
    var symProbe = iterCur
    inc symProbe # past Call tag
    if symProbe.kind != Symbol:
      error e, "closure iterator call must target a symbol, got: ", iterCur
    let iterSym = symProbe.symId
    let res = tryLoadSym(iterSym)
    if res.status != LacksNothing:
      error e, "could not load closure-iter sym: " & pool.syms[iterSym]
    let routine = asRoutine(res.decl, SkipInclBody)
    var retType = routine.retType
    if retType.typeKind in {MutT, LentT}:
      inc retType
    forLoopVarSym = pool.syms.getOrIncl("`coroTup." & $getTmpId(e))
    dest.addParLe LetS, info
    dest.addSymDef forLoopVarSym, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.copyTree retType
    dest.addDotToken() # no initializer — iter writes through slot
    dest.addParRi() # close let

  dest.add tagToken("corofor", info)
  # Emit the iter call verbatim, but append `(haddr forLoopVarSym)` as a
  # trailing arg so cps.nim can recover the result-slot pointer without
  # having to peel the var-decl. Intermediate passes (xelim/destroyer/
  # duplifier) treat the call as opaque, so the extra "arg" rides along
  # without disturbing them.
  var callCur = forStmt.iter
  if callCur.exprKind == HderefX:
    inc callCur # peel hderef for var/lent-returning iters
  dest.takeToken callCur # (call tag
  dest.takeTree callCur # iter sym
  while callCur.hasMore:
    dest.takeTree callCur
  dest.copyIntoKind HaddrX, info:
    dest.addSymUse forLoopVarSym, info
  dest.addParRi() # close iter call

  # body: (block :coroInner.N (stmts [unpack-binds] <user-body>))
  dest.add tagToken($BlockS, info)
  dest.add symdefToken(innerLab, info)
  dest.add tagToken("stmts", info)

  if forVars.len > 1:
    # For each user for-var: (let :userSym T (tupat hidden i)). Using a fresh
    # let-binding per iteration mirrors what sem emitted (the user-vars are
    # lets) and gives the user-body the components by-value-from-the-tuple.
    for i, fv in forVars.pairs:
      let local = asLocal(fv)
      let userSym = local.name.symId
      var typ = local.typ
      dest.addParLe fv.stmtKind, fv.info
      dest.addSymDef userSym, fv.info
      dest.addDotToken() # exported
      dest.addDotToken() # pragmas
      dest.copyTree typ
      dest.copyIntoKind TupatX, info:
        dest.addSymUse forLoopVarSym, info
        dest.addIntLit i, info
      dest.addParRi() # close decl

  var bodyCur = forStmt.body
  if bodyCur.stmtKind == StmtsS:
    inc bodyCur
    while bodyCur.hasMore:
      transformStmt(e, dest, bodyCur)
  else:
    transformStmt(e, dest, bodyCur)

  dest.addParRi() # close inner block body stmts
  dest.addParRi() # close inner block

  dest.addParRi() # close corofor

  discard e.continues.pop()

proc inlineIterator(e: var EContext; dest: var TokenBuf; forStmt: ForStmt) =
  var iter = forStmt.iter
  if iter.exprKind == HderefX:
    # iterators return var/lent
    inc iter
  assert iter.exprKind in CallKinds
  inc iter
  let iterSym = iter.symId
  let res = tryLoadSym(iterSym)
  if res.status == LacksNothing:
    let routine = asRoutine(res.decl, SkipInclBody)
    if hasPragma(routine.pragmas, ClosureP):
      emitCoroFor(e, dest, forStmt)
      return
    var params = routine.params
    inc params # (params
    inc iter # name
    var relationsMap = initTable[SymId, SymId]()
    while params.hasMore:
      let param = asLocal(params)
      var typ = param.typ
      let name = param.name
      let symId = name.symId

      let newName = pool.syms.getOrIncl("`lf." & $e.instId)
      inc e.instId
      createDecl(e, dest, newName, typ, iter, name.info, if constructsValue(iter): VarS else: CursorS, needsAddr=false)
      relationsMap[symId] = newName

      skip params

    var preBodyBuf = createTokenBuf()
    var bodyBuf = createTokenBuf()
    var preBody = routine.body
    swap(dest, preBodyBuf)
    replaceSymbol(e, dest, preBody, relationsMap)
    swap(dest, preBodyBuf)

    swap(dest, bodyBuf)
    var body = cursorAt(preBodyBuf, 0)
    transformStmt(e, dest, body)
    endRead(preBodyBuf)
    swap(dest, bodyBuf)

    var transformedBody = beginRead(bodyBuf)
    inlineIteratorBody(e, dest, transformedBody, forStmt, routine.retType)
    endRead(bodyBuf)
  else:
    # No global iter decl by this name — sem must have accepted the call
    # because the target is a local of `itertype` (a first-class iter
    # value, `let g: iterator(...)`). Route through emitCoroFor; cps's
    # `trCoroFor` then expands the trampoline using the local as the
    # already-typed function pointer (its typeCache, primed by earlier
    # passes, distinguishes iter-decl vs iter-value targets).
    emitCoroFor(e, dest, forStmt)

proc transformForStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  #[ Transforming a `for` statement is quite involved. We have:

  - The iterator call.
  - The iterator body.
  - The for loop variables.
  - The for loop body.

  We traverse the iterator's body. For every `yield` we copy/inline the for loop body.
  The body can contain `break`/`continue`, these must refer to an outer `block` that we
  generate. This is required because the iterator might not even contain a loop or a nested
  loop structure and yet a `break` means to leave the iterator's body, not what is inside.

  The for loop variable `i` gets replaced by the `i-th` yield subexpression. Local variables
  of the iterator body need to be duplicated. Params of iter are bound to the args of `itercall`.

  Both iter params and for loop variables can be accessed multiple times and thus need
  protection against multi-evaluation.

  Local vars of iter can be bound directly to for loop variables and that is preferable
  for debugging::

    yield x  --> establish connection to loop variable `i`

  An example::

    iterator countup(a, b: int): int =
      var i = 0
      while i <= b:
        yield i     # establish as the for loop variable
        inc i

    for x in countup(1, sideEffect()):
      loopBodyStart()
      use x
      if condA:
        break
      elif condB:
        continue
      use x


  Is translated into::

    block forStmtLabel:
      let a = 1
      let b = sideEffect()
      var x = 0
      while x <= b:
        block inner:
          loopBodyStart()
          use x
          if condA:
            break forStmtLabel
          elif condB:
            break inner
          use x
          inc x
  ]#
  let forStmt = asForStmt(c)

  let lab = pool.syms.getOrIncl("forStmtLabel." & $getTmpId(e))
  dest.add tagToken($BlockS, c.info)
  dest.add symdefToken(lab, c.info)
  dest.add tagToken("stmts", c.info)

  e.breaks.add lab

  inlineIterator(e, dest, forStmt)

  discard e.breaks.pop()

  dest.addParRi() # stmts
  dest.addParRi() # block

  skip c

proc transformLoopBody(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  let loopBodyHasContinueStmt = hasContinueStmt(c)
  if loopBodyHasContinueStmt:
    let lab = pool.syms.getOrIncl("continueLabel." & $getTmpId(e))
    dest.add tagToken($BlockS, c.info)
    dest.add symdefToken(lab, c.info)
    dest.add tagToken("stmts", c.info)
    e.continues.add lab

  transformStmt(e, dest, c)

  if loopBodyHasContinueStmt:
    discard e.continues.pop()
    dest.addParRi() # stmts
    dest.addParRi() # block

proc transformWhileStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  let lab = pool.syms.getOrIncl("whileStmtLabel." & $getTmpId(e))
  dest.add tagToken($BlockS, c.info)
  dest.add symdefToken(lab, c.info)
  dest.add tagToken("stmts", c.info)

  e.breaks.add lab
  dest.add c
  inc c

  transformStmt(e, dest, c) # condition
  transformLoopBody(e, dest, c)
  takeParRi(dest, c)

  discard e.breaks.pop()

  dest.addParRi() # stmts
  dest.addParRi() # block

proc transformStmt(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  case c.kind
  of DotToken:
    dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of StmtsS:
      dest.add c
      inc c
      while c.kind notin {EofToken, ParRi}:
        transformStmt(e, dest, c)
      takeParRi dest, c
    of ForS:
      transformForStmt(e, dest, c)
    of IteratorS:
      let routine = asRoutine(c, SkipExclBody)
      let iterSym = routine.name.symId
      let isClosureIter = hasPragma(routine.pragmas, ClosureP)
      let isGeneric = routine.typevars.substructureKind == TypevarsU
      if isClosureIter and not isGeneric:
        # Inject `result: T` + rewrite `(yld v)` so destroyer/duplifier see
        # a typed asgn and inject =destroy/=copy hooks. cps.nim then lifts
        # `result` to `*env.result.0`. Generic closure-iter templates pass
        # through unchanged — only concrete instances need this rewrite.
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
        skip(c)
    of TemplateS:
      dest.takeTree c
    of FuncS, ProcS, ConverterS, MethodS:
      dest.add c
      inc c
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
      takeParRi(dest, c)
    of VarS, LetS, CursorS, PatternvarS, ResultS:
      # We transform `var x {.cursor.} = y` into `cursor x = y` here because
      # this is the first step of the backend pipeline.
      let before = dest.len
      dest.add c
      inc c
      var hasCursorPragma = false
      for i in 0..<LocalValuePos:
        if i == LocalPragmasPos:
          if hasPragma(c, CursorP):
            hasCursorPragma = true
        takeTree(dest, c)
      transformStmt(e, dest, c)
      takeParRi(dest, c)
      if hasCursorPragma:
        dest[before] = parLeToken(CursorS, dest[before].info)
    of GvarS, GletS, TvarS, TletS, ConstS:
      dest.add c
      inc c
      for i in 0..<LocalValuePos:
        takeTree(dest, c)
      transformStmt(e, dest, c)
      takeParRi(dest, c)
    of WhileS:
      transformWhileStmt(e, dest, c)
    of BreakS:
      transformBreakStmt(e, dest, c)
    of ContinueS:
      transformContinueStmt(e, dest, c)
    of BlockS:
      dest.takeToken(c)
      if c.kind == SymbolDef:
        e.breaks.add c.symId
        dest.takeToken(c)
      else:
        let info = c.info
        skip c
        let s = pool.syms.getOrIncl("`lab." & $getTmpId(e))
        dest.add symdefToken(s, info)
        e.breaks.add s
      transformStmt(e, dest, c)
      discard e.breaks.pop
      takeParRi(dest, c)
    of TypeS:
      # Type bodies contain field decls and field-level pragmas (e.g. the
      # `.cursor` annotation on a field). Their tags overlap with
      # statement tags but their layout is not statement-shaped, so
      # descending into them with `transformStmt` mis-parses them.
      # Type bodies don't need iterator transformations.
      dest.takeTree c
    of CallS, CmdS, MacroS, EmitS, AsgnS, ScopeS, IfS,
        WhenS, CaseS, RetS, YldS, PragmasS, PragmaxS, InclS,
        ExclS, IncludeS, ImportS, ImportasS, FromimportS,
        ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
        TryS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
        InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS,
        UsingS, AsmS, DeferS, CoroforS, NoStmt:
      dest.add c
      inc c
      e.loop(dest, c):
        transformStmt(e, dest, c)
  else:
    takeTree(dest, c)

proc elimForLoops*(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  transformStmt(e, dest, c)

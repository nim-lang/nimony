import hexer_context
include nifprelude

import std / [assertions, tables]

import ".." / nimony / [nimony_model, programs, decls]
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
          WhileS, CaseS, RetS, YldS, StmtsS, PragmasS, PragmaxS,
          InclS, ExclS, IncludeS, ImportS, ImportasS, FromimportS,
          ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
          TryS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
          InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS,
          UsingS, AsmS, DeferS, NoStmt:
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
  assert typ.kind != ParRi
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
  while forVars.kind != ParRi:
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
  assert typ.kind != ParRi
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
      while c.kind != ParRi:
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
          let leftTupleAccess = createTupleAccess(left, i, info)
          assert typ.typeKind == TupleT
          inc typ
          while unpackCursor.kind != ParRi:
            unpackTupleAccess(e, dest, unpackCursor, leftTupleAccess, counter, info, typ, needsDeref)
            inc counter
            skip unpackCursor
            skip typ
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

proc inlineLoopBody(e: var EContext; dest: var TokenBuf; c: var Cursor; mapping: var Table[SymId, SymId]; fromForloop = false) =
  case c.kind
  of Symbol:
    let s = c.symId
    if mapping.hasKey(s):
      dest.add symToken(mapping[s], c.info)
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
      var forStmtBuf = createTokenBuf()
      swap dest, forStmtBuf
      dest.add c
      inc c
      e.breaks.add SymId(0)
      e.continues.add SymId(0)
      e.loop(dest, c):
        inlineLoopBody(e, dest, c, mapping)
      swap dest, forStmtBuf
      discard e.breaks.pop()
      discard e.continues.pop()
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
        inc c
        while c.kind != ParRi:
          inlineLoopBody(e, dest, c, mapping)
        skipParRi(e, c)
      else:
        dest.add c
        inc c
        while c.kind != ParRi:
          inlineLoopBody(e, dest, c, mapping)
        takeParRi(dest, c)
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
        AsmS, DeferS, NoStmt:
      if c.substructureKind == KvU:
        # In KvU: first element is field name, don't substitute it
        dest.add c
        inc c
        dest.takeTree c
        while c.kind != ParRi:
          inlineLoopBody(e, dest, c, mapping)
        takeParRi(dest, c)
      elif c.exprKind in {DotX, DdotX}:
        dest.add c
        inc c
        inlineLoopBody(e, dest, c, mapping)
        while c.kind != ParRi:
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
      inc c
      while c.kind != ParRi:
        inlineIteratorBody(e, dest, c, forStmt, yieldType)
      takeParRi dest, c
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
        WhileS, CaseS, RetS, PragmasS, PragmaxS, InclS, ExclS,
        IncludeS, ImportS, ImportasS, FromimportS, ImportexceptS,
        ExportS, ExportexceptS, CommentS, DiscardS, TryS, RaiseS,
        UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS,
        PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
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
        WhenS, BreakS, ContinueS, ForS, WhileS, CaseS, RetS,
        YldS, StmtsS, PragmasS, PragmaxS, InclS, ExclS, IncludeS,
        ImportS, ImportasS, FromimportS, ImportexceptS, ExportS,
        ExportexceptS, CommentS, DiscardS, TryS, RaiseS,
        UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS,
        PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, NoStmt:
      if c.substructureKind == KvU:
        # In KvU: first element is field name, don't substitute it
        dest.add c
        inc c
        dest.takeTree c
        while c.kind != ParRi:
          replaceSymbol(e, dest, c, relations)
        takeParRi(dest, c)
      elif c.exprKind in {DotX, DdotX}:
        dest.add c
        inc c
        replaceSymbol(e, dest, c, relations)
        while c.kind != ParRi:
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
      dest.add symToken(relations[s], c.info)
    else:
      dest.add c
    inc c
  else:
    takeTree(dest, c)

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
    var params = routine.params
    inc params # (params
    inc iter # name
    var relationsMap = initTable[SymId, SymId]()
    while params.kind != ParRi:
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
    error e, "could not find symbol: " & pool.syms[iterSym]

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

proc trProc(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  dest.takeToken c
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
      var iter = c
      if procHasPragma(iter, ClosureP):
        trProc(e, dest, c)
      else:
        inc iter
        if isLocalDecl(iter.symId):
          var buf = createTokenBuf()
          takeTree(buf, c)
          publish iter.symId, buf
        else:
          skip(c)
    of TemplateS:
      dest.takeTree c
    of FuncS, ProcS, ConverterS, MethodS:
      trProc(e, dest, c)
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
    of CallS, CmdS, MacroS, TypeS, EmitS, AsgnS, ScopeS, IfS,
        WhenS, CaseS, RetS, YldS, PragmasS, PragmaxS, InclS,
        ExclS, IncludeS, ImportS, ImportasS, FromimportS,
        ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
        TryS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
        InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS,
        UsingS, AsmS, DeferS, NoStmt:
      dest.add c
      inc c
      e.loop(dest, c):
        transformStmt(e, dest, c)
  else:
    takeTree(dest, c)

proc elimForLoops*(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  transformStmt(e, dest, c)

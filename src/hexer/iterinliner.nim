import hexer_context
include nifprelude

import std / [assertions, tables]

import ".." / nimony / [nimony_model, programs, decls]


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
      else:
        inc nested
        inc c
    of ParRi:
      dec nested
      inc c
    else:
      inc c

    if nested == 0:
      break

proc createDecl(e: var EContext; destSym: SymId;
        typ: var Cursor; value: var Cursor;
        info: PackedLineInfo; kind: string) =
  e.dest.add tagToken(kind, info)
  e.dest.add symdefToken(destSym, info)
  e.dest.addDotToken()
  e.dest.addDotToken()
  takeTree(e, typ)
  takeTree(e, value)
  e.dest.addParRi()

when false:
  proc createAsgn(e: var EContext; destSym: SymId;
        value: var Cursor; info: PackedLineInfo) =
    e.dest.add tagToken("asgn", info)
    e.dest.add symToken(destSym, info)
    takeTree(e, value)
    e.dest.addParRi()

proc createTupleAccess(lvalue: SymId; i: int; info: PackedLineInfo): TokenBuf =
  result = createTokenBuf()
  result.add parLeToken(TupatX, info)
  result.add symToken(lvalue, info)
  result.addIntLit(i, info)
  result.addParRi()

proc getForVars(e: var EContext, forVars: Cursor): seq[Local] =
  result = @[]
  var forVars = forVars
  if forVars.substructureKind notin {UnpackflatU, UnpacktupU}:
    error e, "`unpackflat` or `unpacktup` expected, but got: ", forVars
  inc forVars # unpackflat
  while forVars.kind != ParRi:
    let local = asLocal(forVars)
    result.add local
    skip forVars

proc connectSingleExprToLoopVar(e: var EContext; c: var Cursor;
          local: Local; res: var Table[SymId, SymId]) =
  let destSym = local.name.symId
  let info = local.name.info
  case c.kind
  of Symbol:
    let val = c.symId
    res[destSym] = val
    inc c
  else:
    var typ = local.typ
    createDecl(e, destSym, typ, c, info, "var")

proc createYieldMapping(e: var EContext; c: var Cursor, vars: Cursor, yieldType: Cursor): Table[SymId, SymId] =
  result = initTable[SymId, SymId]()

  let forVars = getForVars(e, vars)

  if forVars.len == 1:
    connectSingleExprToLoopVar(e, c, forVars[0], result)
  else:
    if c.kind == ParLe and c.exprKind == TupX:
      inc c
      var i = 0
      while c.kind != ParRi:
        connectSingleExprToLoopVar(e, c, forVars[i], result)
        inc i
      skipParRi(e, c)
    else:
      let tmpId: SymId
      let info: PackedLineInfo
      var typ = yieldType
      if c.kind == Symbol:
        tmpId = c.symId
        info = c.info
        inc c
      else:
        tmpId = pool.syms.getOrIncl(":tmp.l." & $e.getTmpId)
        info = c.info
        var typ = yieldType
        createDecl(e, tmpId, typ, c, info, "let")

      inc typ # skips tuple
      for i in 0..<forVars.len:
        let symId = forvars[i].name.symId
        var tupBuf = createTupleAccess(tmpId, i, info)
        var tup = beginRead(tupBuf)
        var fieldTyp = getTupleFieldType(typ)
        skip typ
        createDecl(e, symId, fieldTyp, tup, info, "let")

proc transformBreakStmt(e: var EContext; c: var Cursor) =
  e.dest.add c
  inc c
  if c.kind == DotToken and e.breaks.len > 0 and e.breaks[^1] != SymId(0):
    let lab = e.breaks[^1]
    e.dest.add symToken(lab, c.info)
  else:
    assert c.kind in {DotToken, Symbol}
    e.dest.add c
  inc c
  takeParRi e, c

proc transformContinueStmt(e: var EContext; c: var Cursor) =
  e.dest.add tagToken("break", c.info)
  inc c
  if e.continues.len > 0 and e.continues[^1] != SymId(0):
    let lab = e.continues[^1]
    e.dest.add symToken(lab, c.info)
  else:
    e.dest.addDotToken()
  inc c # dotToken
  takeParRi e, c

proc transformForStmt(e: var EContext; c: var Cursor)
proc transformStmt(e: var EContext; c: var Cursor)

proc inlineLoopBody(e: var EContext; c: var Cursor; mapping: var Table[SymId, SymId]; fromForloop = false) =
  case c.kind
  of Symbol:
    let s = c.symId
    if mapping.hasKey(s):
      e.dest.add symToken(mapping[s], c.info)
    else:
      e.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of BreakS:
      transformBreakStmt(e, c)
    of ContinueS:
      transformContinueStmt(e, c)
    of ForS:
      var forStmtBuf = createTokenBuf()
      swap e.dest, forStmtBuf
      e.dest.add c
      inc c
      e.breaks.add SymId(0)
      e.continues.add SymId(0)
      e.loop(c):
        inlineLoopBody(e, c, mapping)
      swap e.dest, forStmtBuf
      discard e.breaks.pop()
      discard e.continues.pop()
      var forCursor = beginRead(forStmtBuf)
      transformForStmt(e, forCursor)
    of WhileS:
      e.dest.add c
      inc c
      inlineLoopBody(e, c, mapping)
      e.breaks.add SymId(0)
      e.continues.add SymId(0)
      inlineLoopBody(e, c, mapping)
      takeParRi(e, c)
      discard e.breaks.pop()
      discard e.continues.pop()
    of BlockS:
      e.dest.add c
      inc c
      e.breaks.add SymId(0)
      inlineLoopBody(e, c, mapping)
      discard e.breaks.pop
    of StmtsS:
      if fromForloop:
        inc c
        while c.kind != ParRi:
          inlineLoopBody(e, c, mapping)
        skipParRi(e, c)
      else:
        e.dest.add c
        inc c
        while c.kind != ParRi:
          inlineLoopBody(e, c, mapping)
        takeParRi(e, c)
    of VarS, LetS, CursorS, ResultS:
      e.dest.add c
      inc c
      let oldName = c.symId
      let freshLocal = pool.syms.getOrIncl(":tmp.v." & $e.getTmpId)
      mapping[oldName] = freshLocal
      e.dest.add symdefToken(freshLocal, c.info) # name

      inc c
      # export marker:
      e.dest.takeTree c
      # pragmas:
      e.dest.takeTree c
      # type:
      e.dest.takeTree c
      # value:
      inlineLoopBody(e, c, mapping)
      e.dest.takeParRi(c)
    else:
      e.dest.add c
      inc c
      e.loop c:
        inlineLoopBody(e, c, mapping)
  else:
    takeTree(e, c)

proc inlineIteratorBody(e: var EContext;
      c: var Cursor; forStmt: ForStmt; yieldType: Cursor) =
  case c.kind
  of ParLe:
    case c.stmtKind
    of StmtsS:
      e.dest.add c
      inc c
      while c.kind != ParRi:
        inlineIteratorBody(e, c, forStmt, yieldType)
      takeParRi e, c
    of YldS:
      e.dest.add tagToken($BlockS, c.info)
      e.dest.addDotToken()
      e.dest.add tagToken("stmts", c.info)

      let loopBodyHasContinueStmt = hasContinueStmt(forStmt.body)
      if loopBodyHasContinueStmt:
        let lab = pool.syms.getOrIncl("continueLabel." & $getTmpId(e))
        e.dest.add tagToken($BlockS, c.info)
        e.dest.add symdefToken(lab, c.info)
        e.dest.add tagToken("stmts", c.info)
        e.continues.add lab

      inc c # skips yield
      var mapping = createYieldMapping(e, c, forStmt.vars, yieldType)
      var body = forStmt.body
      inlineLoopBody(e, body, mapping, true)

      if loopBodyHasContinueStmt:
        discard e.continues.pop()
        e.dest.addParRi() # stmts
        e.dest.addParRi()

      e.dest.addParRi()
      e.dest.addParRi()
      skipParRi(e, c)
    else:
      e.dest.add c
      inc c
      e.loop c:
        inlineIteratorBody(e, c, forStmt, yieldType)
  else:
    takeTree(e, c)

proc replaceSymbol(e: var EContext; c: var Cursor; relations: var Table[SymId, SymId]) =
  case c.kind
  of DotToken:
    e.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of VarS, LetS, CursorS:
      e.dest.add c
      inc c
      let oldName = c.symId
      let newName = pool.syms.getOrIncl(pool.syms[oldName] & ".lf." & $e.instId)
      relations[oldName] = newName
      e.dest.add symdefToken(newName, c.info)
      inc c
      e.loop(c):
        replaceSymbol(e, c, relations)
    else:
      e.dest.add c
      inc c
      e.loop(c):
        replaceSymbol(e, c, relations)
  of Symbol:
    let s = c.symId
    if relations.hasKey(s):
      e.dest.add symToken(relations[s], c.info)
    else:
      e.dest.add c
    inc c
  else:
    takeTree(e, c)

proc inlineIterator(e: var EContext; forStmt: ForStmt) =
  var iter = forStmt.iter
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

      let newName = pool.syms.getOrIncl(pool.syms[symId] & ".lf." & $e.instId)
      createDecl(e, newName, typ, iter, name.info, "var")
      relationsMap[symId] = newName

      skip params

    var preBodyBuf = createTokenBuf()
    var bodyBuf = createTokenBuf()
    var preBody = routine.body
    swap(e.dest, preBodyBuf)
    replaceSymbol(e, preBody, relationsMap)
    swap(e.dest, preBodyBuf)

    swap(e.dest, bodyBuf)
    var body = cursorAt(preBodyBuf, 0)
    transformStmt(e, body)
    swap(e.dest, bodyBuf)

    var transformedBody = beginRead(bodyBuf)
    inlineIteratorBody(e, transformedBody, forStmt, routine.retType)

  else:
    error e, "could not find symbol: " & pool.syms[iterSym]

proc transformForStmt(e: var EContext; c: var Cursor) =
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
  e.dest.add tagToken($BlockS, c.info)
  e.dest.add symdefToken(lab, c.info)
  e.dest.add tagToken("stmts", c.info)

  e.breaks.add lab

  inc e.instId
  inlineIterator(e, forStmt)

  e.dest.addParRi()
  e.dest.addParRi()

  skip c


proc transformStmt(e: var EContext; c: var Cursor) =
  case c.kind
  of DotToken:
    e.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of StmtsS:
      e.dest.add c
      inc c
      while c.kind notin {EofToken, ParRi}:
        transformStmt(e, c)
      takeParRi e, c
    of ForS:
      transformForStmt(e, c)
    of IteratorS:
      skip(c)
    of TemplateS:
      e.dest.takeTree c
    of FuncS, ProcS, ConverterS, MethodS:
      e.dest.add c
      inc c
      for i in 0..<BodyPos:
        takeTree(e, c)
      let oldTmpId = e.tmpId
      e.tmpId = 0
      transformStmt(e, c)
      e.tmpId = oldTmpId
      takeParRi(e, c)
    of VarS, LetS, CursorS, ResultS:
      e.dest.add c
      inc c
      for i in 0..<LocalValuePos:
        takeTree(e, c)
      transformStmt(e, c)
      takeParRi(e, c)
    else:
      e.dest.add c
      inc c
      e.loop(c):
        transformStmt(e, c)
  else:
    takeTree(e, c)

proc elimForLoops*(e: var EContext; c: var Cursor) =
  transformStmt(e, c)

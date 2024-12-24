import std / [hashes, os, tables, sets, syncio, times]

include nifprelude

import nifindexes
import ".." / nimony / [nimony_model]

type
  Context* = object
    dest*: TokenBuf
    breaks: seq[SymId] # how to translate `break`
    continues: seq[SymId] # how to translate `continue`
    forVars: seq[SymId]
    iterCalls: (SymId, seq[SymId], Cursor)
    moduleSyms: HashSet[SymId] # thisModule: ModuleId
    loopBody: Cursor

    declaredSyms: HashSet[SymId]
    requires: seq[SymId]

    iterdecls: Table[SymId, TokenBuf]


proc demand(e: var Context; s: SymId) =
  if not e.declaredSyms.contains(s):
    e.requires.add s

proc offer(e: var Context; s: SymId) =
  e.declaredSyms.incl s

proc transformStmt*(e: var Context; c: var Cursor)

proc error(e: var Context; msg: string; c: Cursor) {.noreturn.}=
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(c)
  when defined(debug):
    echo getStackTrace()
  quit 1

proc tagToken(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(ParLe, pool.tags.getOrIncl(tag), info)

template loop(e: var Context; c: var Cursor; body: untyped) =
  while true:
    case c.kind
    of ParRi:
      e.dest.add c
      inc c
      break
    of EofToken:
      # error e, "expected ')', but EOF reached"
      quit "expected ')', but EOF reached"
      # break
    else: discard
    body

proc extract(e: var Context; c: var Cursor) =
  var nested = 0
  while true:
    case c.kind
    of EofToken: break
    of ParLe:
      inc nested
      e.dest.add c
    of ParRi:
      dec nested
      e.dest.add c
      if nested == 0:
        inc c
        break
    of SymbolDef:
      e.dest.add c
      e.offer c.symId
    of Symbol:
      e.dest.add c
      e.demand c.symId
    else:
      e.dest.add c
    inc c

    if nested == 0:
      break

proc want(e: var Context; c: var Cursor) =
  e.dest.add c
  inc c

proc want(e: var Context; c: var Cursor; kind: TokenKind) =
  if c.kind != kind:
    error e, "expected '" & $kind & "', but got: ", c
  e.dest.add c
  inc c

proc wantParRi(e: var Context; c: var Cursor) =
  if c.kind == ParRi:
    e.dest.add c
    inc c
  else:
    error e, "expected ')', but got: ", c

proc skipParRi(e: var Context; c: var Cursor) =
  if c.kind == ParRi:
    inc c
  else:
    error e, "expected ')', but got: ", c

proc expectSymdef(e: var Context; c: var Cursor) =
  if c.kind != SymbolDef:
    error e, "expected symbol definition, but got: ", c

proc getSymDef(e: var Context; c: var Cursor): SymId =
  expectSymdef(e, c)
  result = c.symId
  inc c

proc extractParam(e: var Context; c: var Cursor): SymId =
  if c.substructureKind != ParamS:
    error e, "expected (param) but got: ", c
  inc c
  result = getSymDef(e, c)
  var skipped = createTokenBuf()
  swap(e.dest, skipped)
  e.loop(c):
    extract(e, c)
  swap(e.dest, skipped)

proc getIteratorParams(e: var Context; c: var Cursor): seq[SymId] =
  result = @[]
  if c.kind == DotToken:
    inc c
  elif c.kind == ParLe and c.substructureKind == ParamsS:
    inc c
    loop e, c:
      result.add extractParam(e, c)

proc collectIterCalls(e: var Context; c: var Cursor): (SymId, seq[SymId], Cursor) =
  result = default((SymId, seq[SymId], Cursor))
  inc c # skips `call`
  result[0] = c.symId
  inc c
  e.demand result[0]
  result[1] = getIteratorParams(e, c)
  result[2] = c
  var skipped = createTokenBuf()
  swap(e.dest, skipped)
  e.loop(c):
    extract(e, c)
  swap(e.dest, skipped)

proc collectVars*(e: var Context; c: var Cursor): seq[SymId] =
  result = @[]
  if c.kind == ParLe and pool.tags[c.tag] == $UnpackFlatS:
    inc c # skips `unpackflat`
    while c.kind == ParLe:
      inc c # skips `let`
      result.add getSymDef(e, c)
      extract(e, c)
      extract(e, c)
      extract(e, c)
      extract(e, c)
      skipParRi(e, c)
    skipParRi(e, c)

proc toSymId(s: string): SymId =
  result = pool.syms.getOrIncl(s)

proc createDecl(e: var Context; c: var Cursor; destSym: SymId; value: TokenBuf; kind: string) =
  # TODO: info
  e.dest.add tagToken(kind, c.info)
  e.dest.add toToken(SymbolDef, destSym, c.info)
  e.dest.addDotToken()
  e.dest.addDotToken()
  e.dest.addDotToken() # TODO: what's the type?
  e.dest.addParRi()

proc createAsgn(e: var Context; c: var Cursor; destSym: SymId; value: TokenBuf) =
  # TODO: info
  e.dest.add tagToken("asgn", c.info)
  e.dest.add toToken(SymbolDef, destSym, c.info)
  e.dest.add value
  e.dest.addParRi()

proc createTupleAccess(e: var Context; c: var Cursor; lvalue: SymId; i: uint32): TokenBuf =
  # TODO: info
  result = createTokenBuf()
  result.add tagToken("at", c.info)
  result.add toToken(Symbol, lvalue, c.info)
  result.add toToken(IntLit, i, c.info)
  result.addParRi()

proc transformLocal(e: var Context; c: var Cursor) =
  want e, c, SymbolDef #
  e.loop(c):
    extract(e, c)

proc transformBreakStmt(e: var Context; c: var Cursor) =
  want e, c
  if c.kind == DotToken and e.breaks.len > 0 and e.breaks[^1] != SymId(0):
    let lab = e.breaks[^1]
    e.dest.add toToken(Symbol, lab, c.info)
  else:
    want e, c
  wantParRi e, c

proc transformContinueStmt(e: var Context; c: var Cursor) =
  want e, c
  if e.continues.len > 0 and e.continues[^1] != SymId(0):
    let lab = e.continues[^1]
    e.dest.add toToken(Symbol, lab, c.info)
  wantParRi e, c

proc pop(s: var seq[SymId]): SymId =
  result = s[^1]
  setLen(s, s.len-1)

proc inlineLoopBody(e: var Context; c: var Cursor; mapping: Table[SymId, SymId]) =
  case c.kind
  of Symbol:
    let s = c.symId
    if mapping.hasKey(s):
      e.dest.add toToken(Symbol, s, c.info)
    else:
      want e, c
      e.demand s
  of ParLe:
    case c.stmtKind
    of BreakS:
      transformBreakStmt(e, c)
    of ContinueS:
      transformContinueStmt(e, c)
    of ForS, WhileS:
      e.want c
      e.breaks.add SymId(0)
      e.continues.add SymId(0)
      inlineLoopBody(e, c, mapping)
      discard e.breaks.pop()
      discard e.continues.pop()
    of BlockS:
      e.want c
      e.breaks.add SymId(0)
      inlineLoopBody(e, c, mapping)
      discard e.breaks.pop
    of StmtsS:
      e.want c
      e.loop c:
        inlineLoopBody(e, c, mapping)
    else:
      extract(e, c)
  else:
    extract(e, c)

proc connectSingleExprToLoopVar(e: var Context; c: var Cursor;
                  destSym: SymId; res: var Table[SymId, SymId]) =
  case c.kind
  of Symbol:
    let val = c.symId
    res[destSym] = val
  else:
    if destSym in e.moduleSyms:
      var dest = createTokenBuf()
      swap(e.dest, dest)
      extract(e, c)
      swap(e.dest, dest)
      createAsgn(e, c, destSym, dest)
    else:
      var dest = createTokenBuf()
      swap(e.dest, dest)
      extract(e, c)
      swap(e.dest, dest)
      createDecl(e, c, destSym, dest, "var")
      e.moduleSyms.incl destSym

proc createYieldMapping(e: var Context; c: var Cursor): Table[SymId, SymId] =
  result = initTable[SymId, SymId]()
  if e.forVars.len == 1:
    let destSym = e.forVars[0]
    # c.init.add destSym
    connectSingleExprToLoopVar(e, c, destSym, result)
  else:
    if c.kind == ParLe and c.exprKind == TupleConstrX:
      var i = 0
      e.loop c:
        connectSingleExprToLoopVar(e, c,  e.forVars[i], result)
    else:
      var dest = createTokenBuf()
      swap(e.dest, dest)
      extract(e, c)
      swap(e.dest, dest)
      let yieldExpr = beginRead(dest)

      let tmpId: SymId
      if yieldExpr.kind == Symbol:
        tmpId = yieldExpr.symId
      else:
        tmpId = toSymId("tmp")
        createDecl(e, c, tmpId, dest, "let")

      for i in 0..<e.forVars.len:
        let symId = e.forvars[i]
        createDecl(e, c, symId, createTupleAccess(e, c, tmpId, uint32(i)), "let")

proc hasContinueStmt(c: var Cursor): bool =
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
        discard
    else:
      discard
    inc c

proc inlineIteratorBody(e: var Context; c: var Cursor) =
  case c.kind
  of ParLe:
    case c.stmtKind
    of StmtsS:
      e.want c
      e.loop c:
        inlineIteratorBody e, c
    of YieldS:
      let loopBodyHasContinueStmt = hasContinueStmt(e.loopBody)
      if loopBodyHasContinueStmt:
        let lab = toSymId("continueLabel")
        e.dest.add tagToken($BlockS, c.info)
        e.dest.add toToken(SymbolDef, lab, c.info)
        e.continues.add lab

      inc c # skips yield
      let mapping = createYieldMapping(e, c)
      inlineLoopBody(e, c, mapping)

      if loopBodyHasContinueStmt:
        discard e.continues.pop()
        e.dest.addParRi()
    else:
      extract(e, c)
  else:
    extract(e, c)

proc inlineIterator(e: var Context; c: var Cursor) =
  var (name, params, valueCursor) = e.iterCalls
  for i in 0..<params.len:
    var value = createTokenBuf()
    swap(e.dest, value)
    extract(e, valueCursor)
    swap(e.dest, value)
    createDecl(e, c, params[i], value, "let")

  var tok {.cursor.} = e.iterdecls[name]
  var body = beginRead(tok)
  inlineIteratorBody(e, body)

proc transformIterator(e: var Context; c: var Cursor) =
  discard

proc transformForStmt(e: var Context; c: var Cursor) =
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
          inc x
          if condA:
            break forStmtLabel
          elif condB:
            break inner
          use x
  ]#
  e.dest.add c
  inc c
  e.iterCalls = collectIterCalls(e, c)
  e.forVars = collectVars(e, c)

  e.loopBody = c
  var skipped = createTokenBuf()
  swap(e.dest, skipped)
  transformStmt(e, c)
  swap(e.dest, skipped)

  let lab = toSymId("forStmtLabel")
  e.dest.add tagToken($BlockS, c.info)
  e.dest.add toToken(SymbolDef, lab, c.info)
  e.dest.add tagToken("stmts", c.info)

  inlineIterator(e, c)

  e.dest.addParRi()
  e.dest.addParRi()

  # let r = beginRead(e.dest)
  # echo r

proc transformStmt(e: var Context; c: var Cursor) =
  case c.kind
  of DotToken:
    e.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of StmtsS:
      inc c
      while c.kind notin {EofToken, ParRi}:
        transformStmt(e, c)
      inc c # skipParRi
    of VarS, LetS, CursorS, ResultS:
      transformLocal(e, c)
    of ForS:
      transformForStmt(e, c)
    # of IterS:
    #   transformIterator(e, c)
    else:
      extract(e, c)
  else:
    extract(e, c)


type
  NifModule = ref object
    buf: TokenBuf
    stream: Stream
    index: NifIndex

proc newNifModule(infile: string): NifModule =
  result = NifModule(stream: nifstreams.open(infile))
  discard processDirectives(result.stream.r)

  result.buf = fromStream(result.stream)
  # fromStream only parses the topLevel 'stmts'
  let eof = next(result.stream)
  result.buf.add eof
  let indexName = infile.changeFileExt".idx.nif"
  if not fileExists(indexName) or getLastModificationTime(indexName) < getLastModificationTime(infile):
    createIndex infile
  result.index = readIndex(indexName)


proc transformCode*(infile: string) =
  var m = newNifModule(infile)
  var c = beginRead(m.buf)

  var e = Context(dest: createTokenBuf())

  transformStmt(e, c)

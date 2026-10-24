import plugins
import std/[syncio]

proc scalarOpName(op: string): string =
  result = op
  if result.len > 1 and result[result.len - 1] in {'.', '!'}:
    result = result[0 ..< result.len - 1]
  if result.len > 1 and result[0] == '!':
    result = result[1 ..< result.len]

proc isScalarBroadcast(op: string): bool =
  op.len > 1 and op[op.len - 1] == '!'

proc hasContainer(containers: seq[NifCursor], n: NifCursor): bool =
  for c in containers:
    if c.kind == n.kind:
      if c.kind == Symbol and c.symId == n.symId:
        return true
      elif c.kind == Ident and c.identText == n.identText:
        return true
  return false

proc collectContainers(n: NifCursor, containers: var seq[NifCursor]) =
  case n.kind
  of Symbol, Ident:
    if not hasContainer(containers, n):
      containers.add n
  of IntLit, UIntLit, FloatLit, StrLit, CharLit:
    discard
  of TagLit:
    let tag = n.tagText
    if tag == "infix":
      var c = firstChild(n)
      let opName = if c.kind == Symbol: c.symId.symBasename else: c.identText
      let isScalar = isScalarBroadcast(opName)
      skip c
      var left = c
      skip c
      var right = c
      if isScalar:
        collectContainers(left, containers)
      else:
        collectContainers(left, containers)
        collectContainers(right, containers)
    elif tag == "prefix":
      var c = firstChild(n)
      skip c # skip op
      while c.hasMore:
        collectContainers(c, containers)
        skip c
    elif tag == "par":
      var c = firstChild(n)
      while c.hasMore:
        collectContainers(c, containers)
        skip c
    elif tag == "call":
      var c = firstChild(n)
      if c.kind == Symbol and c.symId.symBasename == "map":
        skip c # skip map
        collectContainers(c, containers)
      else:
        skip c # skip callee
        while c.hasMore:
          collectContainers(c, containers)
          skip c
    elif tag == "dot" or tag == "at":
      if not hasContainer(containers, n):
        containers.add n
  else:
    discard

proc toScalarOp(result: var NifBuilder, n: NifCursor, idx: SymId, info: LineInfo) =
  case n.kind
  of Symbol, Ident:
    # Container element access: container[idx]
    result.openTree("at", info)
    result.addSubtree(n)
    result.addSymUse(idx, info)
    result.closeTree()
  of IntLit, UIntLit, FloatLit, StrLit, CharLit:
    result.addSubtree(n)
  of TagLit:
    let tag = n.tagText
    if tag == "infix":
      var c = firstChild(n)
      let opName = if c.kind == Symbol: c.symId.symBasename else: c.identText
      let scOp = scalarOpName(opName)
      skip c
      var left = c
      skip c
      var right = c
      result.openTree("infix", info)
      result.addIdent scOp
      if isScalarBroadcast(opName):
        toScalarOp(result, left, idx, info)
        result.addSubtree right
      else:
        toScalarOp(result, left, idx, info)
        toScalarOp(result, right, idx, info)
      result.closeTree()
    elif tag == "prefix":
      var c = firstChild(n)
      let opName = if c.kind == Symbol: c.symId.symBasename else: c.identText
      let scOp = scalarOpName(opName)
      skip c
      result.openTree("prefix", info)
      result.addIdent scOp
      toScalarOp(result, c, idx, info)
      result.closeTree()
    elif tag == "par":
      result.openTree("par", info)
      var inner = firstChild(n)
      toScalarOp(result, inner, idx, info)
      result.closeTree()
    elif tag == "call":
      var c = firstChild(n)
      if c.kind == Symbol and c.symId.symBasename == "map":
        # map(a, f) -> f(a[idx])
        skip c
        var cont = c
        skip c
        var fn = c
        result.openTree("call", info)
        result.addSubtree(fn)
        result.openTree("at", info)
        result.addSubtree(cont)
        result.addSymUse(idx, info)
        result.closeTree() # /at
        result.closeTree() # /call
      else:
        result.openTree("call", info)
        result.addSubtree(c) # callee
        skip c
        while c.hasMore:
          toScalarOp(result, c, idx, info)
          skip c
        result.closeTree()
    else:
      # e.g. dot or already indexed
      result.openTree("at", info)
      result.addSubtree(n)
      result.addSymUse(idx, info)
      result.closeTree()
  else:
    result.addSubtree(n)

proc transform(n: var NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var arg = callArgs(n)
  if not arg.hasMore:
    return errorTree("@. expects an expression", info)

  var containers: seq[NifCursor] = @[]
  collectContainers(arg, containers)
  if containers.len == 0:
    return errorTree("@. could not find a container in expression", info)

  let firstCont = containers[0]
  let temp = genSym()
  let idx = genSym()

  result.openTree("block")
  result.addDotToken()
  result.withTree StmtsS, info:
    # Check that all containers have the same length:
    for i in 1 ..< containers.len:
      result.openTree("if", info)
      result.openTree("elif", info)
      result.openTree("infix", info)
      result.addIdent "!="
      result.openTree("dot", info)
      result.addSubtree containers[i]
      result.addIdent "len"
      result.closeTree() # /dot
      result.openTree("dot", info)
      result.addSubtree firstCont
      result.addIdent "len"
      result.closeTree() # /dot
      result.closeTree() # /infix

      result.withTree StmtsS, info:
        result.openTree("call", info)
        result.addIdent "panic"
        result.addStrLit "broadcast: containers must have the same length\n"
        result.closeTree() # /call
      result.closeTree() # /elif
      result.closeTree() # /if

    # var temp = similar(firstCont)
    result.withTree VarS, info:
      result.addSymDef temp, info
      result.addEmptyNode3(info)
      result.withTree CallS, info:
        result.addIdent "similar"
        result.addSubtree firstCont

    # for idx in 0 ..< temp.len:
    result.openTree("for", info)
    result.openTree("infix", info)
    result.addIdent "..<"
    result.addIntLit 0
    result.openTree("dot", info)
    result.addSymUse temp, info
    result.addIdent "len"
    result.closeTree() # /dot
    result.closeTree() # /infix

    result.openTree("unpackflat", info)
    result.openTree("let", info)
    result.addSymDef idx, info
    result.addEmptyNode4(info)
    result.closeTree() # /let
    result.closeTree() # /unpackflat

    result.withTree StmtsS, info:
      # temp[idx] = scalarExpr
      result.withTree AsgnS, info:
        result.openTree("at", info)
        result.addSymUse temp, info
        result.addSymUse idx, info
        result.closeTree() # /at
        toScalarOp(result, arg, idx, info)
    result.closeTree() # /for

    # return temp
    result.addSymUse temp, info
  result.closeTree() # /block

var inp = loadPluginInput()
var c = inp
saveTree transform(c)
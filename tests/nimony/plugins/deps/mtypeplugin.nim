
import std / [os, strutils, tables]

include ".." / ".." / ".." / ".." / src / lib / nifprelude
import ".." / ".." / ".." / ".." / src / nimony / nimony_model
import nimonyplugins


template traverse*(n: var Node, body: untyped) =
  let n2 = n
  body
  n = n2

proc skip*(n: var Node, count: int) =
  for _ in 0..<count:
    skip n


var knownInstances: Table[SymId, SymId]  # name -> type
var knownTypes: seq[SymId]
var knownOnChanged: Table[SymId, SymId]  # type -> `onChanged` template sym


proc typesTr(n: Node) =
  var n = n
  inc n
  while n.kind != ParRi:
    knownTypes.add n.symId
    inc n
  inc n


proc trAsgn(n: var Node, o: var TokenBuf) =
  var
    fieldName = ""
    access = createTokenBuf(3)
    instance = Node()
    emitOnChanged = false
  traverse n:
    inc n
    if n.kind == ParLe and n.exprKind == DotX:
      traverse n:
        takeTree access, n
      inc n
      if n.kind == Symbol and n.symId in knownInstances and knownInstances[n.symId] in knownOnChanged:
        instance = n
        inc n
        if n.kind == Symbol:
          fieldName = pool.syms[n.symId]
          fieldName.delete fieldName.find('.')..fieldName.high
          emitOnChanged = true
  takeTree o, n
  if emitOnChanged:
    o.addParLe CallS
    o.addSymUse knownOnChanged[knownInstances[instance.symId]], n.info
    o.add instance
    o.add access
    o.addStrLit fieldName
    o.addParRi()


proc trGvar(n: var Node, o: var TokenBuf) =
  traverse n:
    inc n
    let nameSym = n.symId
    skip n, 3
    if n.kind == Symbol and n.symId in knownTypes:
      knownInstances[nameSym] = n.symId
  takeTree o, n


proc trTemplate(n: var Node, o: var TokenBuf) =
  traverse n:
    inc n
    let nameSym = n.symId
    let name = pool.syms[n.symId]
    if name.startsWith("onChanged"):
      skip n, 4
      inc n  # params
      inc n  # (first) param
      skip n, 3
      if n.kind == ParLe and n.typeKind == MutT:
        inc n
      if n.kind == Symbol:
        knownOnChanged[n.symId] = nameSym
  takeTree o, n


proc trAux(n: var Node, o: var TokenBuf) =
  case n.kind
  of ParLe:
    if n.stmtKind == AsgnS:
      trAsgn n, o
    elif n.stmtKind == GvarS:
      trGvar n, o
    elif n.stmtKind == TemplateS:
      trTemplate n, o
    else:
      o.add n
      inc n
      while n.kind != ParRi:
        trAux(n, o)
      o.addParRi()
      inc n
  else:
    takeTree o, n


proc tr(n: Node): TokenBuf =
  result = createTokenBuf()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.addParLe StmtsS, info
  while n.kind != ParRi:
    trAux(n, result)
  result.addParRi()


var inp = loadTree()
var inpTypes = loadTree(paramStr(3))

typesTr(beginRead inpTypes)
writeFile os.paramStr(2), toString tr(beginRead inp)

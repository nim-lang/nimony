
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


proc trAsgn(n: var Node, o: Tree) =
  var
    fieldName = ""
    access = Node()
    instance = Node()
    emitOnChanged = false
  traverse n:
    inc n
    if n.kind == ParLe and n.exprKind == DotX:
      traverse n:
        var accessTree = createTree()
        accessTree.takeTree(n)
        access = freeze(accessTree)
      inc n
      if n.kind == Symbol and n.symId in knownInstances and knownInstances[n.symId] in knownOnChanged:
        instance = n
        inc n
        if n.kind == Symbol:
          fieldName = pool.syms[n.symId]
          fieldName.delete fieldName.find('.')..fieldName.high
          emitOnChanged = true
  let info = n.info
  o.takeTree(n)
  if emitOnChanged:
    o.withTree CallS, info:
      o.addSymUse knownOnChanged[knownInstances[instance.symId]], info
      var instanceCopy = instance
      var accessCopy = access
      o.takeTree(instanceCopy)
      o.takeTree(accessCopy)
      o.addStrLit fieldName


proc trGvar(n: var Node, o: Tree) =
  traverse n:
    inc n
    let nameSym = n.symId
    skip n, 3
    if n.kind == Symbol and n.symId in knownTypes:
      knownInstances[nameSym] = n.symId
  o.takeTree(n)


proc trTemplate(n: var Node, o: Tree) =
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
  o.takeTree(n)


proc trAux(n: var Node, o: Tree) =
  case n.kind
  of ParLe:
    if n.stmtKind == AsgnS:
      trAsgn n, o
    elif n.stmtKind == GvarS:
      trGvar n, o
    elif n.stmtKind == TemplateS:
      trTemplate n, o
    else:
      let info = n.info
      let tag = n.tagId
      o.addParLe(tag, info)
      inc n
      while n.kind != ParRi:
        trAux(n, o)
      o.addParRi()
      inc n
  else:
    o.takeTree(n)


proc tr(n: Node): Tree =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    while n.kind != ParRi:
      trAux(n, result)


var inp = loadTree()
var inpTypes = loadTree(paramStr(3))

typesTr(inpTypes)
saveTree tr(inp), os.paramStr(2)

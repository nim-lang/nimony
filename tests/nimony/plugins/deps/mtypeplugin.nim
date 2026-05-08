
import std / [os, strutils, tables]

import nimonyplugins


template traverse*(n: var NifCursor, body: untyped) =
  let n2 = n
  body
  n = n2

proc skip*(n: var NifCursor, count: int) =
  for _ in 0..<count:
    skip n

var knownTypes: seq[SymId]
var knownInstances: Table[SymId, SymId]  # name -> type
var knownOnChanged: Table[SymId, SymId]  # type -> `onChanged` template sym


proc typesTr(n: NifCursor) =
  var n = n
  n.into:
    while n.hasMore:
      knownTypes.add n.symId
      skip n


proc trAsgn(n: var NifCursor, o: var NifBuilder) =
  var
    fieldName = ""
    access = NifCursor()
    instance = NifCursor()
    emitOnChanged = false
  traverse n:
    n.into:                              # descend past `(asgn`
      if n.kind == ParLe and n.exprKind == DotX:
        access = n                       # snapshot at the dot expression
        skip n                           # past dotX subtree
        if n.kind == Symbol and n.symId in knownInstances and knownInstances[n.symId] in knownOnChanged:
          instance = n
          skip n                         # past instance symbol (atom)
          if n.kind == Symbol:
            fieldName = n.symText
            fieldName.delete fieldName.find('.')..fieldName.high
            emitOnChanged = true
      while n.hasMore: skip n            # consume any leftover children
  let info = n.info
  o.takeTree(n)
  if emitOnChanged:
    o.withTree CallS, info:
      o.addSymUse knownOnChanged[knownInstances[instance.symId]], info
      o.addSubtree(instance)
      o.addSubtree(access)
      o.addStrLit fieldName


proc trGvar(n: var NifCursor, o: var NifBuilder) =
  traverse n:
    n.into:                              # descend past `(gvar`
      let nameSym = n.symId              # name SymbolDef is the first child (atom; reading does not advance)
      skip n, 3                          # name + export marker + pragmas
      if n.kind == Symbol and n.symId in knownTypes:
        knownInstances[nameSym] = n.symId
      while n.hasMore: skip n            # consume value (and anything else)
  o.takeTree(n)


proc trTemplate(n: var NifCursor, o: var NifBuilder) =
  traverse n:
    n.into:                              # descend past `(template`
      let nameSym = n.symId
      let name = n.symText
      if name.startsWith("onChanged"):
        skip n, 4                        # name + export marker + pragmas + typeParams
        n.into:                          # descend into `(params`
          n.into:                        # descend into the first `(param`
            skip n, 3                    # skip first three param children
            if n.kind == ParLe and n.typeKind == MutT:
              n.into:                    # descend into `(mut`
                if n.kind == Symbol:
                  knownOnChanged[n.symId] = nameSym
                while n.hasMore: skip n
            elif n.kind == Symbol:
              knownOnChanged[n.symId] = nameSym
            while n.hasMore: skip n      # rest of param children
          while n.hasMore: skip n        # other params
      while n.hasMore: skip n            # rest of template body
  o.takeTree(n)


proc trAux(n: var NifCursor, o: var NifBuilder) =
  case n.kind
  of ParLe:
    if n.stmtKind == AsgnS:
      trAsgn n, o
    elif n.stmtKind == GvarS:
      trGvar n, o
    elif n.stmtKind == TemplateS:
      trTemplate n, o
    else:
      o.copyInto(n):
        while n.hasMore:
          trAux(n, o)
  else:
    o.takeTree(n)


proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  result.withTree StmtsS, info:
    if n.stmtKind == StmtsS:
      var body = firstChild(n)
      while body.hasMore:
        trAux(body, result)
    else:
      var head = n
      trAux(head, result)


var inp = loadPluginInput()
var inpTypes = loadPluginInput(os.paramStr(3))

typesTr(inpTypes)
saveTree tr(inp), os.paramStr(2)


import std / [os, strutils]

include ".." / ".." / ".." / ".." / src / lib / nifprelude
import ".." / ".." / ".." / ".." / src / nimony / nimony_model


var knownInstances: seq[string]
var onChangedName: SymId


proc trAux(n: var Cursor, o: var TokenBuf) =
  case n.kind
  of ParLe:
    if n.stmtKind == AsgnS:
      var n2 = n
      takeTree o, n
      inc n2
      var access = createTokenBuf(3)
      if n2.kind == ParLe and n2.exprKind == DotX:
        var n3 = n2
        takeTree access, n3
        inc n2
        if n2.kind == Symbol and pool.syms[n2.symId] in knownInstances:
          let instance = n2
          inc n2
          if n2.kind == Symbol:
            var name = pool.syms[n2.symId]
            name.delete name.find('.')..name.high
            o.addParLe CallS
            o.addSymUse onChangedName, n.info
            o.add instance
            o.add access
            o.addStrLit name
            o.addParRi()
    elif n.stmtKind == GvarS:
      var n2 = n
      takeTree o, n
      inc n2
      knownInstances.add pool.syms[n2.symId]
    elif n.stmtKind == TemplateS:
      var n2 = n
      takeTree o, n
      inc n2
      let name = pool.syms[n2.symId]
      if name.startsWith("onChanged"):
        onChangedName = n2.symId
    else:
      o.add n
      inc n
      while n.kind != ParRi:
        trAux(n, o)
      o.addParRi()
      inc n
  else:
    takeTree o, n


proc tr(n: Cursor): TokenBuf =
  result = createTokenBuf()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.addParLe StmtsS, info
  while n.kind != ParRi:
    trAux(n, result)
  result.addParRi()

let input = os.paramStr(1)
let output = os.paramStr(2)
var inp = nifstreams.open(input)
var buf = fromStream(inp)

let outp = tr(beginRead buf)

writeFile output, toString(outp)

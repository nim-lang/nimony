import std / [tables, sets, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symparser

proc takeUnquoted*(c: var Cursor): StrId =
  var r = ""
  while true:
    case c.kind
    of ParLe:
      inc c
    of ParRi:
      inc c
      break
    of EofToken:
      r.add "<unexpected eof>"
      break
    of Ident, StringLit:
      r.add pool.strings[c.litId]
      inc c
    of IntLit:
      r.addInt pool.integers[c.intId]
      inc c
    of CharLit:
      let ch = char(c.uoperand)
      r.add ch
      inc c
    of UIntLit:
      r.add $pool.uintegers[c.uintId]
      inc c
    of FloatLit:
      r.addFloat pool.floats[c.floatId]
      inc c
    of UnknownToken, DotToken, Symbol, SymbolDef:
      r.add "<unexpected token>: " & $c.kind
      inc c
  assert r.len > 0
  result = getOrIncl(pool.strings, r)

proc takeIdent*(n: var Cursor): StrId =
  var nested = 0
  while exprKind(n) in {OchoiceX, CchoiceX}:
    inc nested
    inc n
  case n.kind
  of Ident:
    result = n.litId
    inc n
  of Symbol, SymbolDef:
    let sym = pool.syms[n.symId]
    var isGlobal = false
    result = pool.strings.getOrIncl(extractBasename(sym, isGlobal))
    inc n
  of ParLe:
    if exprKind(n) == QuotedX:
      result = takeUnquoted(n)
    else:
      result = StrId(0)
  else:
    result = StrId(0)
  while nested > 0:
    if n.kind == ParRi: dec nested
    inc n

proc getIdent*(n: Cursor): StrId =
  var n = n
  result = takeIdent(n)

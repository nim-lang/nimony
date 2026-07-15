import std / [tables, sets, syncio, formatfloat, assertions]
include ".." / lib / nifprelude
import nimony_model
import ".." / lib / symparser

proc takeUnquoted*(c: var Cursor): StrId =
  var r = ""
  var scopes: seq[CursorScope] = @[]
  while true:
    case c.kind
    of ParLe:
      scopes.add enterScope(c)
    of ParRi:
      # the first close ends the walk (quoted content is flat in practice)
      if scopes.len > 0:
        leaveScope(c, scopes.pop)
      else:
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
    if exprKind(n) in {OchoiceX, CchoiceX}:
      result = StrId(0)
      n.peekInto:
        if n.hasMore:
          result = takeIdent(n)
    elif exprKind(n) == QuotedX:
      result = takeUnquoted(n)
    else:
      result = StrId(0)
  else:
    result = StrId(0)

proc getIdent*(n: Cursor): StrId =
  var n = n
  result = takeIdent(n)

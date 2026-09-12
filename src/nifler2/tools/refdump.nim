## Dumps the token stream `compiler/lexer.nim` produces, in the canonical
## format `nimlexdump` also prints, so the two can be diffed. This is the
## reference half of `nimlexer`'s differential test.
##
##   nim c -r src/nifler2/tools/refdump.nim file.nim

import std / [os, syncio, strutils]
import compiler / [lexer, options, idents, llstream, pathutils, msgs]

proc esc(s: string): string =
  result = ""
  for c in s:
    case c
    of '\n': result.add "\\n"
    of '\r': result.add "\\r"
    of '\t': result.add "\\t"
    of '\\': result.add "\\\\"
    of ' ': result.add "\\s"
    else:
      if c < ' ' or c == '\x7F': result.add "\\" & $ord(c)
      else: result.add c

proc text(tok: Token): string =
  ## Identifiers go through `nimIdentNormalize` because Nim's identifier cache
  ## hands back whichever spelling was interned first: `Foo` and `foO` share
  ## one `PIdent`, so the original spelling is not recoverable here. Keywords
  ## print nothing at all -- the token kind already is the keyword.
  case tok.tokType
  of tkSymbol:
    nimIdentNormalize(if tok.ident != nil: tok.ident.s else: tok.literal)
  of tokKeywordLow..tokKeywordHigh:
    ""
  of tkOpr, tkColon, tkColonColon, tkEquals, tkDot, tkDotDot:
    if tok.ident != nil: tok.ident.s else: tok.literal
  of tkIntLit..tkCustomLit, tkComment:
    tok.literal
  else:
    ""

proc dump(file: string) =
  let conf = newConfigRef()
  conf.errorMax = high(int)
  let stream = llStreamOpen(AbsoluteFile file, fmRead)
  if stream == nil: quit "cannot open: " & file
  let fileIdx = fileInfoIdx(conf, AbsoluteFile file)
  var L: Lexer = default(Lexer)
  openLexer(L, fileIdx, stream, newIdentCache(), conf)
  var tok: Token = default(Token)
  while true:
    rawGetTok(L, tok)
    var sp = ""
    if tsLeading in tok.spacing: sp.add "L"
    if tsTrailing in tok.spacing: sp.add "T"
    if tsEof in tok.spacing: sp.add "E"
    if sp.len == 0: sp = "-"
    echo $tok.tokType, "\t", esc(text(tok)), "\t", tok.indent, "\t", sp
    if tok.tokType == tkEof: break
  closeLexer(L)

proc main =
  if paramCount() < 1: quit "usage: refdump file.nim"
  dump paramStr(1)

main()

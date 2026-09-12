## Dumps `nimlexer`'s token stream in the format `refdump` prints, so the two
## can be diffed. See `src/nifler2/tests/tlexdiff.sh`.
##
##   bin/nimony c src/nifler2/tools/nimlexdump.nim
##   <binary> file.nim

import std / [syncio, cmdline, assertions]
import ".." / nimlexer

proc esc(s: string): string =
  result = ""
  var i = 0
  while i < s.len:
    let c = s[i]
    if c == '\n': result.add "\\n"
    elif c == '\r': result.add "\\r"
    elif c == '\t': result.add "\\t"
    elif c == '\\': result.add "\\\\"
    elif c == ' ': result.add "\\s"
    elif c < ' ' or c == '\x7F':
      result.add '\\'
      result.add $ord(c)
    else: result.add c
    inc i

proc text(tok: Token): string =
  ## Mirrors `refdump.text`: identifiers normalized, keywords empty, and
  ## nothing for the punctuation whose spelling is its kind.
  if tok.kind == tkSymbol: nimIdentNormalize(tok.s)
  elif tok.kind >= KeywordLow and tok.kind <= KeywordHigh: ""
  elif tok.kind in {tkOpr, tkColon, tkColonColon, tkEquals, tkDot, tkDotDot}:
    tok.s
  elif tok.kind >= tkIntLit and tok.kind <= tkCustomLit: tok.s
  elif tok.kind == tkComment: tok.s
  else: ""

proc dump(file: string) {.raises.} =
  let src = readFile(file)
  for tok in tokens(src, file):
    var sp = ""
    if tsLeading in tok.spacing: sp.add "L"
    if tsTrailing in tok.spacing: sp.add "T"
    if tsEof in tok.spacing: sp.add "E"
    if sp.len == 0: sp = "-"
    echo $tok.kind & "\t" & esc(text(tok)) & "\t" & $tok.indent & "\t" & sp

proc main {.raises.} =
  if paramCount() < 1:
    quit "usage: nimlexdump file.nim"
  dump paramStr(1)

try:
  main()
except:
  quit "cannot read file"

## Where nifler2's time goes: read, lex, parse (lex included), write.
##   bin/nimony c -d:release src/nifler2/tools/phasebench.nim
##   phasebench file.nim [runs]

import std / [syncio, cmdline, monotimes, os, strutils]
import ".." / [nimlexer, parserrt, nimparser, niflerout]

proc ns(a, b: MonoTime): int64 = b.ticks - a.ticks

proc main {.raises.} =
  let inp = paramStr(1)
  let runs = if paramCount() >= 2: parseInt(paramStr(2)) else: 10
  let filename = absolutePath(inp)
  var best = [high(int64), high(int64), high(int64), high(int64)]
  var toks = 0
  for r in 0 ..< runs:
    let t0 = getMonoTime()
    let src = readFile(inp)
    let t1 = getMonoTime()
    var L = openLexer(src, filename)
    var tok = Token(kind: tkInvalid, s: "", indent: -1, spacing: {},
                    line: 0, col: 0, base: 10, suffixPos: -1)
    toks = 0
    while true:
      next L, tok
      inc toks
      if tok.kind == tkEof: break
    let t2 = getMonoTime()
    var p = openParser(src, filename)
    pModule p
    let t3 = getMonoTime()
    writeNifler p.dest, "/dev/shm/phasebench.nif", inp
    let t4 = getMonoTime()
    best[0] = min(best[0], ns(t0, t1))
    best[1] = min(best[1], ns(t1, t2))
    best[2] = min(best[2], ns(t2, t3))
    best[3] = min(best[3], ns(t3, t4))
  echo "tokens ", toks, ", bytes ", getFileSize(inp)
  echo "read  ", best[0] div 1000, " us"
  echo "lex   ", best[1] div 1000, " us"
  echo "parse ", best[2] div 1000, " us  (lex included)"
  echo "write ", best[3] div 1000, " us"

try:
  main()
except:
  quit "failed"

#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Nifler2 parses Nim code and writes NIF. Same job as `src/nifler`, without
## linking Nim 2's compiler: the lexer is `nimlexer` and the parser is
## generated from `nimgrammar.nim`.
##
##   bin/nimony c src/nifler2/nifler2.nim
##   nifler2 p file.nim [out.nif]

import std / [syncio, cmdline, assertions, os]
import nimparser, niflerout

const Usage = """nifler2 - Nim to NIF
Usage:
  nifler2 p|parse file.nim [out.nif]   parse one file, write NIF
  nifler2 t|tree file.nim              parse one file, print the tree
"""

proc parse(p: var Parser; inp: string) {.raises.} =
  ## `Parser` owns a `TokenBuf`, which is not copyable, so it is filled in
  ## place rather than returned.
  p = openParser(readFile(inp), inp)
  pModule p
  if p.tok.kind != tkEof:
    error p, "unexpected token"

proc report(p: Parser): bool =
  for e in p.lex.errors: echo e
  for e in p.errors: echo e
  p.lex.errors.len == 0 and p.errors.len == 0

proc withoutExt(s: string): string =
  var i = s.len - 1
  while i > 0 and s[i] != '.' and s[i] != '/': dec i
  if i > 0 and s[i] == '.': s.substr(0, i-1) else: s

proc main {.raises.} =
  if paramCount() < 2: quit Usage
  let cmd = paramStr(1)
  let inp = paramStr(2)
  case cmd
  of "p", "parse":
    var p = openParser("", inp)
    parse p, inp
    let ok = report(p)
    let outp = if paramCount() >= 3: paramStr(3) else: withoutExt(inp) & ".nif"
    # nimony runs nifler with `--portablePaths`: the file is written relative
    # to the current directory
    writeNifler(p.dest, outp, relativePath(absolutePath(inp), getCurrentDir(), '/'))
    if not ok: quit 1
  of "t", "tree":
    var p = openParser("", inp)
    parse p, inp
    echo toString(p.dest)
    if not report(p): quit 1
  else:
    quit Usage

try:
  main()
except:
  quit "nifler2: cannot read the input file"

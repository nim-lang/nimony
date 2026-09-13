#       Nifler2
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Broken Nim for `errsweep.sh`: writes mutants of valid files, each with one
## small edit -- a token deleted, duplicated or inserted, a line's indentation
## shifted, or a line joined with the next. Nim's own test suite has a few
## dozen syntax errors; this makes as many as are wanted, in code that looks
## like real code.
##
##   nim c -o:bin/errmutate src/nifler2/tools/errmutate.nim
##   bin/errmutate /tmp/mut 1 $(find lib/std -name '*.nim')
##   src/nifler2/tools/errsweep.sh /tmp/mut
##
## The seed makes a set reproducible; a second set with another seed and other
## sources is what shows whether a fix generalizes.

import std / [os, strutils, random, syncio]

const Inserts = [":", "=", ",", "(", ")", "[", "]", "{.", ".}", "if", "else",
  "of", "proc", "var", "*", ".", ";", "do", "elif", "when", "type", "`", "not",
  "x", "1"]

proc tokens(line: string): seq[(int, int)] =
  ## Rough token spans, comments excluded: enough to cut and paste.
  result = @[]
  var i = 0
  while i < line.len:
    let c = line[i]
    let start = i
    if c == '#': break
    elif c == '"':
      inc i
      while i < line.len and line[i] != '"':
        if line[i] == '\\': inc i
        inc i
      inc i
    elif c in IdentStartChars:
      while i < line.len and line[i] in IdentChars: inc i
    elif c in Digits:
      while i < line.len and line[i] in Digits: inc i
    elif c in {'=', ':', ',', ';', '(', ')', '[', ']', '{', '}', '.', '*',
               '+', '-', '<', '>', '/', '`'}:
      while i < line.len and line[i] in {'=', ':', ',', ';', '(', ')', '[',
          ']', '{', '}', '.', '*', '+', '-', '<', '>', '/', '`'}: inc i
    else:
      inc i
      continue
    result.add (start, min(i, line.len))

proc main =
  let args = commandLineParams()
  if args.len < 3: quit "usage: errmutate outdir seed file.nim..."
  let outdir = args[0]
  var r = initRand(parseInt(args[1]))
  createDir outdir
  var n = 0
  for f in args[2 .. ^1]:
    let lines = readFile(f).split('\n')
    if lines.len > 3000: continue
    var cand: seq[int] = @[]
    for i, l in lines:
      if l.strip.len > 0 and not l.strip.startsWith("#"): cand.add i
    if cand.len == 0: continue
    for k in 0 ..< 3:
      var ls = lines
      let i = cand[r.rand(cand.len - 1)]
      var l = ls[i]
      let toks = tokens(l)
      case r.rand(4)
      of 0:
        if toks.len > 0:
          let (a, b) = toks[r.rand(toks.len - 1)]
          l = l[0 ..< a] & l[b .. ^1]
      of 1:
        if toks.len > 0:
          let (a, b) = toks[r.rand(toks.len - 1)]
          l = l[0 ..< b] & " " & l[a ..< b] & l[b .. ^1]
      of 2:
        let d = [-4, -2, -1, 1, 2, 4][r.rand(5)]
        let body = l.strip(trailing = false)
        l = repeat(' ', max(0, l.len - body.len + d)) & body
      of 3:
        if toks.len > 0:
          let (a, _) = toks[r.rand(toks.len - 1)]
          l = l[0 ..< a] & Inserts[r.rand(Inserts.high)] & " " & l[a .. ^1]
      else:
        if i + 1 < ls.len:
          l = l & " " & ls[i+1].strip(trailing = false)
          ls.delete i + 1
      ls[i] = l
      inc n
      writeFile outdir / ("m" & align($n, 5, '0') & ".nim"), ls.join("\n")
  echo n, " mutants"

main()

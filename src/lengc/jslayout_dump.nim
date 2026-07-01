#
#           Leng Compiler — layout dump (test/debug tool)
#        (c) Copyright 2026 Andreas Rumpf
#
## Prints the computed `(size, align)` and field byte-offsets for every top-level
## type declaration in a Leng `.c.nif` module. Used to golden-test `jslayout`
## (the Typed-Array layout pass) against known-answer offsets, without wiring a
## new subcommand into the `lengc` driver.
##
##   jslayout_dump <file.c.nif>

import std / [os, syncio]
import ".." / lib / nifcoreparse
import ".." / lib / nifcdecl
import nifmodules
import noptions
import mangler
import jslayout

proc nameOf(m: MainModule; s: SymId): string = mangleToC(m.pool.syms[s])

proc main() =
  if paramCount() < 1:
    quit "usage: jslayout_dump <file.c.nif>"
  var m = load(paramStr(1))
  m.config = ConfigRef()
  var n = beginRead(m.src)
  if n.stmtKind == StmtsS:
    n.loopInto:
      if n.stmtKind == TypeS:
        let decl = takeTypeDecl(n)
        let lay = typeLayout(m, decl.body)
        stdout.writeLine nameOf(m, decl.name.symId) &
          " size=" & $lay.size & " align=" & $lay.align
        for f in objectFields(m, decl.body):
          stdout.writeLine "  " & nameOf(m, f.sym) & " @" & $f.offset
      else:
        skip n
  else:
    quit "expected a (stmts ...) module root"

main()

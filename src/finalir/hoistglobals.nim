#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A `var x {.global.}` inside a routine is one location for the whole
## program. Sem gives it a module-unique name (`semLocal`); this pass moves its
## declaration out of the routine to module scope, so no later pass sees a
## global inside a routine body: the inliner copied it per inlined call
## (#3167) and the duplifier reset it per call (#3159). The `global` pragma is
## dropped, it has done its job.
##
## The hoisted declarations go in front of the module's own statements, after
## its imports: like Nim 2, a routine's global is initialized once, before the
## module's toplevel code runs. A generic routine is left alone; its instances
## are routines of their own and hoisted like any other.

import std / assertions
include ".." / lib / nifprelude
import nimony_model, decls

proc hasGlobalPragma(buf: TokenBuf): bool =
  result = false
  for i in 0 ..< buf.len:
    let n = readonlyCursorAt(buf, i)
    if n.isTagLit and n.pragmaKind == GlobalP:
      return true

proc isGenericRoutine(n: Cursor): bool =
  let r = asRoutine(n)
  result = r.typevars.substructureKind == TypevarsU

proc copyPragmasWithoutGlobal(dest: var TokenBuf; n: var Cursor) =
  if n.substructureKind != PragmasU:
    dest.takeTree n
  else:
    var kept = createTokenBuf(8)
    n.into:
      while n.hasMore:
        if n.pragmaKind == GlobalP: skip n
        else: kept.takeTree n
    if kept.len == 0:
      dest.addDotToken()
    else:
      dest.addParLe PragmasU, NoLineInfo
      dest.add kept
      dest.addParRi()

proc copyGlobalDecl(dest: var TokenBuf; n: var Cursor) =
  ## `(gvar :x export pragmas type value)` without its `global` pragma.
  takeInto dest, n:
    dest.takeTree n # name
    dest.takeTree n # export marker
    copyPragmasWithoutGlobal dest, n
    while n.hasMore: dest.takeTree n # type, value

proc copyHoisting(dest, hoisted: var TokenBuf; n: var Cursor; inRoutine: bool) =
  if not n.isTagLit:
    dest.takeTree n
  else:
    case n.stmtKind
    of GvarS, GletS:
      if inRoutine: copyGlobalDecl hoisted, n
      else: copyGlobalDecl dest, n
    of ProcS, FuncS, IteratorS, ConverterS, MethodS:
      if isGenericRoutine(n):
        dest.takeTree n
      else:
        takeInto dest, n:
          while n.hasMore: copyHoisting dest, hoisted, n, true
    of TemplateS, MacroS:
      dest.takeTree n
    else:
      takeInto dest, n:
        while n.hasMore: copyHoisting dest, hoisted, n, inRoutine

proc hoistRoutineGlobals*(buf: var TokenBuf) =
  if hasGlobalPragma(buf):
    var n = beginRead(buf)
    assert n.stmtKind == StmtsS
    var imports = createTokenBuf(64)
    var hoisted = createTokenBuf(64)
    var rest = createTokenBuf(buf.len)
    let info = n.info
    n.into:
      while n.hasMore and n.stmtKind == ImportS:
        imports.takeTree n
      while n.hasMore:
        copyHoisting rest, hoisted, n, false
    endRead n
    var result = createTokenBuf(buf.len)
    result.addParLe StmtsS, info
    result.add imports
    result.add hoisted
    result.add rest
    result.addParRi()
    buf = ensureMove result

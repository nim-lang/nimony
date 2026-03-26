#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [tables, sets, syncio]

include nifprelude
import lifter
import ".." / nimony / [nimony_model, typenav, langmodes]

export RcField, DataField

type
  EContext* = object
    dir*, main*, ext*: string
    nestedIn*: seq[(StmtKind, SymId)]
    dynlibs*: Table[StrId, seq[(SymId, StrId, SymId)]]
    strLits*: Table[string, SymId]
    newTypes*: Table[string, SymId]
    pending*: TokenBuf
    strLitBuf*: TokenBuf   ## static LongString const decls for SSO long literals
    strLitCounter*: int    ## unique suffix for strLitBuf symbols
    typeCache*: TypeCache
    bits*: int
    bigEndian*: bool

    breaks*: seq[SymId] # how to translate `break`
    continues*: seq[SymId] # how to translate `continue`
    exceptLabels*: seq[SymId] # how to translate `except`
    instId*: int # per forStmt
    tmpId*: int # per proc
    resultSym*: SymId

    localDeclCounters*: int
    activeChecks*: set[CheckMode]
    liftingCtx*: ref LiftingCtx
    importedModuleSuffixes*: seq[string]
    initBody*: TokenBuf

proc getTmpId*(e: var EContext): int {.inline.} =
  result = e.tmpId
  inc e.tmpId

proc error*(e: var EContext; msg: string; c: Cursor) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(c)
  when defined(debug):
    echo getStackTrace()
  quit 1

proc error*(e: var EContext; msg: string) {.noreturn.} =
  write stdout, "[Error] "
  writeLine stdout, msg
  when defined(debug):
    echo getStackTrace()
  quit 1


proc takeParRi*(e: var EContext; dest: var TokenBuf; c: var Cursor) =
  if c.kind == ParRi:
    dest.add c
    inc c
  else:
    error e, "expected ')', but got: ", c

proc skipParRi*(e: var EContext; c: var Cursor) =
  if c.kind == ParRi:
    inc c
  else:
    error e, "expected ')', but got: ", c

template loop*(e: var EContext; dest: var TokenBuf; c: var Cursor; body: untyped) =
  while true:
    case c.kind
    of ParRi:
      dest.add c
      inc c
      break
    of EofToken:
      error e, "expected ')', but EOF reached"
    else: discard
    body

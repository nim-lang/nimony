#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [tables, sets, hashes, syncio, assertions]

when defined(nimony):
  {.feature: "lenientnils".}


include ".." / lib / nifprelude
include ".." / lib / compat2
import lifter
import ".." / nimony / [nimony_model, typenav, langmodes, sizeof]

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
    sizeofCache*: SizeofCache  ## shared size-by-symbol memoization
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


# The classic `takeParRi`/`skipParRi`/`loop` helpers are gone: they required
# a physical close token, which ParRi elision removes. Use the bounded-scope
# API (`into`/`takeInto`/`enterScope`+`leaveScope`) instead.

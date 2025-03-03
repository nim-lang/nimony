#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std / [tables, sets, syncio]

include nifprelude
import ".." / nimony / [nimony_model, typenav]

const
  RcField* = "r.0."
  DataField* = "d.0."
  GeneratedTypeSuffix* = ".0.t"

type
  MangleScope* {.acyclic.} = ref object
    tab: Table[SymId, string]
    parent: MangleScope

  EContext* = object
    dir*, main*, ext*: string
    dest*: TokenBuf
    declared*: HashSet[SymId]
    requires*: seq[SymId]
    nestedIn*: seq[(StmtKind, SymId)]
    headers*: HashSet[StrId]
    currentOwner*: SymId
    toMangle*: MangleScope
    strLits*: Table[string, SymId]
    newTypes*: Table[string, SymId]
    pending*: TokenBuf
    typeCache*: TypeCache
    bits*: int

    breaks*: seq[SymId] # how to translate `break`
    continues*: seq[SymId] # how to translate `continue`
    instId*: int # per forStmt
    tmpId*: int # per proc
    inImpSection*: int
    resultSym*: SymId

proc getTmpId*(e: var EContext): int {.inline.} =
  result = e.tmpId
  inc e.tmpId

proc openMangleScope*(e: var EContext) =
  e.toMangle = MangleScope(tab: initTable[SymId, string](), parent: e.toMangle)
  e.typeCache.openScope()

proc closeMangleScope*(e: var EContext) =
  e.toMangle = e.toMangle.parent
  e.typeCache.closeScope()

proc registerMangle*(e: var EContext; s: SymId; ext: string) =
  e.toMangle.tab[s] = ext

proc registerMangleInParent*(e: var EContext; s: SymId; ext: string) =
  e.toMangle.parent.tab[s] = ext

proc maybeMangle*(e: var EContext; s: SymId): string =
  var it {.cursor.} = e.toMangle
  while it != nil:
    result = it.tab.getOrDefault(s)
    if result != "":
      return result
    it = it.parent
  return ""

proc error*(e: var EContext; msg: string; c: Cursor) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(c)
  when defined(debug):
    echo getStackTrace()
  quit 1

proc error*(e: var EContext; msg: string) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  when defined(debug):
    echo getStackTrace()
  quit 1


proc tagToken*(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

proc takeParRi*(e: var EContext; c: var Cursor) =
  if c.kind == ParRi:
    e.dest.add c
    inc c
  else:
    error e, "expected ')', but got: ", c

proc skipParRi*(e: var EContext; c: var Cursor) =
  if c.kind == ParRi:
    inc c
  else:
    error e, "expected ')', but got: ", c

template loop*(e: var EContext; c: var Cursor; body: untyped) =
  while true:
    case c.kind
    of ParRi:
      e.dest.add c
      inc c
      break
    of EofToken:
      error e, "expected ')', but EOF reached"
    else: discard
    body

proc takeTree*(e: var EContext; n: var Cursor) =
  takeTree e.dest, n

import std / [tables, sets, syncio]


include nifprelude
import ".." / nimony / [nimony_model, typenav]

type
  SymbolKey* = (SymId, SymId) # (symbol, owner)

  EContext* = object
    dir*, main*, ext*: string
    dest*: TokenBuf
    declared*: HashSet[SymId]
    requires*: seq[SymId]
    nestedIn*: seq[(StmtKind, SymId)]
    headers*: HashSet[StrId]
    currentOwner*: SymId
    toMangle*: Table[SymbolKey, string]
    strLits*: Table[string, SymId]
    newTypes*: Table[string, SymId]
    pending*: TokenBuf
    typeCache*: TypeCache

    breaks*: seq[SymId] # how to translate `break`
    continues*: seq[SymId] # how to translate `continue`
    # TODO: add a instID for each forStmt
    tmpId*: int # per proc


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

proc wantParRi*(e: var EContext; c: var Cursor) =
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
  if n.kind != ParLe:
    e.dest.add n
    inc n
  else:
    var nested = 0
    while true:
      e.dest.add n
      case n.kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0:
          inc n
          break
      of EofToken:
        error e, "expected ')', but EOF reached"
      else: discard
      inc n

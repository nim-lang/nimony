## Minimal repro for the stage-1 boot UAF: a seq of structs with Cursor
## fields. Each Cursor comes from `cursorAt` on a Table-stored TokenBuf
## (mimicking typeToCursor → Match.fn.typ → seq[Match]).

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

type
  FnLike = object
    kind: int
    sym: SymId
    typ: Cursor
    flag: bool

  MatchLike = object
    fn: FnLike
    extra: Cursor
    note: int

  Ctx = object
    typeMem: Table[string, TokenBuf]

proc typeToCursorLike(c: var Ctx; key: string; src: TokenBuf): Cursor =
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem.getOrQuit(key), 0)
  else:
    var nb = createTokenBuf(src.len)
    for i in 0 ..< src.len:
      nb.add src[i]
    result = cursorAt(nb, 0)
    c.typeMem[key] = nb

proc buildCandidate(c: var Ctx; key: string; src: TokenBuf; symbol: SymId): FnLike =
  let typ = typeToCursorLike(c, key, src)
  result = FnLike(kind: 0, sym: symbol, typ: typ, flag: false)

proc createMatch(): MatchLike =
  MatchLike(fn: FnLike(kind: 0, sym: SymId(0), flag: false), note: -1)

proc setFn(m: var MatchLike; fn: FnLike) =
  m.fn = fn

proc resolveLike(c: var Ctx) =
  # mimic resolveOverloads' seq[Match] over a few candidates
  var matches: seq[MatchLike] = @[]
  for i in 0 ..< 12:
    # synthesize a small TokenBuf for the type signature
    var srcBuf = createTokenBuf(8)
    srcBuf.add parLeToken(pool.tags.getOrIncl("proc"), NoLineInfo)
    srcBuf.add intToken(pool.integers.getOrIncl(i.int64), NoLineInfo)
    srcBuf.add parRiToken(NoLineInfo)
    let key = "k" & $i
    let candidate = buildCandidate(c, key, srcBuf, SymId(uint32(i + 1)))
    matches.add createMatch()
    setFn(matches[^1], candidate)
  # `matches` is destroyed at scope end → MatchLike's destroy → FnLike's
  # destroy → Cursor's =destroy on each `typ`.
  echo "matches.len=", matches.len

proc main() =
  discard registerTag("proc")
  var c = Ctx(typeMem: initTable[string, TokenBuf]())
  # run many rounds so mimalloc reuses slots between cursors
  for round in 0 ..< 8:
    resolveLike(c)
  echo "OK"

main()

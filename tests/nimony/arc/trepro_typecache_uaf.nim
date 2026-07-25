## Nimony-compiled repro for the stage-2 boot UAF
## (`bug_stage2_typecache_destroy_uaf.md`). Mirrors `typenav.nim`'s
## TypeCache → TypeScope → Table[SymId, LocalInfo] → Cursor shape and
## drives it through the same lifecycle the bootstrap exercises.
##
## Crash signature when this is broken:
##
##   mimalloc: assertion failed: at "vendor/mimalloc/src/free.c":446,
##   mi_page_usable_size_of  assertion: "ok"
##
## fired inside the auto-derived `TypeCache.=destroy` chain
##   TypeCache → TypeScope → Table[SymId, LocalInfo] → Cursor.=destroy
## because the Cursor field's `owner` pointer references a TokenBuf
## already torn down earlier in the chain.

import std / [tables, syncio, assertions]
import "../../../src/lib/nifcore"

type
  SymKind = enum
    NoSym, VarY, LetY, ResultY, ConstY, ParamY, TypevarY, CursorY, PatternvarY,
    FldY, GfldY, EfldY, GletY, TletY, GvarY, TvarY, ProcY

  LocalInfo = object
    kind: SymKind
    crossedProc: int16
    typ: Cursor

  ScopeKind = enum
    OtherScope, ProcScope, UnusedScope

  TypeScope {.acyclic.} = ref object
    locals: Table[SymId, LocalInfo]
    parent: TypeScope
    kind: ScopeKind

  TypeCache = object
    mem: seq[TokenBuf]
    current: TypeScope

proc createTypeCache(): TypeCache =
  TypeCache()

proc openScope(c: var TypeCache; kind = OtherScope) =
  c.current = TypeScope(
    locals: initTable[SymId, LocalInfo](),
    parent: c.current, kind: kind)

proc closeScope(c: var TypeCache) =
  c.current = c.current.parent

proc registerLocal(c: var TypeCache; s: SymId; kind: SymKind; typ: Cursor) =
  c.current.locals[s] = LocalInfo(kind: kind, typ: typ)

proc getLocalInfo(c: var TypeCache; s: SymId): LocalInfo =
  var it {.cursor.} = c.current
  var crossedProc = 0
  while it != nil:
    var res = it.locals.getOrDefault(s)
    if res.kind != NoSym:
      res.crossedProc = int16(crossedProc)
      return res
    it = it.parent
  return default(LocalInfo)

proc addTypeBuf(c: var TypeCache; tagName: string): Cursor =
  var buf = createTokenBuf(4)
  buf.openTag buf.tags.registerTag(tagName)
  buf.closeTag()
  c.mem.add ensureMove buf
  result = cursorAt(c.mem[c.mem.len-1], 0)

proc exerciseFlat() =
  ## Single-scope variant — mirrors the bootstrap's main-module sym
  ## registration. The TokenBuf seq reallocates as `mem` grows past its
  ## initial capacity; cursors stored in `LocalInfo.typ` need to survive
  ## that.
  var c = createTypeCache()
  c.openScope(OtherScope)
  for i in 0 ..< 8000:
    let s = SymId(i + 1)
    let t = c.addTypeBuf("int")
    c.registerLocal(s, VarY, t)
  var seen = 0
  for i in 0 ..< 8000:
    let info = c.getLocalInfo(SymId(i + 1))
    assert info.kind == VarY
    inc seen
  assert seen == 8000
  c.closeScope()

proc exerciseNested() =
  ## Push/pop many scopes, registering at each level. This is closer to
  ## what the bootstrap exercises while sem-checking proc bodies with
  ## inner blocks.
  var c = createTypeCache()
  var symBase = 1
  for depth in 0 ..< 32:
    c.openScope(if depth mod 4 == 0: ProcScope else: OtherScope)
    for i in 0 ..< 200:
      let s = SymId(symBase + i)
      let t = c.addTypeBuf("int")
      c.registerLocal(s, VarY, t)
    symBase += 200
  # Walk scopes from innermost outward via `getLocalInfo`'s parent chain
  for i in 0 ..< symBase - 1:
    let info = c.getLocalInfo(SymId(i + 1))
    assert info.kind == VarY
  for depth in 0 ..< 32:
    c.closeScope()

proc exerciseMixed() =
  ## Interleave queries and registrations so the inner Table reallocates
  ## while cursors are also being read out by value.
  var c = createTypeCache()
  c.openScope()
  for i in 0 ..< 4000:
    let s = SymId(i + 1)
    let t = c.addTypeBuf("int")
    c.registerLocal(s, VarY, t)
    if i > 0 and (i mod 7) == 0:
      let info = c.getLocalInfo(SymId(i))
      assert info.kind == VarY
  c.closeScope()

proc main =
  for round in 0 ..< 4:
    exerciseFlat()
    exerciseNested()
    exerciseMixed()
    echo "round ", round, " ok"

main()
echo "DONE"

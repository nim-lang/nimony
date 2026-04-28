#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Cursors into token streams. Suprisingly effective even for more complex algorithms.
##
## Memory layout
## =============
##
## A `TokenBuf` always owns a contiguous data buffer pointed at by `b.data`.
## Cursors borrow into that buffer. To make borrowing safe across mutations
## of `b`, an extra reference-counted control block — the `CursorOwner`
## header — coordinates sharing.
##
## **Detached state** (no live Cursors). `b.owner == nil`; the TokenBuf
## directly owns its data buffer:
##
## ```
##   TokenBuf b
##   ┌──────────────┐         data buffer (alloc #1)
##   │ data ────────┼──────▶ ┌─────────┐
##   │ len, cap     │        │ tokens  │
##   │ owner: nil   │        │  ...    │
##   └──────────────┘        └─────────┘
## ```
##
## **Attached state** (one or more live Cursors). `beginRead` /
## `cursorAt` lazily allocate a small `CursorOwnerObj` header (alloc #2)
## whose `data` field aliases `b.data`. `rc` counts: 1 for the TokenBuf
## plus 1 per live Cursor.
##
## ```
##   TokenBuf b
##   ┌──────────────┐         data buffer (alloc #1)
##   │ data ────────┼──────▶ ┌─────────┐
##   │ len, cap     │  ┌─▶   │ tokens  │
##   │ owner ───────┼──┼┐    │  ...    │
##   └──────────────┘  ││    └─────────┘
##                     ││
##           Cursor c1 ││         CursorOwner (alloc #2)
##           ┌────────┐││         ┌─────────┐
##           │ owner ─┼┘│         │ rc: 3   │
##           │ p ─────┼─┼─────▶   │ data ───┼─── points at alloc #1
##           │ rem    │ │         └─────────┘
##           └────────┘ │              ▲
##                      │              │
##           Cursor c2  │              │
##           ┌────────┐ │              │
##           │ owner ─┼─┴──────────────┘
##           │ p      │
##           │ rem    │
##           └────────┘
## ```
##
## Allocations #1 and #2 are independent. Freeing the header (#2) does
## not touch the data buffer (#1) and vice versa. `decRcAndFree(owner)`
## frees both — but only when `rc` decrements to 0.
##
## Mutations and COW
## -----------------
##
## `prepareMutation` is called before any mutation that would invalidate
## existing cursors. Two cases:
##
## - `rc == 1`: only the TokenBuf holds a ref; no cursor is reading.
##   Free the header (#2) and revert to detached state — `b.data` keeps
##   pointing at #1, which the TokenBuf now owns directly.
##
## - `rc > 1`: cursors still read #1. Allocate a fresh data buffer and
##   `copyMem` into it; release one ref on the old owner via
##   `decRcAndFree`. Old cursors continue reading the old #1 (kept alive
##   by their own rc refs); the TokenBuf now owns the new buffer.
##
## Hot-path callers can stay on the no-copy branch by calling
## `endRead(c)` on cursors before mutating the parent buffer, and
## `expectUnique(b)` asserts that contract in debug builds.

import std / [assertions, syncio]
import nifreader, nifstreams, bitabs, lineinfos, vfs

include compat2

when defined(prepMutStats):
  var cowFastCount*: int
  var cowSlowCount*: int
  var cowSlowBytes*: int

type
  Storage = ptr UncheckedArray[PackedToken]
  CursorOwner = ptr CursorOwnerObj
  CursorOwnerObj = object
    rc: int          ## 1 (TokenBuf) + number of live Cursors sharing this control block
    data: Storage    ## the shared backing storage; aliases TokenBuf.data when attached

  Cursor* = object
    owner: CursorOwner
    p: ptr PackedToken
    rem: int

template decRcAndFree(owner: CursorOwner) =
  dec owner.rc
  if owner.rc == 0:
    if owner.data != nil: dealloc(owner.data)
    dealloc(owner)

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`*(c: Cursor) {.inline.} =
    if c.owner != nil:
      decRcAndFree(c.owner)
else:
  proc `=destroy`*(c: var Cursor) {.inline.} =
    if c.owner != nil:
      decRcAndFree(c.owner)

proc `=wasMoved`*(c: var Cursor) {.inline.} =
  c.owner = nil
  c.p = nil
  c.rem = 0

proc `=copy`*(dest: var Cursor; src: Cursor) =
  if dest.owner != src.owner or dest.p != src.p:
    `=destroy`(dest)
    if src.owner != nil: inc src.owner.rc
    dest.owner = src.owner
    dest.p = src.p
    dest.rem = src.rem

proc `=dup`*(src: Cursor): Cursor {.nodestroy.} =
  result = Cursor(owner: src.owner, p: src.p, rem: src.rem)
  if result.owner != nil: inc result.owner.rc

let
  ErrToken = [parLeToken(ErrT, NoLineInfo),
              parRiToken(NoLineInfo)]

proc errCursor*(): Cursor =
  Cursor(p: addr ErrToken[0], rem: 2)

proc fromBuffer*(x: openArray[PackedToken]): Cursor {.inline.} =
  Cursor(p: addr(x[0]), rem: x.len)

proc setSpan*(c: var Cursor; beyondLast: Cursor) {.inline.} =
  c.rem = (cast[int](beyondLast.p) - cast[int](c.p)) div sizeof(PackedToken)

proc load*(c: Cursor): PackedToken {.inline.} =
  assert c.p != nil and c.rem > 0
  c.p[]

proc kind*(c: Cursor): NifKind {.inline.} = c.load.kind

proc info*(c: Cursor): PackedLineInfo {.inline.} = c.load.info
proc setInfo*(c: Cursor; info: PackedLineInfo) {.inline.} = c.p[].info = info

proc litId*(c: Cursor): StrId {.inline.} = nifstreams.litId(c.load)
proc symId*(c: Cursor): SymId {.inline.} = nifstreams.symId(c.load)

proc charLit*(c: Cursor): char {.inline.} = nifstreams.charLit(c.load)

proc intId*(c: Cursor): IntId {.inline.} = nifstreams.intId(c.load)
proc uintId*(c: Cursor): UIntId {.inline.} = nifstreams.uintId(c.load)
proc floatId*(c: Cursor): FloatId {.inline.} = nifstreams.floatId(c.load)
proc tagId*(c: Cursor): TagId {.inline.} = nifstreams.tagId(c.load)

proc tag*(c: Cursor): TagId {.inline.} = nifstreams.tag(c.load)

proc uoperand*(c: Cursor): uint32 {.inline.} = nifstreams.uoperand(c.load)
proc soperand*(c: Cursor): int32 {.inline.} = nifstreams.soperand(c.load)

proc getInt28*(c: Cursor): int32 {.inline.} = nifstreams.getInt28(c.load)

proc inc*(c: var Cursor) {.inline.} =
  assert c.rem > 0
  c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
  dec c.rem

proc unsafeDec*(c: var Cursor) {.inline.} =
  c.p = cast[ptr PackedToken](cast[uint](c.p) - sizeof(PackedToken).uint)
  inc c.rem

proc `+!`*(c: Cursor; diff: int): Cursor {.inline.} =
  assert diff <= c.rem
  result = Cursor(owner: c.owner,
     p: cast[ptr PackedToken](cast[uint](c.p) + diff.uint * sizeof(PackedToken).uint),
     rem: c.rem - diff)
  if result.owner != nil: inc result.owner.rc

proc cursorIsNil*(c: Cursor): bool {.inline.} =
  result = c.p == nil

proc hasCurrentToken*(c: Cursor): bool {.inline.} =
  result = c.p != nil and c.rem > 0

proc skip*(c: var Cursor) =
  if c.kind == ParLe:
    var nested = 0
    while true:
      inc c
      if c.kind == ParRi:
        if nested == 0: break
        dec nested
      elif c.kind == ParLe: inc nested
  inc c

proc skipToEnd*(c: var Cursor) =
  ## skips `c` until an unmatched ParRi is found, then skips the ParRi and returns
  var nested = 0
  while true:
    if c.kind == ParRi:
      if nested == 0:
        inc c
        break
      dec nested
    elif c.kind == ParLe:
      inc nested
    inc c

proc skipUntilEnd*(c: var Cursor) =
  ## skips `c` until an unmatched ParRi is found, then returns without skipping the ParRi
  var nested = 0
  while true:
    if c.kind == ParRi:
      if nested == 0:
        break
      dec nested
    elif c.kind == ParLe:
      inc nested
    inc c

# ── Traversal templates ──────────────────────────────────────────────────
# Pure traversal helpers for reading/analyzing a tree without producing output.

template hasMore*(n: Cursor): bool =
  ## True while there are more children before the closing `)`.
  n.kind != ParRi

template into*(n: var Cursor; body: untyped) =
  ## Enters the current node, runs `body` to process the children,
  ## then advances past the closing `)`.
  assert n.kind == ParLe, "into requires cursor at ParLe"
  inc n          # skip opening tag
  body
  assert n.kind == ParRi, "into: body must consume all children"
  inc n          # skip closing ParRi

template loopInto*(n: var Cursor; body: untyped) =
  ## Enters a node, iterates all children, then advances past `)`.
  ## The body must advance `n` on every iteration.
  into n:
    while n.hasMore:
      body

template balancedTokens*(n: var Cursor; body: untyped) =
  ## Deep-scans all `ParLe` nodes in the subtree rooted at `n`.
  ## Inside `body`, `n` is positioned at each `ParLe` node in turn.
  ## `body` must **not** advance `n` — the template handles traversal.
  var nestedDepth = 0
  if n.kind == ParLe:
    inc nestedDepth; inc n
    while nestedDepth > 0:
      case n.kind
      of ParLe:
        body
        inc nestedDepth; inc n
      of ParRi:
        dec nestedDepth; inc n
      else:
        inc n

type
  TokenBuf* = object
    data: Storage
    len, cap: int
    owner: CursorOwner  ## nil = exclusive ownership of data.
                        ## non-nil = data is shared via CursorOwner with Cursors;
                        ## b.data aliases owner.data. TokenBuf holds one rc ref.

proc `=copy`(dest: var TokenBuf; src: TokenBuf) {.error.}
proc `=wasMoved`(dest: var TokenBuf) {.inline.} =
  dest.data = nil
  dest.len = 0
  dest.cap = 0
  dest.owner = nil

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`(dest: TokenBuf) {.inline.} =
    if dest.owner != nil:
      decRcAndFree(dest.owner)
    else:
      if dest.data != nil: dealloc(dest.data)
else:
  proc `=destroy`(dest: var TokenBuf) {.inline.} =
    if dest.owner != nil:
      decRcAndFree(dest.owner)
    else:
      if dest.data != nil: dealloc(dest.data)

proc createTokenBuf*(cap = 100): TokenBuf =
  result = TokenBuf(data: cast[Storage](alloc(sizeof(PackedToken)*cap)), len: 0, cap: cap)

proc prepareMutation*(b: var TokenBuf) {.inline.} =
  ## Detach the buffer from its CursorOwner so the next mutation does
  ## not invalidate existing cursors. Two paths:
  ##
  ## - **No live cursors (rc == 1).** The TokenBuf is the only ref to the
  ##   owner header; no cursor reads the data anymore. Reuse the existing
  ##   data buffer in place — only the small owner header gets freed.
  ##   `b.data` already aliases `b.owner.data`, so we capture the
  ##   pointer, drop the header, and re-install it as the TokenBuf's
  ##   single-owner data pointer. No alloc, no copy.
  ##
  ## - **Live cursors (rc > 1).** Cursors still read the data; we must
  ##   detach by deep-copying so the old buffer stays valid for them.
  ##
  ## The fast path is what callers achieve by calling `endRead` on their
  ## cursors before the next mutation; without `endRead`, the slow COW
  ## still keeps things correct.
  if b.owner != nil:
    if b.owner.rc == 1:
      # Only the TokenBuf itself holds a ref; no live cursor reads the
      # buffer. Free the owner header (a separate small allocation) and
      # revert to single-owner mode. `b.data` stays valid — it points at
      # the data buffer, which is a different allocation from the header.
      # See the layout diagram at the top of the file.
      dealloc(b.owner)
      b.owner = nil
      when defined(prepMutStats): inc cowFastCount
    else:
      let newData = cast[Storage](alloc(sizeof(PackedToken) * b.cap))
      copyMem(newData, b.data, sizeof(PackedToken) * b.len)
      decRcAndFree(b.owner)
      b.owner = nil
      b.data = newData
      when defined(prepMutStats):
        inc cowSlowCount
        cowSlowBytes += b.len * sizeof(PackedToken)

proc freeze*(b: var TokenBuf) {.inline.} = discard
proc thaw*(b: var TokenBuf) {.inline.} = discard

proc expectUnique*(b: var TokenBuf) {.inline.} =
  ## Hot-path mutation sites stamp this to assert that no cursor
  ## currently holds an rc ref against `b`. Catches a missing `endRead`
  ## that would otherwise silently force `prepareMutation` onto the
  ## copying path. No-op in release builds — COW remains the correctness
  ## floor, this is just a tripwire for performance regressions.
  when defined(debug):
    assert b.owner == nil or b.owner.rc == 1,
      "TokenBuf has live cursors; missing endRead before mutation"

proc beginRead*(b: var TokenBuf): Cursor =
  ## Returns a Cursor into the buffer. Creates a CursorOwner on first call
  ## (rc=2: one for TokenBuf, one for the returned Cursor).
  if b.owner == nil:
    b.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
    b.owner.data = b.data
    b.owner.rc = 1  # 1 for TokenBuf
  inc b.owner.rc  # + 1 for the returned Cursor
  result = Cursor(owner: b.owner, p: addr(b.data[0]), rem: b.len)

proc endRead*(b: var TokenBuf) {.inline.} = discard

proc endRead*(c: var Cursor) {.inline.} =
  ## Release the cursor's rc ref against its owner buffer eagerly. After
  ## this call the cursor is moved-out (`c.owner == nil`); subsequent
  ## mutations on the underlying TokenBuf can take the fast no-copy path
  ## in `prepareMutation` if no other cursors are live.
  if c.owner != nil:
    decRcAndFree(c.owner)
  `=wasMoved`(c)

proc add*(b: var TokenBuf; item: PackedToken) {.inline.} =
  if b.owner != nil: prepareMutation(b)
  if b.len >= b.cap:
    b.cap = max(b.cap div 2 + b.cap, 8)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.data[b.len] = item
  inc b.len

proc len*(b: TokenBuf): int {.inline.} = b.len

when defined(nimony):
  proc `[]`*(b: TokenBuf; i: int): var PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]
else:
  proc `[]`*(b: TokenBuf; i: int): PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]

  proc `[]`*(b: var TokenBuf; i: int): var PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]

proc `[]=`*(b: TokenBuf; i: int; val: PackedToken) {.inline.} =
  assert i >= 0 and i < b.len
  b.data[i] = val

proc cursorAt*(b: var TokenBuf; i: int): Cursor {.inline.} =
  assert i >= 0 and i < b.len
  if b.owner == nil:
    b.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
    b.owner.data = b.data
    b.owner.rc = 1  # 1 for TokenBuf
  inc b.owner.rc  # + 1 for the returned Cursor
  result = Cursor(owner: b.owner, p: addr b.data[i], rem: b.len-i)

proc readonlyCursorAt*(b: TokenBuf; i: int): Cursor {.inline.} =
  assert i >= 0 and i < b.len
  result = Cursor(p: addr b.data[i], rem: b.len-i)
  if b.owner != nil:
    inc b.owner.rc
    result.owner = b.owner

proc shareRead*(b: var TokenBuf; c: Cursor): Cursor =
  result = c

proc cursorToPosition*(b: TokenBuf; c: Cursor): int {.inline.} =
  result = (cast[int](c.p) - cast[int](b.data)) div sizeof(PackedToken)

proc cursorToPosition*(base, c: Cursor): int {.inline.} =
  let c = cast[int](c.p)
  let base = cast[int](base.p)
  assert c >= base
  result = (c - base) div sizeof(PackedToken)
  assert result < 1_000_000

proc toUniqueId*(c: Cursor): int {.inline.} =
  result = cast[int](c.p)

proc add*(result: var TokenBuf; c: Cursor) =
  result.add c.load

proc addToken*(dest: var TokenBuf; t: PackedToken) {.inline.} =
  ## Adds a single token to dest. Prefer this over `dest.add` in compiler
  ## passes so that static analysis tools can track NIF construction.
  dest.add t

proc addToken*(dest: var TokenBuf; c: Cursor) {.inline.} =
  ## Adds a single token (from cursor position) to dest.
  dest.add c.load

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symToken(s, info)

proc addSymDef*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symdefToken(s, info)

proc addSubtree*(result: var TokenBuf; c: Cursor) =
  assert c.kind != ParRi, "cursor at end?"
  if c.kind != ParLe:
    # atom:
    result.add c.load
  else:
    var c = c
    var nested = 0
    while true:
      let item = c.load
      result.add item
      if item.kind == ParRi:
        dec nested
        if nested == 0: break
      elif item.kind == ParLe: inc nested
      inc c

proc addUnstructured*(result: var TokenBuf; c: Cursor) =
  var c = c
  while c.rem > 0:
    result.add c.load
    inc c

proc add*(result: var TokenBuf; s: var Stream) =
  let c = next(s)
  assert c.kind != ParRi, "cursor at end?"
  result.add c
  if c.kind == ParLe:
    var nested = 0
    while true:
      let item = next(s)
      result.add item
      if item.kind == ParRi:
        if nested == 0: break
        dec nested
      elif item.kind == ParLe: inc nested

iterator items*(b: TokenBuf): PackedToken =
  for i in 0 ..< b.len:
    yield b.data[i]

proc add*(dest: var TokenBuf; src: TokenBuf) =
  for t in items(src): dest.add t

proc fromCursor*(c: Cursor): TokenBuf =
  result = createTokenBuf(4)
  result.add c

proc fromStream*(s: var Stream): TokenBuf =
  result = createTokenBuf(4)
  result.add s

proc shrink*(b: var TokenBuf; newLen: int) =
  if b.owner != nil: prepareMutation(b)
  assert newLen >= 0 and newLen <= b.len
  b.len = newLen

proc grow(b: var TokenBuf; newLen: int) =
  if b.owner != nil: prepareMutation(b)
  assert newLen > b.len
  if b.cap < newLen:
    b.cap = max(b.cap div 2 + b.cap, newLen)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.len = newLen

template buildTree*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(tag, info)
  body
  dest.add parRiToken(info)

proc addParLe*(dest: var TokenBuf; tag: TagId; info = NoLineInfo) =
  dest.add parLeToken(tag, info)

proc addParRi*(dest: var TokenBuf) =
  dest.add parRiToken(NoLineInfo)

proc addDotToken*(dest: var TokenBuf) =
  dest.add dotToken(NoLineInfo)

proc addStrLit*(dest: var TokenBuf; s: string; info = NoLineInfo) =
  dest.add strToken(pool.strings.getOrIncl(s), info)

proc addIntLit*(dest: var TokenBuf; i: BiggestInt; info = NoLineInfo) =
  dest.add intToken(pool.integers.getOrIncl(i), info)

proc addUIntLit*(dest: var TokenBuf; i: BiggestUInt; info = NoLineInfo) =
  dest.add uintToken(pool.uintegers.getOrIncl(i), info)

proc addIdent*(dest: var TokenBuf; s: string; info = NoLineInfo) =
  dest.add identToken(pool.strings.getOrIncl(s), info)

proc addCharLit*(dest: var TokenBuf; c: char; info = NoLineInfo) =
  dest.add charToken(c, info)

proc addFloatLit*(dest: var TokenBuf; f: BiggestFloat; info = NoLineInfo) =
  dest.add floatToken(pool.floats.getOrIncl(f), info)

proc span*(c: Cursor): int =
  result = 0
  var c = c
  if c.kind == ParLe:
    var nested = 0
    while true:
      inc c
      inc result
      if c.kind == ParRi:
        if nested == 0: break
        dec nested
      elif c.kind == ParLe: inc nested
  if c.rem > 0:
    inc c
    inc result

proc insert*(dest: var TokenBuf; src: openArray[PackedToken]; pos: int) =
  var j = len(dest) - 1
  var i = j + len(src)
  dest.grow(i + 1)

  # Move items after `pos` to the end of the sequence.
  while j >= pos:
    dest[i] = dest[j]
    dec i
    dec j
  # Insert items from `dest` into `dest` at `pos`
  inc j
  for item in src:
    dest[j] = item
    inc j

proc insert*(dest: var TokenBuf; src: Cursor; pos: int) =
  insert dest, toOpenArray(cast[ptr  UncheckedArray[PackedToken]](src.p), 0, span(src)-1), pos

proc insert*(dest: var TokenBuf; src: TokenBuf; pos: int) =
  insert dest, toOpenArray(src.data, 0, src.len-1), pos

proc replace*(dest: var TokenBuf; by: Cursor; pos: int) =
  if dest.owner != nil: prepareMutation(dest)
  let len = span(Cursor(p: addr dest.data[pos], rem: dest.len-pos))
  let actualLen = min(len, dest.len - pos)
  let byLen = span(by)
  let oldLen = dest.len
  let newLen = oldLen + byLen - actualLen
  if byLen > actualLen:
    # Need to make room for additional elements
    dest.grow(newLen)
    # Move existing elements to the right
    for i in countdown(oldLen - 1, pos + actualLen):
      dest[i + byLen - actualLen] = dest[i]
  elif byLen < actualLen:
    # Need to remove elements
    for i in pos + byLen ..< dest.len - (actualLen - byLen):
      dest[i] = dest[i + actualLen - byLen]
    dest.shrink(newLen)
  # Copy new elements
  var by = by
  for i in 0 ..< byLen:
    dest[pos + i] = by.load
    inc by

proc toString*(b: TokenBuf; produceLineInfo = true): string =
  result = nifstreams.toString(toOpenArray(b.data, 0, b.len-1), produceLineInfo)

proc toString*(b: TokenBuf; first: int; produceLineInfo = true): string =
  var last = first
  var nested = 0
  while last < b.len:
    case b[last].kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc last
  if last == b.len: dec last
  result = nifstreams.toString(toOpenArray(b.data, first, last), produceLineInfo)

proc toString*(b: Cursor; produceLineInfo = true): string =
  let counter = span(b)
  result = nifstreams.toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, counter-1), produceLineInfo)

proc toStringDebug*(b: Cursor; produceLineInfo = true): string =
  let L = if b.kind == ParLe: 1 else: 0
  result = nifstreams.toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, L), produceLineInfo)

type
  FileWriteMode* = enum
    AlwaysWrite,
    OnlyIfChanged

proc writeFile*(b: TokenBuf; filename: string; mode: FileWriteMode = AlwaysWrite) {.canRaise.} =
  let content = toModuleString(toOpenArray(b.data, 0, b.len-1), "." & extractModuleSuffix(filename))
  if mode == OnlyIfChanged:
    let existingContent = try: vfsRead(filename) except: ""
    if existingContent == content: return
  vfsWrite(filename, content)

proc `$`*(c: Cursor): string = toString(c, false)

template copyInto*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(tag, info)
  body
  dest.add parRiToken(NoLineInfo)

proc parLeTokenUnchecked*(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

template copyIntoUnchecked*(dest: var TokenBuf; tag: string; info: PackedLineInfo; body: untyped) =
  dest.add parLeTokenUnchecked(tag, info)
  body
  dest.add parRiToken(NoLineInfo)

proc parse*(r: var Stream; dest: var TokenBuf;
            parentInfo: PackedLineInfo; debug: bool = false) =
  r.parents[0] = parentInfo
  var nested = 0
  while true:
    let tok = r.next()
    dest.add tok
    if debug:
      echo "parsing ", toString([tok], false)
      if tok.kind == UnknownToken: break
    if tok.kind == EofToken:
      break
    elif tok.kind == ParLe:
      inc nested
    elif tok.kind == ParRi:
      dec nested
      if nested == 0: break

proc parseFromBuffer*(input: string; thisModule: sink string; sizeHint = 100): TokenBuf =
  var r = nifstreams.openFromBuffer(input, thisModule)
  result = createTokenBuf(sizeHint)
  parse(r, result, NoLineInfo)

proc parseFromFile*(filename: string; sizeHint = 100): TokenBuf =
  var r = nifstreams.open(filename)
  discard processDirectives(r.r)
  result = createTokenBuf(sizeHint)
  parse(r, result, NoLineInfo)

proc isLastSon*(n: Cursor): bool =
  var n = n
  skip n
  result = n.kind == ParRi

proc firstSon*(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

proc takeToken*(buf: var TokenBuf; n: var Cursor) {.inline.} =
  buf.add n
  inc n

type
  SkipIntent* = enum
    SkipTag       ## advance past a ParLe tag (entering a node to rewrite children)
    SkipParRi     ## advance past a closing paren
    SkipName      ## skip a name/SymbolDef child
    SkipExport    ## skip an export marker child
    SkipPragmas   ## skip a pragmas section
    SkipType      ## skip a type child
    SkipExpr    ## skip an expression child
    SkipStmt    ## skip a statement child
    SkipValue     ## skip a value/expression child
    SkipGenParams ## skip generic parameters
    SkipCond      ## skip a condition expression
    SkipBody      ## skip a body/stmts section
    SkipEffects   ## skip an effects section
    SkipResult    ## skip a result that has been handled separately
    SkipFull      ## skip an entire subtree being dropped or replaced

template skip*(c: var Cursor; intent: SkipIntent) =
  ## Skip a subtree with declared intent. The intent enum documents why the
  ## input is being dropped and enables the validator to check correctness.
  skip(c)

template inc*(c: var Cursor; intent: SkipIntent) =
  ## Advance one token with declared intent. The intent enum documents why
  ## the token is being dropped and enables the validator to check correctness.
  inc c

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  if n.kind != ParLe:
    dest.add n
    inc n
  else:
    var nested = 0
    while true:
      dest.add n
      case n.kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0:
          inc n
          break
      of EofToken:
        raiseAssert "expected ')', but EOF reached"
      else: discard
      inc n

when isMainModule:
  # test replace
  block:
    var dest = createTokenBuf(1)
    dest.addDotToken()
    block:
      let by = [charToken('a', NoLineInfo)]
      replace dest, fromBuffer(by), 0
      assert dest[0] == by[0]
    block:
      let by2 = [parLeToken(TagId 1, NoLineInfo), parRiToken(NoLineInfo)]
      replace dest, fromBuffer(by2), 0
      assert dest.len == 2
      assert dest[0] == by2[0] and dest[1] == by2[1]

      let by1 = [strToken(StrId 2, NoLineInfo)]
      replace dest, fromBuffer(by1), 0
      assert dest.len == 1
      assert dest[0] == by1[0]
  block:
    var dest = createTokenBuf(3)
    let dest0 = parLeToken(TagId 123, NoLineInfo)
    dest.add dest0
    dest.addDotToken()
    dest.addParRi
    block:
      let by = [charToken('a', NoLineInfo)]
      replace dest, fromBuffer(by), 1
      assert dest.len == 3
      assert dest[0] == dest0
      assert dest[1] == by[0]
      assert dest[2].kind == ParRi

      let by2 = [parLeToken(TagId 456, NoLineInfo), parRiToken(NoLineInfo)]
      replace dest, fromBuffer(by2), 1
      assert dest.len == 4
      assert dest[0] == dest0
      assert dest[1] == by2[0] and dest[2] == by2[1]
      assert dest[3].kind == ParRi

      let by1 = [strToken(StrId 789, NoLineInfo)]
      replace dest, fromBuffer(by1), 1
      assert dest.len == 3
      assert dest[0] == dest0
      assert dest[1] == by1[0]
      assert dest[2].kind == ParRi

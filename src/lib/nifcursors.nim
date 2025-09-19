#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Cursors into token streams. Suprisingly effective even for more complex algorithms.

import std / [assertions, syncio]
import nifreader, nifstreams, bitabs, lineinfos

type
  Cursor* = object
    p: ptr PackedToken
    rem: int

const
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
  result = Cursor(
     p: cast[ptr PackedToken](cast[uint](c.p) + diff.uint * sizeof(PackedToken).uint),
     rem: c.rem - diff)

proc cursorIsNil*(c: Cursor): bool {.inline.} =
  result = c.p == nil

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

type
  Storage = ptr UncheckedArray[PackedToken]
  TokenBuf* = object
    data: Storage
    len, cap, readers: int

proc `=copy`(dest: var TokenBuf; src: TokenBuf) {.error.}
when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`(dest: TokenBuf) {.inline.} =
    #assert dest.readers == 0, "TokenBuf still in use by some reader"
    if dest.data != nil: dealloc(dest.data)
else:
  proc `=destroy`(dest: var TokenBuf) {.inline.} =
    #assert dest.readers == 0, "TokenBuf still in use by some reader"
    if dest.data != nil: dealloc(dest.data)

proc createTokenBuf*(cap = 100): TokenBuf =
  result = TokenBuf(data: cast[Storage](alloc(sizeof(PackedToken)*cap)), len: 0, cap: cap)

proc isMutable(b: TokenBuf): bool {.inline.} = b.cap >= 0

proc freeze*(b: var TokenBuf) {.inline.} =
  if isMutable(b):
    b.cap = -(b.cap+1)

proc thaw*(b: var TokenBuf) =
  if not isMutable(b):
    b.cap = -(b.cap+1)

proc beginRead*(b: var TokenBuf): Cursor =
  if b.readers == 0: freeze(b)
  inc b.readers
  result = Cursor(p: addr(b.data[0]), rem: b.len)

proc endRead*(b: var TokenBuf) =
  assert b.readers > 0, "unpaired endRead"
  dec b.readers
  if b.readers == 0: thaw(b)

proc add*(b: var TokenBuf; item: PackedToken) {.inline.} =
  assert isMutable(b), "attempt to mutate frozen TokenBuf"
  if b.len >= b.cap:
    b.cap = max(b.cap div 2 + b.cap, 8)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.data[b.len] = item
  inc b.len

proc len*(b: TokenBuf): int {.inline.} = b.len

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
  if b.readers == 0: freeze(b)
  inc b.readers
  result = Cursor(p: addr b.data[i], rem: b.len-i)

proc readonlyCursorAt*(b: TokenBuf; i: int): Cursor {.inline.} =
  assert i >= 0 and i < b.len
  assert(not isMutable(b))
  result = Cursor(p: addr b.data[i], rem: b.len-i)

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
  result = TokenBuf(data: cast[Storage](alloc(sizeof(PackedToken)*4)), len: 0, cap: 4)
  result.add c

proc fromStream*(s: var Stream): TokenBuf =
  result = TokenBuf(data: cast[Storage](alloc(sizeof(PackedToken)*4)), len: 0, cap: 4)
  result.add s

proc shrink*(b: var TokenBuf; newLen: int) =
  assert isMutable(b), "attempt to mutate frozen TokenBuf"
  assert newLen >= 0 and newLen <= b.len
  b.len = newLen

proc grow(b: var TokenBuf; newLen: int) =
  assert isMutable(b), "attempt to mutate frozen TokenBuf"
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
  while true:
    case b[last].kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc last
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

proc writeFile*(b: TokenBuf; filename: string; mode: FileWriteMode = AlwaysWrite) =
  let content = "(.nif24)\n" & toString(b)
  if mode == OnlyIfChanged:
    let existingContent = try: readFile(filename) except: ""
    if existingContent == content: return
  writeFile(filename, content)

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

proc parseFromBuffer*(input: string; sizeHint = 100): TokenBuf =
  var r = nifstreams.openFromBuffer(input)
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

proc takeToken*(buf: var TokenBuf; n: var Cursor) {.inline.} =
  buf.add n
  inc n

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

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

import std / [assertions, syncio, hashes]
import nifreader, bitabs, stringviews, lineinfos, nifbuilder, vfs
export vfs.FileWriteMode

include compat2

# ─────────────────────────────────────────────────────────────────────────────
# Inlined former contents of `src/lib/nifstreams.nim`. These move here so the
# in-memory token representation can diverge (Phase 2 collapses ParLe + TagId
# into a single tag-kinded NifKind; Phase 3 adds count-to-close packing).
# Wire format and lexer (nifreader) are unchanged.
# ─────────────────────────────────────────────────────────────────────────────

export NifKind
type NivKind* = NifKind
  ## In-memory ("virtual") kind alias. Same enum values as the lexer's
  ## `NifKind` (incl. `ParLe`/`ParRi`), but conceptually marks tokens that
  ## have been re-encoded for in-memory traversal — e.g. `ParLe` tokens carry
  ## a packed `(tag | jump)` operand instead of a raw 28-bit tagId.
export NivKind

const
  InlineInt* = UnknownToken

type
  PackedToken* = object     # 8 bytes
    ## Bit layout of `x: uint32`
    ## ─────────────────────────
    ## bits 0..3   : kind (NivKind, fits in 4 bits — 13 values today)
    ## For kind == ParLe (tag tokens):
    ##   bits 4..12  : tag      (9 bits  — 512 values; tags.md has ~330)
    ##   bits 13..31 : jump     (19 bits — count of tokens until matching close;
    ##                           0 means "not sealed yet"; MaxJump means
    ##                           "doesn't fit, real ParRi follows")
    ## For all other kinds (atoms, ParRi):
    ##   bits 4..31  : operand  (28 bits — pool ID for atoms; 0 for ParRi)
    x: uint32
    info*: PackedLineInfo

const
  TokenKindBits = 4'u32
  TokenKindMask = (1'u32 shl TokenKindBits) - 1'u32
  ExcessK = 1'i32 shl (32 - TokenKindBits - 1)

  TagBits     = 9'u32
  TagShift*   = TokenKindBits                                     # 4
  TagMask     = (1'u32 shl TagBits) - 1'u32                       # 0x1FF
  JumpShift*  = TokenKindBits + TagBits                           # 13
  MaxJump*    = (1'u32 shl (32'u32 - JumpShift)) - 1'u32          # 0x7FFFF
    ## Sentinel: a tag whose subtree is too large for the 19-bit jump
    ## carries `jump = MaxJump`, and a real `ParRi` token is emitted at the
    ## matching position (Phase-3 overflow path).

template kind*(n: PackedToken): NivKind = cast[NivKind](n.x and TokenKindMask)
template uoperand*(n: PackedToken): uint32 = (n.x shr TokenKindBits)
  ## Raw 28-bit operand. Meaningful for atom kinds (returns the pool ID).
  ## For ParLe tokens, use `tagId`/`jump` accessors instead — the same bits
  ## are split (9 + 19) and `uoperand` will return the merged value.
template soperand*(n: PackedToken): int32 = cast[int32](uoperand(n))

template toX(k: NivKind; operand: uint32): uint32 =
  uint32(k) or (operand shl TokenKindBits)

proc int28Token*(operand: int32; info: PackedLineInfo): PackedToken =
  let arg = operand + ExcessK
  PackedToken(x: toX(UnknownToken, cast[uint32](arg)), info: info)

proc patchInt28Token*(n: var PackedToken; operand: int32) =
  let arg = operand + ExcessK
  n.x = toX(UnknownToken, cast[uint32](arg))

proc getInt28*(n: PackedToken): int32 =
  assert n.kind == UnknownToken
  let arg = n.soperand
  result = arg - ExcessK

proc toToken[L](kind: NivKind; id: L; info: PackedLineInfo): PackedToken {.inline.} =
  ## For atom kinds. ParLe tokens go through `tagToken` to encode (tag | jump).
  assert kind != ParLe, "use tagToken for ParLe tokens"
  PackedToken(x: toX(kind, uint32(id)), info: info)

proc parRiToken*(info: PackedLineInfo): PackedToken {.inline.} =
  PackedToken(x: toX(ParRi, 0'u32), info: info)

proc copyKeepLineInfo*(dest: var PackedToken; src: PackedToken) {.inline.} =
  dest.x = src.x

proc withLineInfo*(n: PackedToken; info: PackedLineInfo): PackedToken {.inline.} =
  result = n
  result.info = info

type
  StrId* = distinct uint32
  SymId* = distinct uint32
  IntId* = distinct uint32
  UIntId* = distinct uint32
  FloatId* = distinct uint32
  TagId* = distinct uint32
  Literals* = object
    man*: LineInfoManager
    tags*: BiTable[TagId, string]
    files*: BiTable[FileId, string] # we cannot use StringView here as it may have unexpanded backslashes!
    syms*: BiTable[SymId, string]
    strings*: BiTable[StrId, string]
    integers*: BiTable[IntId, int64]
    uintegers*: BiTable[UIntId, uint64]
    floats*: BiTableFloat[FloatId]

func `==`*(a, b: SymId): bool {.borrow.}
func `==`*(a, b: StrId): bool {.borrow.}
func `==`*(a, b: IntId): bool {.borrow.}
func `==`*(a, b: UIntId): bool {.borrow.}
func `==`*(a, b: FloatId): bool {.borrow.}
func `==`*(a, b: TagId): bool {.borrow.}

func hash*(x: SymId): Hash {.borrow.}
func hash*(x: StrId): Hash {.borrow.}
func hash*(x: IntId): Hash {.borrow.}
func hash*(x: UIntId): Hash {.borrow.}
func hash*(x: FloatId): Hash {.borrow.}
func hash*(x: TagId): Hash {.borrow.}

import ".." / models / tags

const
  Suffixed* = TagId SufTagId
  ErrT* = TagId ErrTagId

proc createLiterals(data: openArray[(string, int)]): Literals =
  result = default(Literals)
  for i in 1 ..< data.len:
    let t = result.tags.getOrIncl(data[i][0])
    assert t.int == data[i][1]

var pool* = createLiterals(TagData)

proc identToken*(s: StrId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(Ident, s, info)

proc symToken*(s: SymId; info: PackedLineInfo): PackedToken {.inline.} =
  assert s.uint32 > 0'u32
  toToken(Symbol, s, info)

proc dotToken*(info: PackedLineInfo): PackedToken {.inline.} =
  toToken(DotToken, 0'u32, info)

proc symdefToken*(s: SymId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(SymbolDef, s, info)

proc tagToken*(t: TagId; info: PackedLineInfo): PackedToken {.inline.} =
  ## Constructs a ParLe token with the given tag and `jump = 0` (unsealed).
  ## The jump field is filled in later by `setJump` (Phase 3 will do this
  ## inside `addParRi`).
  let tagBits = uint32(t) and TagMask
  assert uint32(t) == tagBits, "tag id " & $uint32(t) & " exceeds 9 bits"
  PackedToken(x: uint32(ParLe) or (tagBits shl TagShift), info: info)

proc intToken*(id: IntId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(IntLit, id, info)

proc uintToken*(id: UIntId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(UIntLit, id, info)

proc floatToken*(id: FloatId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(FloatLit, id, info)

proc charToken*(ch: char; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(CharLit, uint32(ch), info)

proc strToken*(id: StrId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(StringLit, id, info)

proc registerTag*(tag: string): TagId =
  ## Mostly useful for code generators like Nifgram.
  result = pool.tags.getOrIncl(tag)

template copyInto*(dest: var seq[PackedToken]; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add tagToken(tag, info)
  body
  dest.add parRiToken(info)

template copyIntoUnchecked*(dest: var seq[PackedToken]; tag: string; info: PackedLineInfo; body: untyped) =
  dest.add tagToken(pool.tags.getOrIncl(tag), info)  # was: pool.strings.getOrIncl (likely a bug pre-Phase-2)
  body
  dest.add parRiToken(info)

type
  Stream* = object
    r*: Reader
    parents*: seq[PackedLineInfo]

proc open*(filename: string): Stream =
  result = Stream(r: nifreader.open(filename))
  result.parents.add NoLineInfo

proc openFromBuffer*(buf: sink string; thisModule: sink string): Stream =
  result = Stream(r: nifreader.openFromBuffer(buf, thisModule))
  result.parents.add NoLineInfo

proc close*(s: var Stream) = nifreader.close(s.r)

proc rawNext(s: var Stream; t: ExpandedToken): PackedToken =
  var currentInfo = NoLineInfo
  let commentId =
    if t.comment.len == 0: 0'u32
    else: pool.strings.getOrIncl(decodeComment t).uint32
  if t.filename.len == 0:
    # relative file position
    if t.pos.line != 0 or t.pos.col != 0 or commentId != 0'u32:
      let rawInfo = unpack(pool.man, s.parents[^1])
      currentInfo = packWithComment(pool.man, rawInfo.file,
                                    rawInfo.line+t.pos.line,
                                    rawInfo.col+t.pos.col, commentId)
    else:
      currentInfo = s.parents[^1]
  else:
    # absolute file position:
    let fileId = pool.files.getOrIncl(decodeFilename t)
    currentInfo = packWithComment(pool.man, fileId, t.pos.line, t.pos.col, commentId)

  case t.tk
  of ParRi:
    result = parRiToken(currentInfo)
    if s.parents.len > 1:
      discard s.parents.pop()
  of EofToken, UnknownToken, DotToken:
    result = toToken(t.tk, 0'u32, currentInfo)
  of ParLe:
    let ka = pool.tags.getOrInclFromView(t.data)
    result = tagToken(ka, currentInfo)
    # Children resolve relative positions against the parent's location, not
    # against its decorations — strip any `#comment#` before pushing.
    s.parents.add stripComment(pool.man, currentInfo)
  of Ident, StringLit:
    result = toToken(t.tk, pool.strings.getOrIncl(s.r.decodeStr t), currentInfo)
  of Symbol, SymbolDef:
    result = toToken(t.tk, pool.syms.getOrIncl(s.r.decodeStr t), currentInfo)
  of CharLit:
    result = toToken(CharLit, uint32 decodeChar(t), currentInfo)
  of IntLit:
    result = toToken(IntLit, pool.integers.getOrIncl(decodeInt t), currentInfo)
  of UIntLit:
    result = toToken(UIntLit, pool.uintegers.getOrIncl(decodeUInt t), currentInfo)
  of FloatLit:
    result = toToken(FloatLit, pool.floats.getOrIncl(decodeFloat t), currentInfo)

proc next*(s: var Stream): PackedToken =
  var t = default(ExpandedToken)
  next(s.r, t)
  result = rawNext(s, t)

proc skip*(s: var Stream; current: PackedToken): PackedToken =
  if current.kind == ParLe:
    # jump to corresponding ParRi:
    var nested = 0
    while true:
      var t = default(ExpandedToken)
      next(s.r, t)
      if t.tk == ParLe: inc nested
      elif t.tk == ParRi:
        if nested == 0: break
        dec nested
  result = next(s)

proc litId*(n: PackedToken): StrId {.inline.} =
  assert n.kind in {Ident, StringLit}
  StrId(n.uoperand)

proc charLit*(n: PackedToken): char {.inline.} =
  assert n.kind == CharLit
  char(n.uoperand)

proc symId*(n: PackedToken): SymId {.inline.} =
  assert n.kind in {Symbol, SymbolDef}
  SymId(n.uoperand)

proc setSymId*(dest: var PackedToken; sym: SymId) {.inline.} =
  let k = dest.kind
  assert k in {Symbol, SymbolDef}
  dest.x = toX(k, uint32 sym)

proc intId*(n: PackedToken): IntId {.inline.} =
  assert n.kind == IntLit
  IntId(n.uoperand)

proc uintId*(n: PackedToken): UIntId {.inline.} =
  assert n.kind == UIntLit
  UIntId(n.uoperand)

proc floatId*(n: PackedToken): FloatId {.inline.} =
  assert n.kind == FloatLit
  FloatId(n.uoperand)

proc tagId*(n: PackedToken): TagId {.inline.} =
  ## Extracts the 9-bit tag from a ParLe token's packed `(tag | jump)` operand.
  assert n.kind == ParLe, $n.kind
  TagId((n.x shr TagShift) and TagMask)

proc tag*(n: PackedToken): TagId {.inline.} =
  if n.kind == ParLe: result = n.tagId
  else: result = ErrT

proc jump*(n: PackedToken): uint32 {.inline.} =
  ## Extracts the 19-bit "tokens until matching close" field from a ParLe.
  ## Returns 0 when the count has not been sealed yet (Phase 2 leaves all
  ## jumps at 0; Phase 3 fills them in inside `addParRi`).
  assert n.kind == ParLe, $n.kind
  n.x shr JumpShift

proc setJump*(n: var PackedToken; j: uint32) {.inline.} =
  ## Patches the jump field of an existing ParLe token. Asserts that the
  ## value fits in 19 bits — callers that detect overflow must store
  ## `MaxJump` and emit a real `ParRi` token themselves.
  assert n.kind == ParLe, $n.kind
  assert j <= MaxJump, "jump " & $j & " exceeds 19 bits; caller must use MaxJump sentinel"
  let preserved = n.x and ((1'u32 shl JumpShift) - 1'u32)  # kind + tag bits
  n.x = preserved or (j shl JumpShift)

proc typebits*(n: PackedToken): int =
  if n.kind == IntLit:
    result = int pool.integers[n.intId]
  elif n.kind == InlineInt:
    result = n.soperand
  else:
    result = 0

proc emitLineInfo(b: var Builder; info, parentInfo: PackedLineInfo) =
  ## Append the NIF27 line-info suffix for `info` (relative to `parentInfo` if
  ## both share a file) to whatever atom or tag was just written into `b`.
  ## Also re-emits any `#comment#` attached to `info`.
  let rawInfo = unpack(pool.man, info)
  let file = rawInfo.file
  var line = rawInfo.line
  var col = rawInfo.col
  if file.isValid:
    var fileAsStr = ""
    if parentInfo.isValid:
      let pRawInfo = unpack(pool.man, parentInfo)
      if file != pRawInfo.file: fileAsStr = pool.files[file]
      if fileAsStr.len == 0:
        line = line - pRawInfo.line
        col = col - pRawInfo.col
    else:
      fileAsStr = pool.files[file]
    b.attachLineInfo(col, line, fileAsStr)
  if rawInfo.comment != 0'u32:
    b.attachComment pool.strings[StrId(rawInfo.comment)]

type
  RenderFrame = object
    closeAt: int                ## buffer index where this scope's virtual
                                ## close fires (`-1` for an overflow scope
                                ## whose close is the next real ParRi).
    parentInfo: PackedLineInfo  ## info of the enclosing tag — used to
                                ## resolve children's relative line-info.

template flushVirtualCloses(stack: var seq[RenderFrame]; b: var Builder; n: int) =
  ## Pops every frame whose virtual close fires at index `n`. Sealed scopes
  ## have no real ParRi token, so their `endTree()` must be emitted lazily
  ## as we cross the matching position.
  while stack.len > 0 and stack[^1].closeAt == n:
    b.endTree()
    discard stack.pop()

proc toString*(tree: openArray[PackedToken]; produceLineInfo = true): string =
  var b = nifbuilder.open(tree.len * 20)
  var stack: seq[RenderFrame] = @[]
  var n = 0
  while n < tree.len:
    flushVirtualCloses(stack, b, n)
    if n >= tree.len: break
    let info = tree[n].info
    let k = tree[n].kind
    case k
    of DotToken:
      b.addEmpty()
    of Ident:
      b.addIdent(pool.strings[tree[n].litId])
    of Symbol:
      b.addSymbol(pool.syms[tree[n].symId])
    of IntLit:
      b.addIntLit(pool.integers[tree[n].intId])
    of UIntLit:
      b.addUIntLit(pool.uintegers[tree[n].uintId])
    of FloatLit:
      b.addFloatLit(pool.floats[tree[n].floatId])
    of SymbolDef:
      b.addSymbolDef(pool.syms[tree[n].symId])
    of CharLit:
      b.addCharLit char(tree[n].uoperand)
    of StringLit:
      b.addStrLit(pool.strings[tree[n].litId])
    of UnknownToken:
      b.addIdent "<unknown token>"
    of EofToken:
      b.addIntLit tree[n].soperand
    of ParRi:
      when defined(preserveRealParRi):
        # Sealed scope's real ParRi was already handled by flushVirtualCloses
        # at the top of this iteration; only an overflow scope still has a
        # frame to close here.
        if stack.len > 0 and stack[^1].closeAt == -1:
          b.endTree()
          discard stack.pop()
        # else: redundant, swallow.
      else:
        assert stack.len > 0 and stack[^1].closeAt == -1, "stray real ParRi"
        b.endTree()
        discard stack.pop()
    of ParLe:
      b.addTree(pool.tags[tree[n].tagId])
    if produceLineInfo and info.isValid and k != ParRi:
      emitLineInfo(b, info, if stack.len > 0: stack[^1].parentInfo else: NoLineInfo)
    if k == ParLe:
      let j = jump(tree[n])
      let closeAt = if j == MaxJump: -1 else: n + 1 + int(j)
      stack.add RenderFrame(closeAt: closeAt, parentInfo: info)
    inc n
  # End-of-buffer flush: close any still-open virtual scopes (root tag, etc.).
  while stack.len > 0:
    b.endTree()
    discard stack.pop()
  result = b.extract()

proc toModuleString*(tree: openArray[PackedToken]; dottedSuffix = ""; produceLineInfo = true): string =
  ## Like `toString` but produces a full file including the header and an index.
  var b = nifbuilder.open(tree.len * 20)
  let patchPos = b.addHeader27()
  var stack: seq[RenderFrame] = @[]
  var mostRecentOffset = 0
  var previousOffset = 0
  var index = nifbuilder.open(tree.len * 2)
  index.addTree ".index"
  if tree.len > 0:
    index.emitLineInfo(tree[0].info, NoLineInfo)
  var n = 0
  while n < tree.len:
    flushVirtualCloses(stack, b, n)
    if n >= tree.len: break
    let info = tree[n].info
    let k = tree[n].kind
    case k
    of DotToken:
      b.addEmpty()
    of Ident:
      b.addIdent(pool.strings[tree[n].litId])
    of Symbol:
      b.addSymbol(pool.syms[tree[n].symId], dottedSuffix)
    of IntLit:
      b.addIntLit(pool.integers[tree[n].intId])
    of UIntLit:
      b.addUIntLit(pool.uintegers[tree[n].uintId])
    of FloatLit:
      b.addFloatLit(pool.floats[tree[n].floatId])
    of SymbolDef:
      let symId = tree[n].symId
      if b.addSymbolDefRetIsGlobal(pool.syms[symId], dottedSuffix):
        if n+1 >= tree.len or (tree[n+1].kind == DotToken):
          index.addTree "h" # no export marker --> hidden
        else:
          index.addTree "x" # export marker --> exported
        let parentInfo = if stack.len >= 2: stack[^2].parentInfo else: tree[0].info
        emitLineInfo(index, parentInfo, tree[0].info)
        index.addSymbol(pool.syms[symId], dottedSuffix)
        index.addIntLit(mostRecentOffset - previousOffset)
        previousOffset = mostRecentOffset
        index.endTree()
    of CharLit:
      b.addCharLit char(tree[n].uoperand)
    of StringLit:
      b.addStrLit(pool.strings[tree[n].litId])
    of UnknownToken:
      b.addIdent "<unknown token>"
    of EofToken:
      b.addIntLit tree[n].soperand
    of ParRi:
      when defined(preserveRealParRi):
        if stack.len > 0 and stack[^1].closeAt == -1:
          b.endTree()
          discard stack.pop()
      else:
        assert stack.len > 0 and stack[^1].closeAt == -1, "stray real ParRi"
        b.endTree()
        discard stack.pop()
    of ParLe:
      mostRecentOffset = b.offset
      b.addTree(pool.tags[tree[n].tagId])
    if produceLineInfo and info.isValid and k != ParRi:
      emitLineInfo(b, info, if stack.len > 0: stack[^1].parentInfo else: NoLineInfo)
    if k == ParLe:
      let j = jump(tree[n])
      let closeAt = if j == MaxJump: -1 else: n + 1 + int(j)
      stack.add RenderFrame(closeAt: closeAt, parentInfo: info)
    inc n
  while stack.len > 0:
    b.endTree()
    discard stack.pop()

  b.patchIndexAt(patchPos, b.offset)
  result = b.extract()
  index.endTree()
  result.add index.extract()

# ─────────────────────────────────────────────────────────────────────────────
# End of inlined nifstreams content. Cursor / TokenBuf APIs follow.
# ─────────────────────────────────────────────────────────────────────────────

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
  ErrToken = [tagToken(ErrT, NoLineInfo),
              parRiToken(NoLineInfo)]

proc errCursor*(): Cursor =
  Cursor(p: addr ErrToken[0], rem: 2)

proc fromBuffer*(x: openArray[PackedToken]): Cursor {.inline.} =
  Cursor(p: addr(x[0]), rem: x.len)

proc setSpan*(c: var Cursor; beyondLast: Cursor) {.inline.} =
  c.rem = (cast[int](beyondLast.p) - cast[int](c.p)) div sizeof(PackedToken)

proc load*(c: Cursor): PackedToken {.inline.} =
  ## With scope-rem semantics, `rem == 0` is the legitimate "at the scope's
  ## (virtual) close" position — `c.p` still points at the parent's next
  ## token (or a real ParRi for overflow / preserve mode), so loading is
  ## fine. Only `c.p == nil` is a real error.
  assert c.p != nil
  c.p[]

proc kind*(c: Cursor): NifKind {.inline.} = c.load.kind

proc info*(c: Cursor): PackedLineInfo {.inline.} = c.load.info
proc setInfo*(c: Cursor; info: PackedLineInfo) {.inline.} = c.p[].info = info

proc litId*(c: Cursor): StrId {.inline.} = litId(c.load)
proc symId*(c: Cursor): SymId {.inline.} = symId(c.load)

proc charLit*(c: Cursor): char {.inline.} = charLit(c.load)

proc intId*(c: Cursor): IntId {.inline.} = intId(c.load)
proc uintId*(c: Cursor): UIntId {.inline.} = uintId(c.load)
proc floatId*(c: Cursor): FloatId {.inline.} = floatId(c.load)
proc tagId*(c: Cursor): TagId {.inline.} = tagId(c.load)

proc tag*(c: Cursor): TagId {.inline.} = tag(c.load)

proc uoperand*(c: Cursor): uint32 {.inline.} = uoperand(c.load)
proc soperand*(c: Cursor): int32 {.inline.} = soperand(c.load)

proc getInt28*(c: Cursor): int32 {.inline.} = getInt28(c.load)

## Cursor `rem` field in Phase 3
## ─────────────────────────────
## `rem` is repurposed from "tokens remaining in buffer" to **tokens remaining
## in the current scope** (or root buffer at the outermost level).
##
##  - `rem > 0`  : more children remain in the current scope.
##  - `rem == 0` : at the (virtual) end of the current scope. `inc` is illegal;
##                 use the `into` epilogue to restore the outer scope's `rem`.
##  - `rem == -1`: scope is *unbounded* — either the root buffer (we don't
##                 know how many tokens until EOF) or an overflow `tagToken`
##                 whose subtree is too large for the 19-bit jump field.
##                 In this case `hasMore` falls back to `c.kind != ParRi`
##                 (the real ParRi terminates the scope).
##
## `inc` decrements `rem` only when `rem > 0`; for `rem == -1` it is left
## alone. `hasCurrentToken` is gone — it conflated "tokens in buffer" with
## "tokens in scope" and shouldn't have a Phase-3 equivalent.

proc inc*(c: var Cursor) {.inline.} =
  assert c.rem != 0, "advancing past end of scope"
  c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
  if c.rem > 0: dec c.rem

proc unsafeDec*(c: var Cursor) {.inline.} =
  c.p = cast[ptr PackedToken](cast[uint](c.p) - sizeof(PackedToken).uint)
  if c.rem >= 0: inc c.rem

proc `+!`*(c: Cursor; diff: int): Cursor {.inline.} =
  ## Advances the cursor by `diff` tokens. If `c` is bounded (`c.rem >= 0`),
  ## subtracts `diff` from the bound (clamped at 0); if `c` is unbounded
  ## (`c.rem == -1`), stays unbounded.
  let newRem =
    if c.rem == -1: -1
    elif c.rem >= diff: c.rem - diff
    else: 0
  result = Cursor(owner: c.owner,
     p: cast[ptr PackedToken](cast[uint](c.p) + diff.uint * sizeof(PackedToken).uint),
     rem: newRem)
  if result.owner != nil: inc result.owner.rc

proc cursorIsNil*(c: Cursor): bool {.inline.} =
  result = c.p == nil

template subtreeSpan(n: PackedToken): uint32 =
  ## Number of tokens that `skip` past this token would consume (including
  ## this token itself). For a sealed tag (`jump < MaxJump`) this is `1 + jump`
  ## under default elision, or `2 + jump` under `-d:preserveRealParRi` (where
  ## the matching real ParRi is also stored). Overflow tags (`jump == MaxJump`)
  ## return 0 to signal "caller takes the slow path".
  case n.kind
  of ParLe:
    let j = jump(n)
    if j == MaxJump: 0'u32
    else:
      when defined(preserveRealParRi): 2'u32 + j
      else: 1'u32 + j
  else: 1'u32

proc skip*(c: var Cursor) =
  ## Advances `c` past the current subtree (a single token or a whole
  ## `(...)`). For sealed tags this is an O(1) jump; for overflow tags
  ## (`jump == MaxJump`) it falls back to a balanced walk that recognises
  ## both real ParRi and nested sealed tags.
  if c.kind == ParLe:
    let span = subtreeSpan(c.load)
    if span > 0:
      # Sealed tag: jump the whole span in one shot.
      c.p = cast[ptr PackedToken](cast[uint](c.p) + uint(span) * sizeof(PackedToken).uint)
      if c.rem > 0:
        let s = int(span)
        c.rem = if c.rem >= s: c.rem - s else: 0
      return
    # Overflow tag: balanced walk. Real ParRi balances the (overflow) ParLe;
    # nested sealed tags are skipped via their own jump fields.
    var depth = 1
    inc c   # past overflow tag
    while depth > 0:
      if c.kind == ParRi:
        dec depth
        inc c
      elif c.kind == ParLe:
        let innerSpan = subtreeSpan(c.load)
        if innerSpan > 0:
          c.p = cast[ptr PackedToken](cast[uint](c.p) + uint(innerSpan) * sizeof(PackedToken).uint)
          if c.rem > 0:
            let s = int(innerSpan)
            c.rem = if c.rem >= s: c.rem - s else: 0
        else:
          inc depth
          inc c
      else:
        inc c
  else:
    inc c

proc skipToEnd*(c: var Cursor) =
  ## Skip all remaining children of the current scope, then consume the
  ## scope's close (real or virtual). After this `c` is positioned at the
  ## next sibling of the scope's parent.
  while c.rem > 0:
    skip c
  if c.rem == -1 and c.kind == ParRi:
    inc c

proc skipUntilEnd*(c: var Cursor) =
  ## Like `skipToEnd` but leaves the cursor at the scope's close (does not
  ## consume it). For sealed scopes this is exactly the position of the
  ## parent's next child.
  while c.rem > 0:
    skip c

# ── Traversal templates ──────────────────────────────────────────────────
# Pure traversal helpers for reading/analyzing a tree without producing output.

template hasMore*(c: Cursor): bool =
  ## True while there are more children before the (real or virtual) close.
  ## For bounded scopes (`rem >= 0`) this is the cheap `rem > 0` check;
  ## for unbounded scopes (`rem == -1`, root or overflow tag) we fall back
  ## to recognising the real ParRi token.
  (c.rem > 0) or (c.rem < 0 and c.kind != ParRi)

template into*(c: var Cursor; body: untyped) =
  ## Enters the current ParLe node, runs `body` to process the children,
  ## then advances past the (virtual or real) close.
  ##
  ## The body must consume exactly the scope's children and leave `c.rem == 0`
  ## (sealed scope) or `c.kind == ParRi` (overflow scope). `loopInto`'s
  ## `while c.hasMore: body` is the canonical shape that satisfies this.
  assert c.kind == ParLe, "into requires cursor at ParLe"
  let savedRem = c.rem
  let savedP   = c.p
  let j        = jump(c.load)
  let isOverflow = j == MaxJump
  # advance past the tag without touching rem (we're about to overwrite it)
  c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
  c.rem = if isOverflow: -1 else: int(j)
  body
  if isOverflow:
    assert c.kind == ParRi, "into: overflow scope did not end at real ParRi"
    c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
  else:
    assert c.rem == 0, "into: body did not consume all children of sealed scope"
    when defined(preserveRealParRi):
      # In preserve mode the buffer keeps real ParRi tokens even for sealed
      # scopes; eat the matching one so the outer cursor lands at the next
      # sibling (matches both bracket-balanced and into-based callers).
      if c.kind == ParRi:
        c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
  # Restore outer rem from token-consumption pointer arithmetic.
  if savedRem >= 0:
    let consumed = int((cast[uint](c.p) - cast[uint](savedP)) div sizeof(PackedToken).uint)
    c.rem = if savedRem >= consumed: savedRem - consumed else: 0
  else:
    c.rem = -1

template loopInto*(c: var Cursor; body: untyped) =
  ## Enters a node, iterates all children, then advances past the close.
  ## The body must advance `c` on every iteration (typically by emitting one
  ## child via `take*` / `skip` / a recursive `loopInto`).
  into c:
    while c.hasMore:
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
    openTags: seq[int]  ## positions of unsealed `tagToken`s, pushed by
                        ## `add(ParLe)` and popped by `add(ParRi)` /
                        ## `addParRi`. Lets `addParRi` find its matching
                        ## opening in O(1) so it can seal the jump and elide
                        ## the real ParRi when the count fits.
    owner: CursorOwner  ## nil = exclusive ownership of data.
                        ## non-nil = data is shared via CursorOwner with Cursors;
                        ## b.data aliases owner.data. TokenBuf holds one rc ref.

proc `=copy`(dest: var TokenBuf; src: TokenBuf) {.error.}
proc `=wasMoved`(dest: var TokenBuf) {.inline.} =
  dest.data = nil
  dest.len = 0
  dest.cap = 0
  dest.owner = nil
  # `openTags` is a `seq[int]` — Nim auto-handles its move/destroy lifecycle.

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

proc reserveOne(b: var TokenBuf) {.inline.} =
  if b.owner != nil: prepareMutation(b)
  if b.len >= b.cap:
    b.cap = max(b.cap div 2 + b.cap, 8)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))

proc addRaw*(b: var TokenBuf; item: PackedToken) {.inline.} =
  ## Append a single token without touching the open-tags stack. Use this
  ## when bulk-copying an already-sealed subtree (where opens and closes are
  ## already paired inside the copied span).
  reserveOne(b)
  b.data[b.len] = item
  inc b.len

proc add*(b: var TokenBuf; item: PackedToken) {.inline.} =
  ## Single-token append. ParLe pushes to `openTags`; ParRi pops, seals the
  ## matching tag's jump field, and (by default) elides the real ParRi when
  ## the count fits in 19 bits.
  ##
  ## Build with `-d:preserveRealParRi` to keep emitting real `)` tokens even
  ## for sealed scopes. The jump field is still set, so cursor `into`/`skip`
  ## use the fast path; but bracket-balanced traversals (`while n.kind !=
  ## ParRi`) still terminate. Use this while porting tools whose code base
  ## hasn't been migrated to `into`/`hasMore` yet.
  case item.kind
  of ParLe:
    reserveOne(b)
    b.data[b.len] = item
    b.openTags.add b.len
    inc b.len
  of ParRi:
    var elide = false
    if b.openTags.len > 0:
      let openPos = b.openTags.pop()
      let count = uint32(b.len - openPos - 1)
      if count < MaxJump:
        b.data[openPos].setJump count
        elide = true
      else:
        b.data[openPos].setJump MaxJump
    when defined(preserveRealParRi):
      reserveOne(b)
      b.data[b.len] = item
      inc b.len
    else:
      if elide: return  # virtual close — emit nothing
      reserveOne(b)
      b.data[b.len] = item
      inc b.len
  else:
    reserveOne(b)
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

proc bulkCopy(dest: var TokenBuf; src: pointer; n: int) =
  ## Append `n` raw tokens to `dest` without touching `openTags`. Use only
  ## when the copied span is internally balanced (sealed scopes from another
  ## TokenBuf, or a closed subtree from a Cursor).
  if n == 0: return
  if dest.owner != nil: prepareMutation(dest)
  let needed = dest.len + n
  if needed > dest.cap:
    dest.cap = max(dest.cap div 2 + dest.cap, needed)
    dest.data = cast[Storage](realloc(dest.data, sizeof(PackedToken)*dest.cap))
  copyMem(addr dest.data[dest.len], src, n * sizeof(PackedToken))
  dest.len += n

proc addSubtree*(result: var TokenBuf; c: Cursor) =
  ## Bulk-copies the subtree at `c` into `result`. The copied span is
  ## internally balanced (sealed jumps + any real ParRi for overflow scopes
  ## are preserved verbatim), so `openTags` is left alone.
  assert c.kind != ParRi, "cursor at end?"
  if c.kind != ParLe:
    result.addRaw c.load
    return
  var c2 = c
  let startP = c2.p
  skip c2
  let n = int((cast[uint](c2.p) - cast[uint](startP)) div sizeof(PackedToken).uint)
  bulkCopy(result, startP, n)

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
  ## Bulk-copies `src` into `dest` (both opens and closes inside `src` are
  ## already paired, so `openTags` is left alone).
  if src.len == 0: return
  bulkCopy(dest, src.data, src.len)

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
  # Drop any open-tag positions that are now past the end.
  while b.openTags.len > 0 and b.openTags[^1] >= newLen:
    discard b.openTags.pop()

proc grow(b: var TokenBuf; newLen: int) =
  if b.owner != nil: prepareMutation(b)
  assert newLen > b.len
  if b.cap < newLen:
    b.cap = max(b.cap div 2 + b.cap, newLen)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.len = newLen

template buildTree*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add tagToken(tag, info)
  body
  dest.addParRi(info)

proc addParLe*(dest: var TokenBuf; tag: TagId; info = NoLineInfo) =
  dest.add tagToken(tag, info)

proc addParRi*(dest: var TokenBuf) =
  ## Sealing happens inside `add(parRiToken(...))` via the `openTags` stack.
  dest.add parRiToken(NoLineInfo)

proc addParRi*(dest: var TokenBuf; info: PackedLineInfo) =
  dest.add parRiToken(info)

proc addParRi*(dest: var seq[PackedToken]; info: PackedLineInfo) =
  ## seq[PackedToken] variant — used by tests/inlined constructors that build
  ## a fragment before wrapping it in a TokenBuf. Currently keeps the legacy
  ## "real ParRi" emission since seq[] doesn't carry the metadata `setJump`
  ## needs (and these fragments are normally tiny throwaways).
  dest.add parRiToken(info)

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
  result = toString(toOpenArray(b.data, 0, b.len-1), produceLineInfo)

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
  result = toString(toOpenArray(b.data, first, last), produceLineInfo)

proc toString*(b: Cursor; produceLineInfo = true): string =
  let counter = span(b)
  result = toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, counter-1), produceLineInfo)

proc toStringDebug*(b: Cursor; produceLineInfo = true): string =
  let L = if b.kind == ParLe: 1 else: 0
  result = toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, L), produceLineInfo)

proc writeFile*(b: TokenBuf; filename: string; mode: FileWriteMode = AlwaysWrite) {.canRaise.} =
  let content = toModuleString(toOpenArray(b.data, 0, b.len-1), "." & extractModuleSuffix(filename))
  if mode == OnlyIfChanged:
    let existingContent = try: vfsRead(filename) except: ""
    if existingContent == content: return
  vfsWrite(filename, content)

proc `$`*(c: Cursor): string = toString(c, false)

template copyInto*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add tagToken(tag, info)
  body
  dest.addParRi()

proc tagTokenUnchecked*(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  tagToken(pool.tags.getOrIncl(tag), info)

template copyIntoUnchecked*(dest: var TokenBuf; tag: string; info: PackedLineInfo; body: untyped) =
  dest.add tagTokenUnchecked(tag, info)
  body
  dest.addParRi()

proc parse*(r: var Stream; dest: var TokenBuf;
            parentInfo: PackedLineInfo; debug: bool = false) =
  ## Read tokens from `r` into `dest`. `parentInfo` seeds the parent line-info
  ## stack so that the first token read resolves its diff against the correct
  ## source-tree parent. `parseFromFile`/`parseFromBuffer` pass `NoLineInfo`;
  ## index-jumped reads pass the indexed compound's *parent* info (recorded
  ## by `toModuleString`'s SymbolDef branch — see the comment there).
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
  var r = openFromBuffer(input, thisModule)
  result = createTokenBuf(sizeHint)
  parse(r, result, NoLineInfo)

proc parseFromFile*(filename: string; sizeHint = 100): TokenBuf =
  var r = open(filename)
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
      let by2 = [tagToken(TagId 1, NoLineInfo), parRiToken(NoLineInfo)]
      replace dest, fromBuffer(by2), 0
      assert dest.len == 2
      assert dest[0] == by2[0] and dest[1] == by2[1]

      let by1 = [strToken(StrId 2, NoLineInfo)]
      replace dest, fromBuffer(by1), 0
      assert dest.len == 1
      assert dest[0] == by1[0]
  block:
    var dest = createTokenBuf(3)
    let dest0 = tagToken(TagId 123, NoLineInfo)
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

      let by2 = [tagToken(TagId 456, NoLineInfo), parRiToken(NoLineInfo)]
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

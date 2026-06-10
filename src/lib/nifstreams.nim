#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A NIF stream is simply a seq of tokens. It turns out to be useful
## for many different cases.

import std / [assertions, hashes]

import bitabs, stringviews, lineinfos, nifreader, nifbuilder

export NifKind

const
  InlineInt* = UnknownToken

type
  PackedToken* = object     # 8 bytes
    x: uint32
    info*: PackedLineInfo

const
  TokenKindBits = 4'u32
  TokenKindMask = (1'u32 shl TokenKindBits) - 1'u32
  ExcessK = 1'i32 shl (32 - TokenKindBits - 1)

  # Under `-d:virtualParRi` the ParLe operand is split into a 9-bit tag and
  # a 19-bit "jump" field counting tokens until the matching close. Phase 3
  # uses this to elide trailing `ParRi` tokens whose count fits in 19 bits.
  # Without the flag the operand stays a 28-bit tagId — nothing changes.
  TagBits     = 9'u32
  TagShift*   = TokenKindBits                                     # 4
  TagMask     = (1'u32 shl TagBits) - 1'u32                       # 0x1FF
  JumpShift*  = TokenKindBits + TagBits                           # 13
  NaturalMaxJump = (1'u32 shl (32'u32 - JumpShift)) - 1'u32       # 0x7FFFF

const
  NifJumpCap*: int = int(NaturalMaxJump)
    ## Test/stress knob (only meaningful under `-d:virtualParRi`): lower
    ## with `-d:nifJumpCap=N` to artificially shrink the elision window so
    ## any subtree of ≥ N body tokens hits the real-ParRi overflow path.
    ## E.g. `-d:nifJumpCap=3` makes only trivial subtrees use virtual ParRi.

const
  MaxJump* = uint32(NifJumpCap)
    ## Sentinel: a tag whose subtree is too large carries `jump = MaxJump`,
    ## and a real `ParRi` token is emitted at the matching position.

template kind*(n: PackedToken): NifKind = cast[NifKind](n.x and TokenKindMask)
template uoperand*(n: PackedToken): uint32 = (n.x shr TokenKindBits)
template soperand*(n: PackedToken): int32 = cast[int32](uoperand(n))

template toX(k: NifKind; operand: uint32): uint32 =
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

proc toToken[L](kind: NifKind; id: L; info: PackedLineInfo): PackedToken {.inline.} =
  PackedToken(x: toX(kind, uint32(id)), info: info)

proc parRiToken*(info: PackedLineInfo): PackedToken {.inline.} =
  PackedToken(x: toX(ParRi, 0'u32), info: info)

proc addToken[L](tree: var seq[PackedToken]; kind: NifKind; id: L; info: PackedLineInfo) =
  tree.add PackedToken(x: toX(kind, uint32(id)), info: info)

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

proc parLeToken*(t: TagId; info: PackedLineInfo): PackedToken {.inline.} =
  when defined(virtualParRi):
    let tagBits = uint32(t) and TagMask
    assert uint32(t) == tagBits, "tag id " & $uint32(t) & " exceeds 9 bits"
    PackedToken(x: uint32(ParLe) or (tagBits shl TagShift), info: info)
  else:
    toToken(ParLe, t, info)

proc tagToken*(t: TagId; info: PackedLineInfo): PackedToken {.inline.} =
  ## Alias for `parLeToken` (the nifprims spelling).
  parLeToken(t, info)

proc jump*(n: PackedToken): uint32 {.inline.} =
  ## Extract the 19-bit "tokens until matching close" field from a ParLe.
  ## Returns 0 for unsealed scopes; under `-d:virtualParRi` this is filled
  ## in by `setJump` on the matching `ParRi`. Without the flag this is
  ## always 0 — the value is meaningless and `MaxJump` should be used.
  assert n.kind == ParLe, $n.kind
  when defined(virtualParRi):
    n.x shr JumpShift
  else:
    MaxJump  # always overflow → callers fall back to balanced walks

proc setJump*(n: var PackedToken; j: uint32) {.inline.} =
  ## Patch the jump field of an existing ParLe. No-op without
  ## `-d:virtualParRi` (no jump field to patch).
  assert n.kind == ParLe, $n.kind
  when defined(virtualParRi):
    assert j <= MaxJump, "jump " & $j & " exceeds 19 bits"
    let preserved = n.x and ((1'u32 shl JumpShift) - 1'u32)  # kind + tag bits
    n.x = preserved or (j shl JumpShift)

proc setTag*(n: var PackedToken; t: TagId) {.inline.} =
  ## Patch the tag of an existing ParLe in place, preserving any
  ## already-sealed `jump`. Use this instead of `n = parLeToken(t, info)`
  ## when retagging a ParLe in a buffer that has already been sealed —
  ## `parLeToken` resets jump to 0.
  assert n.kind == ParLe, $n.kind
  when defined(virtualParRi):
    let tagBits = uint32(t) and TagMask
    assert uint32(t) == tagBits, "tag id " & $uint32(t) & " exceeds 9 bits"
    n.x = (n.x and not (TokenKindMask or (TagMask shl TagShift))) or
          uint32(ParLe) or (tagBits shl TagShift)
  else:
    n = parLeToken(t, n.info)

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
  dest.addToken ParLe, tag, info
  body
  dest.addToken ParRi, 0'u32, info

template copyIntoUnchecked*(dest: var seq[PackedToken]; tag: string; info: PackedLineInfo; body: untyped) =
  dest.addToken ParLe, pool.strings.getOrIncl(tag), info
  body
  dest.addToken ParRi, 0'u32, info

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
    result = toToken(t.tk, 0'u32, currentInfo)
    if s.parents.len > 1:
      discard s.parents.pop()
  of EofToken, UnknownToken, DotToken:
    result = toToken(t.tk, 0'u32, currentInfo)
  of ParLe:
    let ka = pool.tags.getOrInclFromView(t.data)
    result = toToken(ParLe, ka, currentInfo)
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
  assert n.kind == ParLe, $n.kind
  when defined(virtualParRi):
    TagId((n.x shr TagShift) and TagMask)
  else:
    TagId(n.uoperand)

proc tag*(n: PackedToken): TagId {.inline.} =
  if n.kind == ParLe: result = n.tagId
  else: result = ErrT

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

proc toString*(tree: openArray[PackedToken]; produceLineInfo = true): string =
  var b = nifbuilder.open(tree.len * 20)
  var stack: seq[PackedLineInfo] = @[]
  when defined(virtualParRi):
    # Sealed scopes have their closing `)` elided from the buffer; reconstruct
    # it from the jump field. `endsAt[k]` is the index of the last token of an
    # open sealed scope, after which its synthetic `)` must be emitted.
    var endsAt: seq[int] = @[]
  for n in 0 ..< tree.len:
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
      if stack.len > 0:
        discard stack.pop()
      b.endTree()
    of ParLe:
      b.addTree(pool.tags[tree[n].tagId])
    # NIF27: line info is a postfix suffix on the atom or tag we just wrote.
    if produceLineInfo and info.isValid and k != ParRi:
      emitLineInfo(b, info, if stack.len > 0: stack[^1] else: NoLineInfo)
    if k == ParLe:
      stack.add info
      when defined(virtualParRi):
        let j = jump(tree[n])
        if j != MaxJump: endsAt.add(n + int(j))
    when defined(virtualParRi):
      # Emit synthetic closers for every sealed scope ending at this token.
      while endsAt.len > 0 and endsAt[^1] == n:
        discard endsAt.pop()
        if stack.len > 0: discard stack.pop()
        b.endTree()
  result = b.extract()

proc toModuleString*(tree: openArray[PackedToken]; dottedSuffix = ""; produceLineInfo = true): string =
  ## Like `toString` but produces a full file including the header and an index.
  var b = nifbuilder.open(tree.len * 20)
  let patchPos = b.addHeader27()
  var stack: seq[PackedLineInfo] = @[]
  when defined(virtualParRi):
    # See `toString`: reconstruct elided `)` of sealed scopes from jumps.
    var endsAt: seq[int] = @[]
  var mostRecentOffset = 0
  var previousOffset = 0
  var index = nifbuilder.open(tree.len * 2)
  index.addTree ".index"
  if tree.len > 0:
    index.emitLineInfo(tree[0].info, NoLineInfo)
  for n in 0 ..< tree.len:
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
        # Record the **parent** info (stack[^2]) of the indexed compound, not
        # the compound's own info (stack[^1]). On lookup, parse() seeds its
        # parent stack with this value and the very first token it reads is
        # the indexed compound's ParLe — its NIF27 suffix encodes the diff
        # from this same parent, so parent + diff yields the correct absolute
        # info. Recording the compound's own info instead would double-apply
        # the diff (the bug fixed in 2026-04: line=160 vs line=317).
        let parentInfo = if stack.len >= 2: stack[^2] else: tree[0].info
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
      if stack.len > 0:
        discard stack.pop()
      b.endTree()
    of ParLe:
      mostRecentOffset = b.offset
      b.addTree(pool.tags[tree[n].tagId])
    # NIF27: line info is postfix on the atom or tag just emitted.
    if produceLineInfo and info.isValid and k != ParRi:
      emitLineInfo(b, info, if stack.len > 0: stack[^1] else: NoLineInfo)
    if k == ParLe:
      stack.add info
      when defined(virtualParRi):
        let j = jump(tree[n])
        if j != MaxJump: endsAt.add(n + int(j))
    when defined(virtualParRi):
      # Emit synthetic closers for every sealed scope ending at this token.
      while endsAt.len > 0 and endsAt[^1] == n:
        discard endsAt.pop()
        if stack.len > 0: discard stack.pop()
        b.endTree()

  b.patchIndexAt(patchPos, b.offset)
  result = b.extract()
  index.endTree()
  result.add index.extract()

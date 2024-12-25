#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A NIF stream is simply a seq of tokens. It turns out to be useful
## for many different cases.

import std / [assertions, hashes]

import bitabs, stringviews, lineinfos, nifreader, nifbuilder

export TokenKind

const
  InlineInt* = UnknownToken

type
  PackedToken* = object     # 8 bytes
    x: uint32
    info*: PackedLineInfo

const
  TokenKindBits = 4'u32
  TokenKindMask = (1'u32 shl TokenKindBits) - 1'u32

template kind*(n: PackedToken): TokenKind = cast[TokenKind](n.x and TokenKindMask)
template uoperand*(n: PackedToken): uint32 = (n.x shr TokenKindBits)
template soperand*(n: PackedToken): int32 = cast[int32](uoperand(n))

template toX(k: TokenKind; operand: uint32): uint32 =
  uint32(k) or (operand shl TokenKindBits)

proc toToken[L](kind: TokenKind; id: L; info: PackedLineInfo): PackedToken {.inline.} =
  PackedToken(x: toX(kind, uint32(id)), info: info)

proc parRiToken*(info: PackedLineInfo): PackedToken {.inline.} =
  PackedToken(x: toX(ParRi, 0'u32), info: info)

proc addToken[L](tree: var seq[PackedToken]; kind: TokenKind; id: L; info: PackedLineInfo) =
  tree.add PackedToken(x: toX(kind, uint32(id)), info: info)

proc copyKeepLineInfo*(dest: var PackedToken; src: PackedToken) {.inline.} =
  dest.x = src.x

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
    floats*: BiTable[FloatId, float64]

proc `==`*(a, b: SymId): bool {.borrow.}
proc `==`*(a, b: StrId): bool {.borrow.}
proc `==`*(a, b: IntId): bool {.borrow.}
proc `==`*(a, b: UIntId): bool {.borrow.}
proc `==`*(a, b: FloatId): bool {.borrow.}
proc `==`*(a, b: TagId): bool {.borrow.}

proc hash*(x: SymId): Hash {.borrow.}
proc hash*(x: StrId): Hash {.borrow.}
proc hash*(x: IntId): Hash {.borrow.}
proc hash*(x: UIntId): Hash {.borrow.}
proc hash*(x: FloatId): Hash {.borrow.}
proc hash*(x: TagId): Hash {.borrow.}

const
  Suffixed* = TagId(1)
  ErrT* = TagId(2)

proc createLiterals*(): Literals =
  result = default(Literals)
  let t = result.tags.getOrIncl("suf")
  assert t == Suffixed

  let t2 = result.tags.getOrIncl("err")
  assert t2 == ErrT

var pool* = createLiterals()

proc identToken*(s: StrId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(Ident, s, info)

proc symToken*(s: SymId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(Symbol, s, info)

proc dotToken*(info: PackedLineInfo): PackedToken {.inline.} =
  toToken(DotToken, 0'u32, info)

proc symdefToken*(s: SymId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(SymbolDef, s, info)

proc parLeToken*(t: TagId; info: PackedLineInfo): PackedToken {.inline.} =
  toToken(ParLe, t, info)

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

proc registerTag*(tag: string; expected: TagId) =
  ## Mostly useful for code generators like Nifgram.
  let t = pool.tags.getOrIncl(tag)
  assert t == expected, "tag " & tag & " expected Id " & $expected.uint32 &
      " but it is already of value " & $t.uint32

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

proc openFromBuffer*(buf: sink string): Stream =
  result = Stream(r: nifreader.openFromBuffer(buf))
  result.parents.add NoLineInfo

proc close*(s: var Stream) = nifreader.close(s.r)

proc rawNext(s: var Stream; t: Token): PackedToken =
  var currentInfo = NoLineInfo
  if t.filename.len == 0:
    # relative file position
    if t.pos.line != 0 or t.pos.col != 0:
      let (file, line, col) = unpack(pool.man, s.parents[^1])
      currentInfo = pack(pool.man, file, line+t.pos.line, col+t.pos.col)
    else:
      currentInfo = s.parents[^1]
  else:
    # absolute file position:
    let fileId = pool.files.getOrIncl(decodeFilename t)
    currentInfo = pack(pool.man, fileId, t.pos.line, t.pos.col)

  case t.tk
  of ParRi:
    result = toToken(t.tk, 0'u32, currentInfo)
    if s.parents.len > 1:
      discard s.parents.pop()
  of EofToken, UnknownToken, DotToken:
    result = toToken(t.tk, 0'u32, currentInfo)
  of ParLe:
    let ka = pool.tags.getOrInclFromView(t.s)
    result = toToken(ParLe, ka, currentInfo)
    s.parents.add currentInfo
  of Ident, StringLit:
    result = toToken(t.tk, pool.strings.getOrIncl(decodeStr t), currentInfo)
  of Symbol, SymbolDef:
    result = toToken(t.tk, pool.syms.getOrIncl(decodeStr t), currentInfo)
  of CharLit:
    result = toToken(CharLit, uint32 decodeChar(t), currentInfo)
  of IntLit:
    result = toToken(IntLit, pool.integers.getOrIncl(decodeInt t), currentInfo)
  of UIntLit:
    result = toToken(UIntLit, pool.uintegers.getOrIncl(decodeUInt t), currentInfo)
  of FloatLit:
    result = toToken(FloatLit, pool.floats.getOrIncl(decodeFloat t), currentInfo)

proc next*(s: var Stream): PackedToken =
  let t = next(s.r)
  result = rawNext(s, t)

proc skip*(s: var Stream; current: PackedToken): PackedToken =
  if current.kind == ParLe:
    # jump to corresponding ParRi:
    var nested = 0
    while true:
      let t = next(s.r)
      if t.tk == ParLe: inc nested
      elif t.tk == ParRi:
        if nested == 0: break
        dec nested
  result = next(s)

proc litId*(n: PackedToken): StrId {.inline.} =
  assert n.kind in {Ident, StringLit}
  StrId(n.uoperand)

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
  TagId(n.uoperand)

proc tag*(n: PackedToken): TagId {.inline.} =
  if n.kind == ParLe: result = n.tagId
  else: result = ErrT

proc typebits*(n: PackedToken): int =
  if n.kind == IntLit:
    result = pool.integers[n.intId]
  elif n.kind == InlineInt:
    result = n.soperand
  else:
    result = 0

proc toString*(tree: openArray[PackedToken]; produceLineInfo = true): string =
  var b = nifbuilder.open(tree.len * 20)
  var stack: seq[PackedLineInfo] = @[]
  for n in 0 ..< tree.len:
    let info = tree[n].info
    let k = tree[n].kind
    if produceLineInfo and info.isValid and k != ParRi:
      var (file, line, col) = unpack(pool.man, info)
      var fileAsStr = ""
      if stack.len > 0:
        let (pfile, pline, pcol) = unpack(pool.man, stack[^1])
        line = line - pline
        col = col - pcol
        if file != pfile: fileAsStr = pool.files[file]
      else:
        fileAsStr = pool.files[file]
      b.addLineInfo(col, line, fileAsStr)

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
      stack.add info
  result = b.extract()

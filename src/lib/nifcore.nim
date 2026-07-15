## nifcore — in-memory NIF representation, builder, and cursor.
##
## Design summary
## ==============
##
## A NIF token is a `distinct uint32` (4 bytes) with the kind in the low
## 4 bits and a 28-bit kind-specific payload above.
##
## There is **no separate ParLe / ParRi** kind — a node is just a `TagLit`
## that carries (tag, jump), where `jump` counts the body tokens that
## follow. The matching close is implicit; iterators stop after consuming
## `jump` body tokens.
##
## ```
##   TagLit payload (28 bits):
##     [3..0]   kind = TagLit
##     [12..4]  tag  (9 bits, 0..511)
##     [31..13] jump (19 bits, 0..524287 body tokens)
## ```
##
## Atom kinds (`StrLit`, `IntLit`, `FloatLit`, …) put a 28-bit pool id (or
## a small inline value, e.g. `CharLit`) in the payload.
##
## **ExtendedSuffix** is the universal extension knob — one kind handles
## both literal overflow and jump overflow uniformly:
##
##   Atoms with wide payload:
##     [P]   StrLit(low28)
##     [P+1] ExtendedSuffix(high28)            ⇒ 56-bit combined id
##
##   TagLits with overflowing jump:
##     [P]   TagLit(tag, jump_low19)
##     [P+1] ExtendedSuffix(jump_high28)       ⇒ 47-bit combined jump
##     [P+2 .. P+1+combined_jump] body
##
## Putting the extension *after* the kinded token means a cursor always
## lands on the kinded token; `kind(c)` is one load + one mask (no
## branch). The suffix is only consulted by the helpers that actually
## need extended bits (`combinedPayload`, `cursorJump`, `tokenWidth`).
##
## Pool ownership is per-`TokenBuf` by default; pass `sharedPool` to
## `createTokenBuf` to thread the same intern tables through many trees.

when defined(nimony):
  # Generic bodies (e.g. `createTags[E: enum]`) and `untyped`-pool templates
  # are sem-checked at instantiation, Nim-2 style — matches how this file
  # compiles under host Nim. Unknown pragma on host Nim, hence the guard.
  {.feature: "untyped".}
  # `ref` fields/returns are nilable here (e.g. `pool`/`tags` return nil for
  # an ownerless cursor); opt out of Nimony's strict not-nil analysis.
  {.feature: "lenientnils".}

import std / [assertions, hashes]
import bitabs, lineinfos
export bitabs  # adapters touching pool.strings / tags need getOrIncl etc.
export lineinfos.FileId, lineinfos.NoFile, lineinfos.isValid

type
  NifKind* = enum
    DotToken
    CharLit
    StrLit
    IntLit
    UIntLit
    FloatLit
    Symbol
    SymbolDef
    Ident
    TagLit            # opening tag with body-token-count "jump"
    ExtendedSuffix    # supplies 28 extra high bits to the preceding token
    LineInfoLit       # trailing line-info suffix on a head token (file/line/col)

  NifToken* = distinct uint32

  TagId* = distinct uint32
  StrId* = distinct uint32
  SymId* = distinct uint32

func `==`*(a, b: NifToken): bool {.borrow.}
func `==`*(a, b: TagId): bool {.borrow.}
func `==`*(a, b: StrId): bool {.borrow.}
func `==`*(a, b: SymId): bool {.borrow.}

func hash*(x: TagId): Hash {.borrow.}
func hash*(x: StrId): Hash {.borrow.}
func hash*(x: SymId): Hash {.borrow.}

proc `$`*(x: TagId): string {.borrow.}
proc `$`*(x: StrId): string {.borrow.}
proc `$`*(x: SymId): string {.borrow.}

# ── bit-packing constants ────────────────────────────────────────────────

const
  KindBits*    = 4'u32
  KindMask*    = (1'u32 shl KindBits) - 1'u32       # 0x0F
  PayloadBits* = 32'u32 - KindBits                  # 28
  PayloadMask* = (1'u32 shl PayloadBits) - 1'u32    # 0x0FFFFFFF

  TagBits*     = 9'u32
  TagShift*    = KindBits                           # 4
  TagMask*     = (1'u32 shl TagBits) - 1'u32        # 0x1FF
  JumpShift*   = KindBits + TagBits                 # 13
  JumpBits*    = 32'u32 - JumpShift                 # 19
  InlineJumpCap* = (1'u32 shl JumpBits) - 1'u32     # 524287

template kind*(n: NifToken): NifKind = NifKind(uint32(n) and KindMask)
template uoperand*(n: NifToken): uint32 = uint32(n) shr KindBits
template soperand*(n: NifToken): int32 =
  ## Sign-extended 28-bit payload (for inline signed values).
  cast[int32](uint32(n)) shr KindBits.int32

template toX(k: NifKind; operand: uint32): uint32 =
  uint32(k) or (operand shl KindBits)

# ── atom constructors ────────────────────────────────────────────────────

proc dotToken*(): NifToken {.inline.} = NifToken(uint32(DotToken))

proc tagLitToken*(t: TagId; jump: uint32 = 0): NifToken {.inline.} =
  let tagBits = uint32(t) and TagMask
  assert uint32(t) == tagBits, "tag id " & $uint32(t) & " exceeds 9 bits"
  assert jump <= InlineJumpCap, "use ExtendedSuffix for jump > InlineJumpCap"
  NifToken(uint32(TagLit) or (tagBits shl TagShift) or (jump shl JumpShift))

proc charToken*(ch: char): NifToken {.inline.} =
  NifToken(toX(CharLit, uint32(ch)))

proc strLitToken*(id: StrId): NifToken {.inline.} =
  assert uint32(id) <= PayloadMask
  NifToken(toX(StrLit, uint32(id) shl 1))    # bit 0 = 0 ⇒ pool ref
proc symToken*(id: SymId): NifToken {.inline.} =
  assert uint32(id) <= PayloadMask
  NifToken(toX(Symbol, uint32(id) shl 1))
proc symdefToken*(id: SymId): NifToken {.inline.} =
  assert uint32(id) <= PayloadMask
  NifToken(toX(SymbolDef, uint32(id) shl 1))
proc identToken*(id: StrId): NifToken {.inline.} =
  assert uint32(id) <= PayloadMask
  NifToken(toX(Ident, uint32(id) shl 1))

proc extendedSuffixToken*(high28: uint32): NifToken {.inline.} =
  assert high28 <= PayloadMask
  NifToken(toX(ExtendedSuffix, high28))

# ── Line info (LineInfoLit) ──────────────────────────────────────────────
# Line info is sparse: a `LineInfoLit` rides as a trailing suffix on a head
# token (after any value/jump `ExtendedSuffix`), emitted only where the
# position changes. The file is a `FileId` interned in `pool.filenames`;
# line/col are inline. Two fixed layouts of the (possibly suffix-extended)
# payload, chosen by whether the `LineInfoLit` carries its own trailing
# `ExtendedSuffix` (detected structurally — no flag bit):
#
#   Common (no suffix, 28 bits):  col 7 | file 7  | line 14
#   Overflow (one suffix, 56 bits): col 10 | file 14 | line 32
#
# Common matches nimony's `lineinfos` field sizes (line 14, col 7); file is
# trimmed 10→7. Any field past its common width bumps the whole value to the
# overflow layout.
#
# An optional `#comment#` decoration (a `StrId` into `pool.strings`) rides as
# one further `ExtendedSuffix` after the position word. To keep the layout
# selectable structurally (still no flag bit), a comment ALWAYS forces the
# overflow position layout, so the count `k` of `ExtendedSuffix` words trailing
# the `LineInfoLit` decodes unambiguously:
#   k == 0  →  common position, no comment
#   k == 1  →  overflow position, no comment   (the two pre-comment cases)
#   k >= 2  →  overflow position (word 0) + comment id in words 1.. (28-bit
#             chunks, low first; a 2nd word only for ids past 2^28)
# The head-skip walk (`tokenWidth`, `kind >= ExtendedSuffix`) already absorbs
# these extra words, so navigation needs no change.

const
  LiColBitsC*  = 7'u32
  LiFileBitsC* = 7'u32
  LiLineBitsC* = 14'u32
  LiColMaxC*   = (1'u32 shl LiColBitsC) - 1'u32      # 127
  LiFileMaxC*  = (1'u32 shl LiFileBitsC) - 1'u32     # 127
  LiLineMaxC*  = (1'u32 shl LiLineBitsC) - 1'u32     # 16383

  LiColBitsX  = 10'u64
  LiFileBitsX = 14'u64
  LiColMaskX  = (1'u64 shl LiColBitsX) - 1'u64
  LiFileMaskX = (1'u64 shl LiFileBitsX) - 1'u64
  LiLineMaskX = (1'u64 shl 32'u64) - 1'u64

type
  NifLineInfo* = object
    file*: FileId
    line*, col*: int32
    comment*: StrId
      ## Optional NIF `#…#` decoration on the head, interned in `pool.strings`
      ## (`StrId(0)` = none). Carried in the `LineInfoLit` suffix; see below.

const
  NoNifLineInfo* = NifLineInfo(file: NoFile, line: 0'i32, col: 0'i32,
                               comment: StrId(0))

proc isValid*(x: NifLineInfo): bool {.inline.} = x.file.isValid

proc fitsCommonLineInfo(file: FileId; line, col: int32): bool {.inline.} =
  uint32(file) <= LiFileMaxC and uint32(col) <= LiColMaxC and
    uint32(line) <= LiLineMaxC

proc encodeLineInfoCommon(file: FileId; line, col: int32): uint32 {.inline.} =
  (uint32(col) and LiColMaxC) or
  ((uint32(file) and LiFileMaxC) shl LiColBitsC) or
  ((uint32(line) and LiLineMaxC) shl (LiColBitsC + LiFileBitsC))

proc encodeLineInfoWide(file: FileId; line, col: int32): uint64 {.inline.} =
  (uint64(col) and LiColMaskX) or
  ((uint64(file) and LiFileMaskX) shl LiColBitsX) or
  ((uint64(line) and LiLineMaskX) shl (LiColBitsX + LiFileBitsX))

# ── TagLit field accessors (raw — don't consult the suffix) ──────────────

proc rawJump(n: NifToken): uint32 {.inline.} =
  assert n.kind == TagLit, $n.kind
  uint32(n) shr JumpShift

proc rawTag(n: NifToken): TagId {.inline.} =
  assert n.kind == TagLit, $n.kind
  TagId((uint32(n) shr TagShift) and TagMask)

proc setJump*(n: var NifToken; j: uint32) {.inline.} =
  assert n.kind == TagLit, $n.kind
  assert j <= InlineJumpCap, "jump " & $j & " exceeds 19 bits"
  let preserved = uint32(n) and ((1'u32 shl JumpShift) - 1'u32)  # kind+tag
  n = NifToken(preserved or (j shl JumpShift))

proc setTag*(n: var NifToken; t: TagId) {.inline.} =
  assert n.kind == TagLit, $n.kind
  let tagBits = uint32(t) and TagMask
  assert uint32(t) == tagBits, "tag id " & $uint32(t) & " exceeds 9 bits"
  n = NifToken((uint32(n) and not (KindMask or (TagMask shl TagShift))) or
               uint32(TagLit) or (tagBits shl TagShift))

# ── Pool (literals) and TagPool (tags) ───────────────────────────────────
# Two refs, deliberately split:
#
# * `Pool` (literals) is sharable across adapters — a string is a string,
#   whether it came from JSON or HTML, and cross-format dedup is the
#   whole point.
# * `TagPool` is language-specific. Each adapter creates its own and
#   registers its tag enum in ordinal order at startup, so the resulting
#   TagId equals the enum ordinal and `cast[MyTag](c.cursorTagId.uint32)`
#   is a register-to-register move with no memory access. Sharing a tag
#   pool across adapters destroys that property and gives no benefit
#   (tag namespaces don't overlap meaningfully).

type
  Pool* = ref object
    ## Literals pool — only categories where dedup genuinely pays:
    ## strings and symbols. Integers / unsigned integers / floats are
    ## stored entirely inside the token stream (using a chain of
    ## `ExtendedSuffix` tokens to widen the carrier), so there's no
    ## pool for them — no hash lookups, no auto-tune machinery.
    strings*:   BiTable[StrId, string]
    syms*:      BiTable[SymId, string]
    filenames*: BiTable[FileId, string]
      ## Source filenames referenced by `LineInfoLit` tokens. Line/col are
      ## encoded inline in the token; only the filename is interned here.

  TagPool* = ref object
    tags*: BiTable[TagId, string]

proc newPool*(): Pool =
  Pool(strings:   initBiTable[StrId, string](),
       syms:      initBiTable[SymId, string](),
       filenames: initBiTable[FileId, string]())

proc newTagPool*(): TagPool =
  ## All BiTable ids start at 1 (id 0 is the "not used" sentinel).
  ## Adapters whose enum has ordinal 0 as the first real value bridge
  ## the gap with a `+/- 1` shim in their `tagId` / `myKind` helpers
  ## (see jsonnif/htmlnif).
  TagPool(tags: initBiTable[TagId, string]())

proc registerTag*(tp: TagPool; tag: string): TagId =
  ## Intern a tag string. Adapters call this in enum-ordinal order at
  ## startup so the returned TagId equals the enum ordinal (1-based).
  tp.tags.getOrIncl(tag)

proc createTags*[E: enum](): TagPool =
  ## One-shot tag-pool builder for an adapter's enum. Registers every
  ## value of `E` in ordinal order using its string form (`$e`), so the
  ## resulting TagIds are `1, 2, …` matching the `tagId` / `myKind`
  ## `+/- 1` shim. Replaces hand-rolled `createJsonTagPool` /
  ## `createHtmlTagPool` boilerplate.
  ##
  ## The assertion guards against an enum with holes or out-of-order
  ## ordinals — those would silently break the `cast[E](tagId-1)` path.
  result = newTagPool()
  for e in E.low..E.high:
    let id = result.registerTag($e)
    assert id.uint32 == e.uint32 + 1'u32,
      "createTags: enum/TagId misalignment for " & $e & " (got id " & $id & ")"

template tagName*(tp: TagPool; t: TagId): lent string = tp.tags[t]
# Direct pool lookups (only meaningful for pool-mode payloads — inline
# strings/syms don't have ids). Prefer the cursor-side `strVal(c)` /
# `symName(c)` accessors which handle both modes transparently.
template poolStr*(p: Pool; s: StrId): lent string = p.strings[s]
template poolSym*(p: Pool; s: SymId): lent string = p.syms[s]

# ── Storage / CursorOwner / Cursor ───────────────────────────────────────

type
  Storage = ptr UncheckedArray[NifToken]
  CursorOwner = ptr CursorOwnerObj
  CursorOwnerObj = object
    rc: int
    data: Storage
    pool: Pool      ## literals (sharable across adapters)
    tags: TagPool   ## tag space (per-adapter)
    ## Both ref-tracked manually via GC_ref/GC_unref since this object
    ## lives in raw `alloc`'d memory and ARC won't trace ref fields here.

  Cursor* = object
    owner: CursorOwner
    p: ptr NifToken
    rem: int

  CursorScope* = object ## Saved outer bounds for bounded cursor traversal.
    savedP: ptr NifToken
    savedRem: int
    bodyLen: int

template decRcAndFree(owner: CursorOwner) =
  dec owner.rc
  if owner.rc == 0:
    if owner.data != nil: dealloc(owner.data)
    if owner.pool != nil:
      GC_unref(owner.pool)
      owner.pool = nil
    if owner.tags != nil:
      GC_unref(owner.tags)
      owner.tags = nil
    dealloc(owner)

proc pool*(c: Cursor): Pool {.inline.} =
  ## Literals pool the cursor's underlying buffer was built against.
  if c.owner != nil: c.owner.pool else: nil

proc tags*(c: Cursor): TagPool {.inline.} =
  ## Tag pool the cursor's underlying buffer was built against. Adapter
  ## code is expected to know which TagPool layout to expect, so callers
  ## typically reach for `cast[MyTag](c.cursorTagId.uint32)` instead of
  ## consulting `c.tags` directly.
  if c.owner != nil: c.owner.tags else: nil

proc toUniqueId*(c: Cursor): int {.inline.} =
  ## A stable identity for the cursor's *position*: two cursors over the same
  ## buffer at the same token share it, distinct positions differ. Suitable as a
  ## `HashSet[int]`/`IntSet` key (e.g. type-traversal dedup). Not stable across
  ## buffers or runs — it is the underlying token pointer reinterpreted.
  cast[int](c.p)

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`*(c: Cursor) {.inline.} =
    if c.owner != nil: decRcAndFree(c.owner)
else:
  proc `=destroy`*(c: var Cursor) {.inline.} =
    if c.owner != nil: decRcAndFree(c.owner)

proc `=wasMoved`*(c: var Cursor) {.inline.} =
  c.owner = nil
  c.p = nil
  c.rem = 0

proc `=copy`*(dest: var Cursor; src: Cursor) {.inline.} =
  if dest.owner != src.owner or dest.p != src.p:
    `=destroy`(dest)
    if src.owner != nil: inc src.owner.rc
    dest.owner = src.owner
    dest.p = src.p
  dest.rem = src.rem

proc `=dup`*(src: Cursor): Cursor {.nodestroy, inline.} =
  result = Cursor(owner: src.owner, p: src.p, rem: src.rem)
  if result.owner != nil: inc result.owner.rc

# ── Cursor primitives ────────────────────────────────────────────────────

proc load*(c: Cursor): NifToken {.inline.} =
  assert c.p != nil and c.rem > 0
  c.p[]

template peekAhead(c: Cursor; offset: int): NifToken =
  cast[ptr NifToken](cast[uint](c.p) +
                     uint(offset) * sizeof(NifToken).uint)[]

template advanceBy(c: var Cursor; n: int) =
  # Capture `n` once — callers pass `tokenWidth(c)`, which reads c.p, and
  # re-evaluating after we move c.p would read the wrong token.
  let advance = n
  c.p = cast[ptr NifToken](cast[uint](c.p) +
                           uint(advance) * sizeof(NifToken).uint)
  if c.rem > 0:
    c.rem = if c.rem >= advance: c.rem - advance else: 0

proc cursorIsNil*(c: Cursor): bool {.inline.} = c.p == nil

proc kind*(c: Cursor): NifKind {.inline.} =
  ## Effective kind of the value at the cursor. Always one load + one
  ## mask — the cursor lands on the kinded token; any `ExtendedSuffix`
  ## sits *after* it and is only consulted when extended bits are needed.
  c.load.kind

proc tokenWidth*(c: Cursor): int {.inline.} =
  ## Tokens occupied by the head of the current value — the kinded
  ## token plus any consecutive `ExtendedSuffix` tokens chained behind
  ## it. NOT including a TagLit's body.
  ##
  ## Chains are unbounded in principle; in practice the writers in
  ## nifcore emit at most 2 suffixes (enough to cover int64/float64 /
  ## 47-bit jumps / 55-bit pool ids). The hot path — no suffix at all
  ## — is one peek + one branch, same as the prior single-suffix code.
  result = 1
  while c.rem > result and peekAhead(c, result).kind >= ExtendedSuffix:
    inc result

proc combinedPayload*(c: Cursor): uint64 {.inline.} =
  ## Combine the kinded token's 28-bit payload with any chained
  ## ExtendedSuffix tokens. Each suffix supplies the next 28 high bits.
  result = uint64(c.load.uoperand)
  var i = 1
  var shift = PayloadBits
  while c.rem > i and peekAhead(c, i).kind == ExtendedSuffix:
    result = result or (uint64(peekAhead(c, i).uoperand) shl shift)
    inc i
    shift += PayloadBits

# String/sym layout (StrLit, Ident, Symbol, SymbolDef):
#   bit 0      mode (1 = inline-short, 0 = pool ref)
#   inline:
#     bits 1..2   length (0..3)
#     bits 3..10  char 0 (if length ≥ 1)
#     bits 11..18 char 1 (if length ≥ 2)
#     bits 19..26 char 2 (if length ≥ 3)
#     bit 27      unused
#   pool ref:
#     bits 1..27  pool id (27 bits, or up to 55 with one ExtendedSuffix)
#
# Writer rule: `s.len <= 3` always goes inline, longer goes pool. That
# preserves the same-string ⇒ same-payload invariant (no string is
# encodable both ways in the same tree), so a token-payload equality
# test remains a valid same-pool string-equality fast path.

const
  StrInlineFlag* = 1'u32                 ## bit 0 = inline mode
  StrLengthShift* = 1'u32
  StrLengthMask* = 3'u32                 ## bits 1..2
  StrDataShift* = 3'u32                  ## bits 3..26 hold up to 3 bytes
  StrInlineMaxLen* = 3                   ## bytes that fit inline

proc isInlineLit*(c: Cursor): bool {.inline.} =
  ## True if a StrLit/Ident/Symbol/SymbolDef stores its bytes inline.
  (c.load.uoperand and StrInlineFlag) != 0'u32

proc inlineStrLen(payload: uint32): int {.inline.} =
  int((payload shr StrLengthShift) and StrLengthMask)

proc readInlineStr(payload: uint32): string =
  let length = inlineStrLen(payload)
  result = newString(length)
  let chars = payload shr StrDataShift
  for i in 0 ..< length:
    result[i] = char((chars shr (uint32(i) * 8)) and 0xFF)

proc strVal*(c: Cursor; pool: Pool): string =
  ## Decode a StrLit/Ident at the cursor into a `string`. Handles the
  ## inline-short path (no pool touch) and the pool-ref path (with or
  ## without an ExtendedSuffix extending the pool id).
  assert c.kind in {StrLit, Ident}, "strVal on " & $c.kind
  let payload = c.load.uoperand
  if (payload and StrInlineFlag) != 0'u32:
    readInlineStr(payload)
  else:
    pool.strings[StrId(combinedPayload(c) shr 1)]

proc strVal*(c: Cursor): string {.inline.} = strVal(c, c.pool)

proc strId*(c: Cursor; pool: Pool): StrId =
  ## Stable pool id of the StrLit/Ident at `c` — the inverse of `strVal`.
  ## A pool-ref token already carries its id, so use it directly; only an inline
  ## short string (stored in the token itself) has to be interned. Mirrors
  ## `symId` and avoids the decode-then-reintern round trip of `strings[strVal]`.
  assert c.kind in {StrLit, Ident}, "strId on " & $c.kind
  let payload = c.load.uoperand
  if (payload and StrInlineFlag) != 0'u32:
    pool.strings.getOrIncl(readInlineStr(payload))
  else:
    StrId(combinedPayload(c) shr 1)

proc strId*(c: Cursor): StrId {.inline.} = strId(c, c.pool)

proc symName*(c: Cursor; pool: Pool): string =
  assert c.kind in {Symbol, SymbolDef}, "symName on " & $c.kind
  let payload = c.load.uoperand
  if (payload and StrInlineFlag) != 0'u32:
    readInlineStr(payload)
  else:
    pool.syms[SymId(combinedPayload(c) shr 1)]

proc symName*(c: Cursor): string {.inline.} = symName(c, c.pool)

proc symId*(c: Cursor; pool: Pool): SymId =
  ## Stable pool id of the Symbol/SymbolDef at `c` — the inverse of `symName`.
  ## A pool-ref token already carries its id, so use it directly; only an inline
  ## short name (stored in the token itself) has to be interned.
  assert c.kind in {Symbol, SymbolDef}, "symId on " & $c.kind
  let payload = c.load.uoperand
  if (payload and StrInlineFlag) != 0'u32:
    pool.syms.getOrIncl(readInlineStr(payload))
  else:
    SymId(combinedPayload(c) shr 1)

proc symId*(c: Cursor): SymId {.inline.} = symId(c, c.pool)

# Int/UInt/Float: pure-inline via chainable ExtendedSuffix.
#
#   IntLit  payload: 28-bit signed (one token); 56-bit signed with one
#                    suffix; 84-bit (covers int64) with two suffixes.
#   UIntLit payload: 28-bit unsigned; widens the same way.
#   FloatLit payload: cast[uint64](float64); 0 / 1 / 2 suffixes depending
#                     on which 28-bit chunk is the highest set bit.
#
# No flag bit, no pool — Pool.integers / Pool.uintegers / Pool.floats
# are gone. The reader's per-call cost is the same as before in the
# common case (no suffix → one peek + one branch).

proc valueWidth*(c: Cursor): int {.inline.} =
  ## Tokens carrying value bits — the kinded token plus consecutive
  ## `ExtendedSuffix` tokens. Excludes trailing LineInfoLit (which
  ## `tokenWidth` does include for advance-past purposes). This is the
  ## width sign-extension wants for IntLit; it lines up exactly with
  ## what `combinedPayload` chooses to OR together.
  result = 1
  while c.rem > result and peekAhead(c, result).kind == ExtendedSuffix:
    inc result

proc intVal*(c: Cursor): int64 {.inline.} =
  assert c.kind == IntLit
  let width = uint32(valueWidth(c)) * PayloadBits      # 28, 56, or 84
  let combined = combinedPayload(c)
  if width >= 64'u32:
    cast[int64](combined)                              # int64 fully used
  else:
    let shift = uint64(64'u32 - width)
    cast[int64](combined shl shift) shr shift          # sign-extend

proc uintVal*(c: Cursor): uint64 {.inline.} =
  assert c.kind == UIntLit
  combinedPayload(c)                                   # no sign extension

proc floatVal*(c: Cursor): float64 {.inline.} =
  assert c.kind == FloatLit
  cast[float64](combinedPayload(c))

proc charLit*(c: Cursor): char {.inline.} =
  ## Returns the character stored in the `CharLit` at `c`.
  assert c.kind == CharLit
  char(c.load.uoperand)

# TagLit accessors — the tag is always in the kinded token; the jump
# extends via the suffix when present.
proc cursorTagId*(c: Cursor): TagId {.inline.} =
  assert c.kind == TagLit
  c.load.rawTag

proc cursorJump*(c: Cursor): uint64 {.inline.} =
  ## Body tokens following this TagLit. 19 bits if unsuffixed; with one
  ## ExtendedSuffix the field widens to 47 bits (further chaining would
  ## extend it further, but no plausible jump needs that). The unified
  ## `combinedPayload` handles the suffix walk for us; we just discard
  ## the low 9 tag bits to expose the jump.
  assert c.kind == TagLit
  combinedPayload(c) shr TagBits

# ── Line info reader ─────────────────────────────────────────────────────

proc rawLineInfo*(c: Cursor): NifLineInfo =
  ## Decode the `LineInfoLit` trailing the value at `c`, if present, returning
  ## a `FileId` (resolve against `pool.filenames`) plus line/col, and an
  ## optional `comment` `StrId` (a NIF `#…#` decoration; `StrId(0)` = none).
  ## Returns `NoNifLineInfo` when the head carries no line info. The
  ## `LineInfoLit` sits after the value's `ExtendedSuffix` chain (`valueWidth`);
  ## file and comment are interned, so cross-pool readers must map them via the
  ## right pool. Layout is selected structurally by the count `k` of
  ## `ExtendedSuffix` words trailing the `LineInfoLit` (see the section header):
  ## `k==0` common/no-comment, `k==1` wide/no-comment, `k>=2` wide + comment.
  # Walk past the value carrier's own ExtendedSuffix chain.
  var off = 1
  while c.rem > off and peekAhead(c, off).kind == ExtendedSuffix: inc off
  if c.rem <= off or peekAhead(c, off).kind != LineInfoLit:
    return NoNifLineInfo
  let lit = peekAhead(c, off)
  # Count the ExtendedSuffix words trailing the LineInfoLit (position + comment).
  var ext = off + 1
  while c.rem > ext and peekAhead(c, ext).kind == ExtendedSuffix: inc ext
  let extCount = ext - (off + 1)
  if extCount == 0:
    # common layout: col 7 | file 7 | line 14 in 28 bits
    let p = lit.uoperand
    result = NifLineInfo(
      col:  int32(p and LiColMaxC),
      file: FileId((p shr LiColBitsC) and LiFileMaxC),
      line: int32((p shr (LiColBitsC + LiFileBitsC)) and LiLineMaxC))
  else:
    # overflow layout: col 10 | file 14 | line 32 across 56 bits (lit + word 0)
    let combined = uint64(lit.uoperand) or
                   (uint64(peekAhead(c, off + 1).uoperand) shl PayloadBits)
    result = NifLineInfo(
      col:  int32(combined and LiColMaskX),
      file: FileId((combined shr LiColBitsX) and LiFileMaskX),
      line: int32((combined shr (LiColBitsX + LiFileBitsX)) and LiLineMaskX))
    # Words 1.. hold the comment StrId as 28-bit chunks, low first.
    if extCount >= 2:
      var cid = 0'u64
      var shift = 0'u64
      for i in 1 ..< extCount:
        cid = cid or (uint64(peekAhead(c, off + 1 + i).uoperand) shl shift)
        shift += uint64(PayloadBits)
      result.comment = StrId(cid)

proc lineInfoFile*(c: Cursor): string =
  ## The filename for the value's line info, resolved against `c.pool.filenames`.
  ## Empty string when there is no line info.
  let li = rawLineInfo(c)
  if li.file.isValid and c.pool != nil: c.pool.filenames[li.file] else: ""

# ── inc / skip / into ────────────────────────────────────────────────────

proc inc*(c: var Cursor) {.inline.} =
  ## Advance past the *head* of the current value (kinded token plus its
  ## suffix, if any). For a TagLit this lands at the first body token;
  ## use `skip` to jump past the whole subtree.
  assert c.rem != 0, "advancing past end of scope"
  advanceBy(c, tokenWidth(c))

proc skip*(c: var Cursor) =
  ## Advance past the current value, including all descendants of a TagLit.
  if c.kind == TagLit:
    let span = int(tokenWidth(c).uint64 + c.cursorJump)
    advanceBy(c, span)
  else:
    advanceBy(c, tokenWidth(c))

template hasMore*(c: Cursor): bool =
  ## True while there are more tokens in the current bounded scope. No
  ## sentinel: `rem` does all the work.
  c.rem > 0

proc enterScope*(c: var Cursor): CursorScope =
  ## Enters the current `TagLit` body and returns its saved outer bounds.
  ## Pair with `leaveScope` after consuming every child.
  assert c.load.kind == TagLit, "into requires cursor at TagLit"
  result = CursorScope(savedP: c.p, savedRem: c.rem,
                       bodyLen: int(c.cursorJump))
  let headWidth = tokenWidth(c)
  c.p = cast[ptr NifToken](cast[uint](c.p) +
                           uint(headWidth) * sizeof(NifToken).uint)
  c.rem = result.bodyLen

proc leaveScope*(c: var Cursor; scope: CursorScope) =
  ## Leaves a scope opened by `enterScope`.
  ## The cursor must have consumed the complete bounded body.
  assert c.rem == 0, "into: body did not consume all " & $scope.bodyLen &
                     " children (left " & $c.rem & ")"
  let consumed = int((cast[uint](c.p) - cast[uint](scope.savedP)) div
                     sizeof(NifToken).uint)
  c.rem = if scope.savedRem >= consumed:
            scope.savedRem - consumed
          else:
            0

proc sub*(c: Cursor): Cursor =
  ## Read-only descent: returns a bounded cursor over the children of the
  ## `TagLit` at `c`, leaving `c` itself untouched. Use it for a throwaway
  ## walk of a node's body (`while result.hasMore: …`) where there is no dest
  ## to preserve into and hence no scope to `leaveScope`. Replaces the old
  ## `var t = c; discard enterScope(t)` idiom.
  assert c.load.kind == TagLit, "sub requires cursor at TagLit"
  result = c
  discard enterScope(result)

template into*(c: var Cursor; body: untyped) =
  ## Enters the current `TagLit`, runs `body`, then restores the outer bounds.
  ## `body` must consume every child.
  let cursorScope = enterScope(c)
  body
  leaveScope(c, cursorScope)

template loopInto*(c: var Cursor; body: untyped) =
  into c:
    while c.hasMore: body

proc leaveScopePartial*(c: var Cursor; scope: CursorScope) =
  ## Leaves a scope opened by `enterScope` **without** requiring the body to
  ## have been fully consumed: rewinds to the scope head and skips the whole
  ## subtree. The early-out counterpart to `leaveScope`.
  c.p = scope.savedP
  c.rem = scope.savedRem
  skip c

template peekInto*(c: var Cursor; body: untyped) =
  ## Like `into`, but `body` need not consume every child — any unconsumed
  ## remainder is skipped. Use for early-out searches over a node's children
  ## (e.g. `break` out on the first match). The finish is a single `skip`
  ## from the rewound scope head, so it does not depend on where `body`
  ## stopped; `body` still sees a bounded scope so `hasMore` terminates.
  let cursorScope = enterScope(c)
  body
  leaveScopePartial(c, cursorScope)

proc rootOf*(c: Cursor): SymId =
  ## The access root of an lvalue: the first `Symbol` in the subtree at `c`
  ## — `x` in `x.f[i]` — or `SymId(0)` if there is none.
  if not c.hasMore: return SymId(0)
  case c.kind
  of Symbol:
    result = symId(c)
  of TagLit:
    result = SymId(0)
    var n = c
    n.loopInto:
      if result == SymId(0):
        let inner = rootOf(n)
        if inner != SymId(0): result = inner
      skip n
  else:
    result = SymId(0)

# ── TokenBuf ─────────────────────────────────────────────────────────────

type
  TokenBuf* = object
    data: Storage
    len, cap: int
    owner: CursorOwner
    openTags: seq[int]            # build-time stack of unsealed TagLit positions
    pool*: Pool                   # literals (typically shared by an app)
    tags*: TagPool                # tags (typically per-adapter)

proc `=copy`(dest: var TokenBuf; src: TokenBuf) {.error.}
proc `=wasMoved`(dest: var TokenBuf) {.nodestroy, inline.} =
  # `nodestroy`: this hook establishes the moved-from state and may run on
  # uninitialized storage (Nimony does not zero-init `result`), so the field
  # writes below must be raw stores — never destroy the prior (garbage) value.
  dest.data = nil
  dest.len = 0
  dest.cap = 0
  dest.owner = nil
  dest.pool = nil
  dest.tags = nil
  dest.openTags = @[]

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`(dest: TokenBuf) {.inline.} =
    if dest.owner != nil: decRcAndFree(dest.owner)
    elif dest.data != nil: dealloc(dest.data)
else:
  proc `=destroy`(dest: var TokenBuf) {.inline.} =
    if dest.owner != nil: decRcAndFree(dest.owner)
    elif dest.data != nil: dealloc(dest.data)

proc createTokenBuf*(cap = 16; sharedPool: Pool = nil;
                     sharedTags: TagPool = nil): TokenBuf =
  ## Mint a new buffer. Pass `sharedPool` to thread a single literals
  ## pool through multiple buffers (cross-format dedup). Pass
  ## `sharedTags` only if you want multiple buffers to use the same
  ## tag namespace — adapters typically create their own fresh
  ## `TagPool` per buffer so `cast[Enum](c.cursorTagId.uint32)` lines
  ## up with the adapter's enum ordinals.
  result = TokenBuf(
    data: cast[Storage](alloc(sizeof(NifToken) * cap)),
    len: 0, cap: cap,
    pool: (if sharedPool != nil: sharedPool else: newPool()),
    tags: (if sharedTags != nil: sharedTags else: newTagPool())
  )

proc adoptForeignTokens*(data: pointer; count: int;
                         sharedPool: Pool = nil; sharedTags: TagPool = nil): TokenBuf =
  ## Build a buffer whose `count` tokens are BORROWED from `data` (e.g. an mmap'd
  ## `.bif` region) rather than copied into a heap allocation — a zero-copy load.
  ## The buffer reads like any other. It NEVER frees `data`: the block is handed to
  ## an eagerly-created cursor owner seeded with an EXTRA, permanent reference
  ## (`rc = 2` = the buffer's own ref plus one keep-alive), so `decRcAndFree` never
  ## reaches 0 and never `dealloc`s the borrowed storage. A *mutation* still works
  ## unchanged: because the owner is shared (`rc > 1`), `prepareMutation` forks a
  ## private heap copy and abandons the borrowed block — no special-casing anywhere
  ## else. The buffer does NOT own the mapping's lifetime: `data` must stay valid
  ## (and aligned for `NifToken`, 4 bytes) for as long as the buffer or any cursor
  ## over it lives. The caller keeps the backing store resident — typically for the
  ## whole process; an mmap left mapped costs only address space, reclaimed at exit.
  assert (cast[uint](data) and (sizeof(NifToken).uint - 1)) == 0,
    "adoptForeignTokens: misaligned token block"
  result = TokenBuf(
    data: cast[Storage](data),
    len: count, cap: count,
    pool: (if sharedPool != nil: sharedPool else: newPool()),
    tags: (if sharedTags != nil: sharedTags else: newTagPool())
  )
  # Pin the borrowed block for the buffer's whole life via an owner seeded with a
  # permanent extra ref — same shape `ensureOwner` builds, but `rc = 2` so it is
  # never collected. (Mirrors `ensureOwner`, inlined because that proc is declared
  # further down and this one needs the non-default seed.)
  result.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
  result.owner.data = cast[Storage](data)
  result.owner.pool = result.pool
  result.owner.tags = result.tags
  GC_ref(result.owner.pool)
  GC_ref(result.owner.tags)
  result.owner.rc = 2

proc len*(b: TokenBuf): int {.inline.} = b.len

proc `[]`*(b: TokenBuf; i: int): NifToken {.inline.} =
  assert i >= 0 and i < b.len
  b.data[i]

proc `[]=`*(b: var TokenBuf; i: int; v: NifToken) {.inline.} =
  assert i >= 0 and i < b.len
  b.data[i] = v

proc prepareMutation*(b: var TokenBuf) {.inline.} =
  if b.owner != nil:
    if b.owner.rc == 1:
      # Reclaim the owner header; b.data keeps its allocation. Detach
      # data first so decRcAndFree's data-dealloc path doesn't free
      # what the TokenBuf is about to keep using.
      b.owner.data = nil
      decRcAndFree(b.owner)
      b.owner = nil
    else:
      let newData = cast[Storage](alloc(sizeof(NifToken) * b.cap))
      copyMem(newData, b.data, sizeof(NifToken) * b.len)
      decRcAndFree(b.owner)
      b.owner = nil
      b.data = newData

proc expectUnique*(b: var TokenBuf) {.inline.} =
  when defined(debug):
    assert b.owner == nil or b.owner.rc == 1,
      "TokenBuf has live cursors; missing endRead before mutation"

template ensureCap(b: var TokenBuf) =
  if b.len >= b.cap:
    b.cap = max(b.cap div 2 + b.cap, 8)
    b.data = cast[Storage](realloc(b.data, sizeof(NifToken) * b.cap))

proc add*(b: var TokenBuf; t: NifToken) {.inline.} =
  ## Plain append. Use `openTag` / `closeTag` for nested TagLits.
  if b.owner != nil: prepareMutation(b)
  ensureCap(b)
  b.data[b.len] = t
  inc b.len

# ── Raw bulk token access (for binary serialization, see bif.nim) ────────
# These expose the contiguous token storage for direct block I/O. They bypass
# all structural bookkeeping (jumps, openTags, suffix chains) — the caller must
# hand over / consume a stream that is already well-formed. The pools are
# serialized separately; because token payloads only reference *pool ids*
# (assigned 1,2,… in intern order), a loader that re-interns the pool values in
# the same order reproduces identical ids, so the raw token words stay valid.

proc rawTokenPtr*(b: TokenBuf): pointer {.inline.} =
  ## Pointer to the first token word; `len(b) * sizeof(NifToken)` bytes follow.
  b.data

proc growRawUninit*(b: var TokenBuf; count: int): pointer =
  ## Grow storage to hold exactly `count` tokens, set `len = count`, and return
  ## the storage pointer so the caller can fill it directly (e.g. `readBuffer`).
  ## The contents are left uninitialized. Binary loaders only.
  if b.owner != nil: prepareMutation(b)
  if count > b.cap:
    b.cap = count
    b.data = cast[Storage](realloc(b.data, sizeof(NifToken) * b.cap))
  b.len = count
  b.data

# ── Builder API (interns, emits suffix on overflow) ──────────────────────

proc appendLineInfo*(b: var TokenBuf; file: FileId; line, col: int32;
                     comment = StrId(0)) =
  ## Append a `LineInfoLit` suffix (plus one `ExtendedSuffix` on overflow) to
  ## the head token / `openTag` just emitted — call it *immediately* after, so
  ## it lands as that head's trailing suffix. Implements no policy: the caller
  ## decides when the position actually changed (emit-on-change). No-op when
  ## `file` is invalid. The filename must already be interned in `b.pool.filenames`.
  ## Optionally attach `comment` (a `StrId` into `b.pool.strings`, `StrId(0)` =
  ## none) — a NIF `#…#` decoration on this head. A non-zero comment forces the
  ## overflow position layout and rides as one further `ExtendedSuffix` (a second
  ## only for ids past 2^28), so `rawLineInfo` recovers it unambiguously.
  if not file.isValid: return
  var line = line
  var col = col
  if line < 0'i32: line = 0'i32
  if col < 0'i32: col = 0'i32
  let cid = uint32(comment)
  if cid == 0'u32 and fitsCommonLineInfo(file, line, col):
    b.add NifToken(toX(LineInfoLit, encodeLineInfoCommon(file, line, col)))
  else:
    assert uint64(uint32(file)) <= LiFileMaskX, "too many files for LineInfoLit"
    let combined = encodeLineInfoWide(file, line, col)
    b.add NifToken(toX(LineInfoLit, uint32(combined and uint64(PayloadMask))))
    b.add extendedSuffixToken(uint32((combined shr PayloadBits) and uint64(PayloadMask)))
    if cid != 0'u32:
      b.add extendedSuffixToken(cid and PayloadMask)
      if uint64(cid) > uint64(PayloadMask):
        b.add extendedSuffixToken(cid shr PayloadBits)

proc appendLineInfo*(b: var TokenBuf; info: NifLineInfo) {.inline.} =
  appendLineInfo(b, info.file, info.line, info.col, info.comment)

template addSuffixIfNeeded(b: var TokenBuf; payload: uint64) =
  ## Emit an ExtendedSuffix token carrying `payload`'s high 28 bits if
  ## the value doesn't fit in 28 bits. `payload` is uint64 because the
  ## shifted-by-1 pool ids used by IntLit/UIntLit can overflow uint32.
  if payload > uint64(PayloadMask):
    b.add extendedSuffixToken(uint32(payload shr PayloadBits))

template lowBits(x: uint32): uint32 = x and PayloadMask
template lowBits(x: uint64): uint32 = uint32(x and uint64(PayloadMask))

proc addDotToken*(b: var TokenBuf) {.inline.} =
  ## Appends an empty dot placeholder.
  b.add dotToken()

proc addCharLit*(b: var TokenBuf; c: char) {.inline.} =
  ## Appends a character literal.
  b.add charToken(c)

proc encodeInlineStr(s: string): uint32 {.inline.} =
  ## Pack up to 3 bytes of `s` into the inline-string layout.
  result = StrInlineFlag or (uint32(s.len) shl StrLengthShift)
  for i in 0 ..< s.len:
    result = result or (uint32(byte(s[i])) shl (StrDataShift + uint32(i) * 8))

template addStringLike(b: var TokenBuf; kind: NifKind; s: string; pool: untyped) =
  ## Shared body for StrLit/Ident/Symbol/SymbolDef. Inlines bytes for
  ## `s.len <= 3`; otherwise interns in the given `pool` BiTable.
  if s.len <= StrInlineMaxLen:
    b.add NifToken(toX(kind, encodeInlineStr(s)))
  else:
    let id = uint32 pool.getOrIncl(s)
    let payload = uint64(id) shl 1               # bit 0 = 0 ⇒ pool ref
    b.add NifToken(toX(kind, lowBits(payload)))
    addSuffixIfNeeded(b, payload)

proc addStrLit*(b: var TokenBuf; s: string) =
  ## Appends a string literal, using inline storage when possible.
  addStringLike(b, StrLit, s, b.pool.strings)

proc addIdent*(b: var TokenBuf; s: string) =
  ## Appends an identifier, using inline storage when possible.
  addStringLike(b, Ident,  s, b.pool.strings)

proc addSymUse*(b: var TokenBuf; s: string) =
  ## Appends a symbol use, interning `s` when it does not fit inline.
  addStringLike(b, Symbol, s, b.pool.syms)

proc addSymDef*(b: var TokenBuf; s: string) =
  ## Appends a symbol definition, interning `s` when it does not fit inline.
  addStringLike(b, SymbolDef, s, b.pool.syms)

proc addInternedSymbol(b: var TokenBuf; kind: NifKind; id: SymId) =
  let s = b.pool.syms[id]
  if s.len <= StrInlineMaxLen:
    b.add NifToken(toX(kind, encodeInlineStr(s)))
  else:
    let payload = uint64(uint32(id)) shl 1
    b.add NifToken(toX(kind, lowBits(payload)))
    addSuffixIfNeeded(b, payload)

proc addSymDef*(b: var TokenBuf; id: SymId) =
  ## Emits a symbol definition already interned in `b.pool`.
  addInternedSymbol(b, SymbolDef, id)

proc addSymUse*(b: var TokenBuf; id: SymId) =
  ## Emit a symbol already interned in `b.pool`. Short symbols remain inline;
  ## longer symbols reuse `id` without a second hash-table lookup.
  addInternedSymbol(b, Symbol, id)

template emitChained(b: var TokenBuf; kind: NifKind; bits: uint64) =
  ## Emit a value carrier (kinded token plus 0/1/2 ExtendedSuffix
  ## tokens) holding `bits`. Picks the minimum chain length whose
  ## carrier width covers all the set bits.
  b.add NifToken(toX(kind, uint32(bits and uint64(PayloadMask))))
  if bits > uint64(PayloadMask):
    b.add extendedSuffixToken(uint32((bits shr PayloadBits) and
                                      uint64(PayloadMask)))
    if bits shr (PayloadBits * 2) != 0'u64:
      b.add extendedSuffixToken(uint32(bits shr (PayloadBits * 2)))

proc addIntLit*(b: var TokenBuf; v: int64) =
  ## Pure inline. Writer picks the shortest carrier whose SIGNED width holds `v`:
  ##   28-bit (one token)    for v in [-2^27, 2^27),
  ##   56-bit (two tokens)   for v in [-2^55, 2^55),
  ##   84-bit (three tokens) otherwise.
  ## The token COUNT must follow the chosen *signed* width, not `v`'s unsigned
  ## magnitude: a positive `v` whose top carrier bit is set (e.g. 2^27 ≤ v < 2^28)
  ## still needs the wider carrier, or the reader's sign-extend-from-width would
  ## read it back negative. (Hence this does NOT go through `emitChained`, which
  ## trims by magnitude — correct for unsigned/float, wrong for signed.)
  let bits = cast[uint64](v)
  b.add NifToken(toX(IntLit, uint32(bits and uint64(PayloadMask))))
  if v >= -(1'i64 shl 27) and v < (1'i64 shl 27):
    discard                                   # 28-bit: one token suffices
  elif v >= -(1'i64 shl 55) and v < (1'i64 shl 55):
    b.add extendedSuffixToken(uint32((bits shr PayloadBits) and uint64(PayloadMask)))
  else:
    b.add extendedSuffixToken(uint32((bits shr PayloadBits) and uint64(PayloadMask)))
    b.add extendedSuffixToken(uint32(bits shr (PayloadBits * 2)))

proc addUIntLit*(b: var TokenBuf; v: uint64) =
  ## Appends an unsigned integer literal using the shortest token chain.
  emitChained(b, UIntLit, v)

proc addFloatLit*(b: var TokenBuf; v: float64) =
  ## Appends a floating-point literal using its exact bit representation.
  emitChained(b, FloatLit, cast[uint64](v))

# ── Open / close tags (the only mutations that touch `openTags`) ─────────

proc openTag*(b: var TokenBuf; t: TagId) {.inline.} =
  ## Begin a new tagged subtree. The matching `closeTag` patches the
  ## emitted TagLit's jump in place (or splices in an `ExtendedSuffix`
  ## right after the TagLit if the body overflows the 19-bit jump field).
  if b.owner != nil: prepareMutation(b)
  ensureCap(b)
  b.openTags.add b.len
  b.data[b.len] = tagLitToken(t, 0)
  inc b.len

proc closeTag*(b: var TokenBuf) =
  ## Seal the most recently opened tag.
  if b.owner != nil: prepareMutation(b)
  assert b.openTags.len > 0, "closeTag with no matching openTag"
  let p = b.openTags.pop()
  # The TagLit at `p` may already carry trailing suffix tokens (a
  # `LineInfoLit`, and its own `ExtendedSuffix` on overflow, appended by
  # `openTag`). Those belong to the head — `tokenWidth` counts them — not to
  # the body, so the jump must exclude them. Find where the body starts.
  var headWidth = 1
  while p + headWidth < b.len and b.data[p + headWidth].kind >= ExtendedSuffix:
    inc headWidth
  let count = uint64(b.len - p - headWidth)
  if count <= uint64(InlineJumpCap):
    b.data[p].setJump uint32(count)
  else:
    # Body too big for the 19-bit jump field. Splice a jump `ExtendedSuffix`
    # immediately after the TagLit (before any line-info suffix), shifting the
    # rest right by one. The TagLit keeps the low 19 bits of `count`; the
    # suffix carries the high bits. `tokenWidth` already counts that spliced
    # suffix as head, so the jump still encodes just the body `count` (no +1).
    # Still-open outer ancestors have positions < p, so their stored positions
    # stay valid — they see the extra token in their own count when they close.
    let highBits = uint32(count shr JumpBits)
    let lowBits  = uint32(count and uint64(InlineJumpCap))
    assert highBits <= PayloadMask,
           "subtree exceeds 47-bit jump (size " & $count & ")"
    ensureCap(b)
    # Shift [p+1 .. b.len-1] right by one to make a hole at p+1.
    for i in countdown(b.len, p + 2):
      b.data[i] = b.data[i - 1]
    inc b.len
    b.data[p].setJump lowBits
    b.data[p + 1] = extendedSuffixToken(highBits)

template buildTree*(b: var TokenBuf; tag: TagId; body: untyped) =
  b.openTag tag
  body
  b.closeTag()

# ── Cursor construction ──────────────────────────────────────────────────

proc ensureOwner(b: var TokenBuf) {.inline.} =
  ## Lazily allocate the CursorOwner header, taking tracked references on
  ## both pool and tag pool so they stay alive for cursors even if the
  ## TokenBuf is freed first. Idempotent.
  if b.owner == nil:
    b.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
    b.owner.data = b.data
    b.owner.pool = b.pool
    b.owner.tags = b.tags
    GC_ref(b.owner.pool)
    GC_ref(b.owner.tags)
    b.owner.rc = 1

proc beginRead*(b: var TokenBuf): Cursor =
  assert b.openTags.len == 0, "beginRead with unclosed tags"
  ensureOwner(b)
  inc b.owner.rc
  result = Cursor(owner: b.owner,
                  p: addr(b.data[0]),
                  rem: b.len)

proc childCursor*(c: Cursor): Cursor =
  ## Returns a bounded cursor over the children of the `TagLit` at `c`.
  ## The input cursor is not advanced.
  assert c.kind == TagLit, "childCursor requires cursor at TagLit"
  result = c
  let headWidth = tokenWidth(result)
  result.p = cast[ptr NifToken](cast[uint](result.p) +
                                uint(headWidth) * sizeof(NifToken).uint)
  result.rem = int c.cursorJump

proc endRead*(c: var Cursor) {.inline.} =
  if c.owner != nil: decRcAndFree(c.owner)
  `=wasMoved`(c)

proc cursorAt*(b: var TokenBuf; i: int): Cursor =
  assert i >= 0 and i < b.len
  ensureOwner(b)
  inc b.owner.rc
  result = Cursor(owner: b.owner,
                  p: addr b.data[i],
                  rem: b.len - i)

proc cursorToPosition*(b: TokenBuf; c: Cursor): int {.inline.} =
  ## Token index of `c` within `b` (inverse of `cursorAt`). Stable key for
  ## per-expression tables (e.g. a register allocator's location map).
  (cast[int](c.p) - cast[int](b.data)) div sizeof(NifToken)

# ── Subtree copy ─────────────────────────────────────────────────────────

proc subtreeWidth*(c: Cursor): int =
  ## Total tokens occupied by the value at `c` (head + body).
  if c.kind == TagLit:
    int(tokenWidth(c).uint64 + c.cursorJump)
  else:
    tokenWidth(c)

proc reinternLineInfo(dest: var TokenBuf; c: Cursor): NifLineInfo =
  ## Map the source head's trailing line info into `dest`'s pools — the filename
  ## and, if present, the `#comment#` string. Returns `NoNifLineInfo` when none.
  let li = rawLineInfo(c)
  if not li.isValid: return NoNifLineInfo
  let fname = if c.pool != nil: c.pool.filenames[li.file] else: ""
  var comment = StrId(0)
  if uint32(li.comment) != 0'u32 and c.pool != nil:
    comment = dest.pool.strings.getOrIncl(c.pool.strings[li.comment])
  result = NifLineInfo(file: dest.pool.filenames.getOrIncl(fname),
                       line: li.line, col: li.col, comment: comment)

proc addAcrossPools(dest: var TokenBuf; c: var Cursor) =
  ## Internal: copy one value (atom or whole TagLit subtree) from `c`
  ## into `dest`, re-interning literals (via the literals pool), tag names
  ## (via the tag pool) and line-info filenames. Recurses through children.
  ## The trailing `LineInfoLit` is part of the head's width (skipped by
  ## `c.inc`), so it is re-emitted explicitly via `appendLineInfo`.
  let srcPool = c.pool
  let srcTags = c.tags
  let destLi = reinternLineInfo(dest, c)
  case c.kind
  of TagLit:
    let tagStr = srcTags.tags[c.cursorTagId]
    let newTag = dest.tags.tags.getOrIncl(tagStr)
    dest.openTag TagId(newTag)
    dest.appendLineInfo destLi          # right after the tag head
    c.into:
      while c.hasMore:
        addAcrossPools(dest, c)
    dest.closeTag()
  of StrLit:    dest.addStrLit  strVal(c, srcPool);  dest.appendLineInfo destLi; c.inc
  of IntLit:    dest.addIntLit  intVal(c);           dest.appendLineInfo destLi; c.inc
  of UIntLit:   dest.addUIntLit uintVal(c);          dest.appendLineInfo destLi; c.inc
  of FloatLit:  dest.addFloatLit floatVal(c);        dest.appendLineInfo destLi; c.inc
  of Symbol:    dest.addSymUse  symName(c, srcPool); dest.appendLineInfo destLi; c.inc
  of SymbolDef: dest.addSymDef  symName(c, srcPool); dest.appendLineInfo destLi; c.inc
  of Ident:     dest.addIdent   strVal(c, srcPool);  dest.appendLineInfo destLi; c.inc
  of CharLit:   dest.addCharLit charLit(c);          dest.appendLineInfo destLi; c.inc
  of DotToken:  dest.addDotToken();                  dest.appendLineInfo destLi; c.inc
  of LineInfoLit:
    dest.add c.load; c.inc              # defensive: not a valid head token
  of ExtendedSuffix:
    assert false, "ExtendedSuffix cannot be a head token"

proc addSubtree*(dest: var TokenBuf; c: Cursor) =
  ## Copy the subtree rooted at `c` into `dest`. When both pools AND
  ## both tag pools match, this is a single bulk `copyMem`; otherwise
  ## the source's literals and tag names are re-interned into `dest`'s
  ## pools token-by-token. Callers don't need to know which case applies.
  if dest.pool == c.pool and dest.tags == c.tags:
    let n = subtreeWidth(c)
    if dest.owner != nil: prepareMutation(dest)
    if dest.len + n > dest.cap:
      dest.cap = max(dest.cap div 2 + dest.cap, dest.len + n)
      dest.data = cast[Storage](realloc(dest.data, sizeof(NifToken) * dest.cap))
    copyMem(addr dest.data[dest.len], c.p, n * sizeof(NifToken))
    dest.len += n
  else:
    assert c.pool != nil and c.tags != nil
    var c = c
    addAcrossPools(dest, c)

proc addBufferSamePool*(dest: var TokenBuf; src: TokenBuf) =
  ## Append a closed buffer that shares `dest`'s literal and tag pools.
  ##
  ## The source is borrowed and remains usable. Matching pools make the
  ## append one bulk copy without constructing a read cursor.
  assert src.openTags.len == 0, "addBufferSamePool with unclosed source tags"
  assert dest.data != src.data, "cannot append a TokenBuf to itself"
  assert dest.pool == src.pool and dest.tags == src.tags,
         "addBufferSamePool requires matching pools"
  if src.len == 0:
    return
  if dest.owner != nil:
    prepareMutation(dest)
  if dest.len + src.len > dest.cap:
    dest.cap = max(dest.cap div 2 + dest.cap, dest.len + src.len)
    dest.data = cast[Storage](
      realloc(dest.data, sizeof(NifToken) * dest.cap))
  copyMem(addr dest.data[dest.len], src.data,
          src.len * sizeof(NifToken))
  dest.len += src.len

proc addBuffer*(dest: var TokenBuf; src: var TokenBuf) =
  ## Append all complete top-level values from `src` to `dest`.
  ##
  ## Matching pools permit one bulk copy. Otherwise values are re-interned
  ## through `addSubtree`. `dest` and `src` must be distinct buffers.
  assert src.openTags.len == 0, "addBuffer with unclosed source tags"
  assert dest.data != src.data, "cannot append a TokenBuf to itself"
  if src.len == 0:
    return
  if dest.pool == src.pool and dest.tags == src.tags:
    dest.addBufferSamePool(src)
  else:
    var c = src.beginRead()
    while c.hasMore:
      dest.addSubtree(c)
      c.skip()

# ── Self-test ────────────────────────────────────────────────────────────

when isMainModule:
  import std / formatfloat  # nimPreviewSlimSystem: `$`/echo of float64
  template test(a, b) =
    let xx = a
    if xx != b:
      echo "test failed ", astToStr(a), ": got ", xx, " expected ", b
      quit 1

  block bit_layout:
    let tp = newTagPool()
    let tFoo = tp.registerTag("foo")
    var n = tagLitToken(tFoo, 0)
    test n.kind, TagLit
    test n.rawTag, tFoo
    test n.rawJump, 0'u32
    n.setJump 42
    test n.rawTag, tFoo
    test n.rawJump, 42'u32

  block round_trip_sealed:
    # (foo (bar "hi" 42) 3.14) — pure TagLits, no closers in the buffer.
    var b = createTokenBuf(8)
    let tFoo = b.tags.registerTag("foo")
    let tBar = b.tags.registerTag("bar")
    b.buildTree tFoo:
      b.buildTree tBar:
        b.addStrLit "hi"
        b.addIntLit 42
      b.addFloatLit 3.14
    # 1 foo + (1 bar + 1 "hi" + 1 (small int 42)) + 3 (3.14 needs 3 tokens
    # for the full float64 bit pattern) = 7 tokens.
    test b.len, 7

    var c = b.beginRead()
    test c.kind, TagLit
    test c.cursorTagId, tFoo
    test c.cursorJump, 6'u64
    c.into:
      test c.kind, TagLit
      test c.cursorTagId, tBar
      test c.cursorJump, 2'u64
      c.into:
        test c.kind, StrLit
        test strVal(c), "hi"; c.inc
        test c.kind, IntLit
        test intVal(c), 42; c.inc
      test c.kind, FloatLit
      test floatVal(c), 3.14; c.inc

  block add_subtree_cross_pool:
    var src = createTokenBuf(4)
    let tFoo = src.tags.registerTag("foo")
    src.buildTree tFoo:
      src.addStrLit "hello"
      src.addIntLit 7
    var dest = createTokenBuf(4)
    test (src.pool == dest.pool), false
    test (src.tags == dest.tags), false
    var c = src.beginRead()
    dest.addSubtree c                       # re-interns via cross-pool path
    var d = dest.beginRead()
    test d.kind, TagLit
    test dest.tags.tagName(d.cursorTagId), "foo"
    d.into:
      test strVal(d), "hello"; d.inc
      test intVal(d), 7; d.inc

  block add_subtree_same_pools:
    # Both pools shared → bulk copyMem.
    let p = newPool()
    let tp = newTagPool()
    var src = createTokenBuf(4, p, tp)
    let tBar = tp.registerTag("bar")
    src.buildTree tBar:
      src.addStrLit "longish"   # > 3 bytes → pool path, exercises sharing
      src.addIntLit 9
    var dest = createTokenBuf(4, p, tp)
    test (src.pool == dest.pool), true
    test (src.tags == dest.tags), true
    let srcStrId = uint32 p.strings.getOrIncl("longish")
    var c = src.beginRead()
    dest.addSubtree c
    var d = dest.beginRead()
    test d.kind, TagLit
    d.into:
      # Same pool → bulk-copied; the token payload's pool id is preserved.
      test (d.load.uoperand shr 1), srcStrId
      d.inc
      d.inc

  block add_subtree_shared_literals_different_tags:
    # Real-world cross-format use: jsonnif and htmlnif share a literals
    # pool but keep separate tag pools.
    let p = newPool()
    var src = createTokenBuf(4, p)            # gets its own fresh tag pool
    let tJ = src.tags.registerTag("oconstr")
    src.buildTree tJ: src.addStrLit "sharedlonger"
    var dest = createTokenBuf(4, p)           # different tag pool
    var c = src.beginRead()
    dest.addSubtree c                          # must re-intern the tag…
    var d = dest.beginRead()
    test dest.tags.tagName(d.cursorTagId), "oconstr"
    # …but the literal "sharedlonger" stays at the same StrId in p.
    d.into:
      test (d.load.uoperand shr 1), uint32 p.strings.getOrIncl("sharedlonger")
      d.inc

  block inline_short_str:
    # Strings ≤ 3 bytes inline directly into the token. No pool touch.
    var b = createTokenBuf(8)
    b.addStrLit ""
    b.addStrLit "x"
    b.addStrLit "ab"
    b.addStrLit "abc"
    b.addStrLit "abcd"   # length 4 → pool
    test b.pool.strings.len, 1     # only "abcd" interned
    var c = b.beginRead()
    test isInlineLit(c), true;  test strVal(c, b.pool), "";    c.inc
    test isInlineLit(c), true;  test strVal(c, b.pool), "x";   c.inc
    test isInlineLit(c), true;  test strVal(c, b.pool), "ab";  c.inc
    test isInlineLit(c), true;  test strVal(c, b.pool), "abc"; c.inc
    test isInlineLit(c), false; test strVal(c, b.pool), "abcd"; c.inc

  block inline_int_chain:
    # 28-bit signed → 1 token; 56-bit → 2 tokens; > 56-bit → 3 tokens.
    # Pool.integers is gone — verify by tokens-per-value via len bookkeeping.
    var b = createTokenBuf(16)
    let beforeShort = b.len
    b.addIntLit 0;                  test b.len - beforeShort, 1
    let beforeSmall = b.len
    b.addIntLit 1_000_000;          test b.len - beforeSmall, 1
    let beforeWide = b.len
    b.addIntLit 1'i64 shl 40;       test b.len - beforeWide, 2
    let beforeVeryWide = b.len
    b.addIntLit high(int64);        test b.len - beforeVeryWide, 3
    let beforeNeg = b.len
    b.addIntLit low(int64);         test b.len - beforeNeg, 3

    var c = b.beginRead()
    test intVal(c), 0'i64;            c.inc
    test intVal(c), 1_000_000'i64;    c.inc
    test intVal(c), 1'i64 shl 40;     c.inc
    test intVal(c), high(int64);      c.inc
    test intVal(c), low(int64);       c.inc

  block inline_uint_chain:
    var b = createTokenBuf(8)
    b.addUIntLit 0'u64
    b.addUIntLit 1'u64 shl 27         # 2 tokens (just past 28-bit unsigned cap)
    b.addUIntLit 1'u64 shl 50         # 2 tokens
    b.addUIntLit high(uint64)         # 3 tokens
    var c = b.beginRead()
    test uintVal(c), 0'u64;            c.inc
    test uintVal(c), 1'u64 shl 27;     c.inc
    test uintVal(c), 1'u64 shl 50;     c.inc
    test uintVal(c), high(uint64);     c.inc

  block inline_float_chain:
    var b = createTokenBuf(6)
    b.addFloatLit 0.0     # all-zero bit pattern → 1 token
    b.addFloatLit 3.14    # all 64 bits used → 3 tokens
    b.addFloatLit -0.5    # 3 tokens
    var c = b.beginRead()
    test floatVal(c), 0.0;  c.inc
    test floatVal(c), 3.14; c.inc
    test floatVal(c), -0.5; c.inc

  block extended_suffix_jump:
    # Build a subtree larger than InlineJumpCap so `closeTag` splices an
    # ExtendedSuffix right after the TagLit.
    let big = int(InlineJumpCap) + 1   # 524288 body tokens
    var b = createTokenBuf(big + 4)
    let tT = b.tags.registerTag("t")
    b.buildTree tT:
      for _ in 0 ..< big: b.addDotToken()
    # After splice: 1 (TagLit) + 1 (suffix) + big body = big + 2
    test b.len, big + 2
    test b[0].kind, TagLit
    test b[0].rawTag, tT
    test b[1].kind, ExtendedSuffix

    var c = b.beginRead()
    test c.kind, TagLit                          # one load, no branch
    test c.cursorTagId, tT
    test c.cursorJump, uint64(big)               # body count (suffix is head)
    c.skip
    test c.rem, 0

  block line_info_common_and_overflow:
    var b = createTokenBuf(16)
    let fA = b.pool.filenames.getOrIncl("a.nim")
    let fB = b.pool.filenames.getOrIncl("b.nim")
    # common layout (fits 7/7/14)
    b.addStrLit "x";  b.appendLineInfo fA, 100'i32, 12'i32
    # a value with its own ExtendedSuffix chain, plus line info after it
    b.addIntLit 1'i64 shl 40;  b.appendLineInfo fA, 101'i32, 1'i32
    # overflow layout (line beyond 14 bits → wide)
    b.addStrLit "y";  b.appendLineInfo fB, 200000'i32, 5'i32
    # a head with no line info
    b.addCharLit 'z'

    var c = b.beginRead()
    block:
      test c.kind, StrLit; test strVal(c), "x"
      let li = rawLineInfo(c)
      test li.file.uint32, fA.uint32; test li.line, 100'i32; test li.col, 12'i32
      test lineInfoFile(c), "a.nim"
      c.inc
    block:
      test c.kind, IntLit; test intVal(c), 1'i64 shl 40
      let li = rawLineInfo(c)            # info read past the value's suffix
      test li.file.uint32, fA.uint32; test li.line, 101'i32; test li.col, 1'i32
      c.inc
    block:
      test c.kind, StrLit; test strVal(c), "y"
      let li = rawLineInfo(c)            # overflow (wide) layout
      test li.file.uint32, fB.uint32; test li.line, 200000'i32; test li.col, 5'i32
      test lineInfoFile(c), "b.nim"
      c.inc
    block:
      test c.kind, CharLit; test charLit(c), 'z'
      test rawLineInfo(c).isValid, false    # no line info present
      c.inc

  block line_info_with_comment:
    var b = createTokenBuf(16)
    let f = b.pool.filenames.getOrIncl("c.nim")
    let doc = b.pool.strings.getOrIncl("## a doc comment")
    # comment on a head whose position would fit the common layout → forced wide
    b.addStrLit "x";          b.appendLineInfo f, 10'i32, 4'i32, doc
    # comment alongside a value that has its OWN ExtendedSuffix chain
    b.addIntLit 1'i64 shl 40; b.appendLineInfo f, 11'i32, 2'i32, doc
    # line info without a comment still reports comment 0
    b.addStrLit "y";          b.appendLineInfo f, 12'i32, 1'i32

    var c = b.beginRead()
    block:
      test c.kind, StrLit; test strVal(c), "x"
      let li = rawLineInfo(c)
      test li.file.uint32, f.uint32; test li.line, 10'i32; test li.col, 4'i32
      test li.comment.uint32, doc.uint32
      test b.pool.strings[li.comment], "## a doc comment"
      c.inc
    block:
      test c.kind, IntLit; test intVal(c), 1'i64 shl 40
      let li = rawLineInfo(c)            # comment read past the value's suffix
      test li.line, 11'i32; test li.col, 2'i32; test li.comment.uint32, doc.uint32
      c.inc
    block:
      test c.kind, StrLit; test strVal(c), "y"
      let li = rawLineInfo(c)
      test li.line, 12'i32; test li.comment.uint32, 0'u32   # no comment
      c.inc

    # cross-pool copy of the first value must re-intern the comment string
    var dest = createTokenBuf(16)
    var cc = b.beginRead()
    dest.addSubtree cc                    # different pools → addAcrossPools
    var d = dest.beginRead()
    test d.kind, StrLit; test strVal(d), "x"
    let dli = rawLineInfo(d)
    test (dli.comment.uint32 != 0'u32), true
    test dest.pool.strings[dli.comment], "## a doc comment"
    test dli.line, 10'i32; test dli.col, 4'i32

  block line_info_on_tags_and_cross_pool:
    var src = createTokenBuf(8)
    let t = src.tags.registerTag("stmt")
    let f = src.pool.filenames.getOrIncl("m.nim")
    src.openTag t
    src.appendLineInfo f, 7'i32, 3'i32       # line info on the tag head
    src.addIntLit 42
    src.closeTag()
    # tag jump must count only the body (the IntLit), not the LineInfoLit head
    block:
      var c = src.beginRead()
      test c.kind, TagLit; test c.cursorJump, 1'u64
      let li = rawLineInfo(c)
      test li.line, 7'i32; test li.col, 3'i32; test lineInfoFile(c), "m.nim"
      c.into:
        test intVal(c), 42'i64; c.inc       # into must land past the tag head
    # cross-pool copy must re-intern the filename and preserve line/col
    var dest = createTokenBuf(8)
    block:
      var c = src.beginRead()
      dest.addSubtree c                       # different pools → addAcrossPools
    var d = dest.beginRead()
    test d.kind, TagLit
    test dest.tags.tagName(d.cursorTagId), "stmt"
    let dli = rawLineInfo(d)
    test dli.line, 7'i32; test dli.col, 3'i32
    test lineInfoFile(d), "m.nim"
    d.into:
      test intVal(d), 42'i64; d.inc

  echo "nifcore self-tests passed"

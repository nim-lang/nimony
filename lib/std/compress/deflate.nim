# (c) 2026 Andreas Rumpf
#
# DEFLATE (RFC 1951): the raw compressed format under gzip, zlib and HTTP's
# `Content-Encoding`. Containers — headers, trailers, checksums — are
# `std/compress/gzip`'s business; this module is only the bitstream.
#
#   var d = initDeflater(6)
#   var packed = ""
#   d.compress(part1, packed)
#   d.compress(part2, packed)
#   d.finish(packed)
#
#   var i = initInflater()
#   i.feed(packed)
#   var plain = ""
#   assert i.inflate(plain) == infDone
#
# ## Streaming, in both directions
#
# Both sides take their input in pieces of any size and keep enough state to
# continue where the last piece stopped. That is what lets an HTTP body be
# decoded as it arrives off the socket, and a streamed response compressed as
# the handler writes it, without either side ever holding the whole body.
#
# The inflater does not suspend in the middle of a symbol. It decodes one
# whole literal or one whole length/distance pair at a time, and when the
# input runs out part-way through one it rewinds to the symbol's start and
# reports `infNeedInput`; the next `feed` replays it. Rewinding is three
# integers, and it means there is no state for "half a symbol" that every step
# would otherwise have to be written to resume from. A dynamic block's header
# is treated the same way, as one indivisible step.
#
# ## Output is bounded by the caller
#
# `inflate` takes a `limit` and stops once it has produced that many bytes,
# even in the middle of a back-reference. A DEFLATE stream expands by up to
# 1032:1, so a decoder that always runs its input to the end lets the peer
# choose how much memory a 4 KiB read costs. With the limit the caller
# chooses, and `infOutputFull` says "call again", not "something is wrong".

import std / [bitops, algorithm]

const
  WSize = 32768
    ## The window: how far back a match may reach. Fixed by the format.
  WMask = WSize - 1
  MinMatch = 3
  MaxMatch = 258

  LenBase: array[29, int] = [
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
  LenExtra: array[29, int] = [
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
  DistBase: array[30, int] = [
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
    16385, 24577]
  DistExtra: array[30, int] = [
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]
  ClOrder: array[19, int] = [
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
    ## The order a dynamic header lists code-length code lengths in: the ones
    ## most likely to be zero last, so they can be left off.

proc reverseBits(code, len: int): int {.inline.} =
  ## Huffman codes are defined most-significant bit first but packed into a
  ## stream that is read least-significant bit first.
  result = 0
  var c = code
  for i in 0..<len:
    result = (result shl 1) or (c and 1)
    c = c shr 1

proc log2floor(x: int): int {.inline.} =
  31 - leadingZeroBits(uint32(x))

proc lengthSymbol(mlen: int): int =
  ## The length code for a match of `mlen`, from the shape of `LenBase`: four
  ## codes per extra bit, so the code is the bit length plus the two bits
  ## under the top one.
  let x = mlen - 3
  if x < 8: result = 257 + x
  elif mlen == MaxMatch: result = 285
  else:
    let hb = log2floor(x)
    result = 257 + 4 * (hb - 1) + ((x shr (hb - 2)) and 3)

proc distSymbol(dist: int): int =
  ## The distance code, by the same argument with two codes per extra bit.
  let x = dist - 1
  if x < 4: result = x
  else:
    let hb = log2floor(x)
    result = 2 * hb + ((x shr (hb - 1)) and 1)

# ================================================================ inflate ==

const
  FastBits = 9
    ## Codes this short or shorter decode with one table lookup. Almost every
    ## literal in real data is; the rest take the canonical slow path.
  FastSize = 1 shl FastBits
  FastMask = FastSize - 1

  SymNeedInput = -2
  SymBad = -1

type
  Huff = object
    ## A canonical Huffman code, as a decoder needs it.
    fast: array[FastSize, uint16]
      ## Indexed by the next `FastBits` bits of the stream: `sym shl 4 or
      ## len` for a code of at most `FastBits` bits, `0` for anything longer.
    counts: array[16, int]
      ## Codes per length, for the canonical slow path.
    symbols: array[288, int]
      ## Symbols ordered by (code length, symbol) — which is code order.

  BitReader = object
    inp: string        ## buffered input; `inp[ip..]` is not yet in `bitbuf`
    ip: int
    bitbuf: uint64     ## unread bits, the next one in bit 0
    bitcnt: int

  Checkpoint = object
    ip: int
    bitbuf: uint64
    bitcnt: int

  InflateStatus* = enum
    infNeedInput   ## everything fed so far was used; `feed` more
    infOutputFull  ## `limit` bytes were produced; call again for more
    infDone        ## the final block has ended; see `takeRest`
    infError       ## the stream is not valid DEFLATE

  Stage = enum
    stBlockHead, stStored, stCodes, stDone, stError

  Inflater* = object
    ## Decodes one DEFLATE stream, fed in pieces.
    br: BitReader
    stage: Stage
    final: bool        ## the current block is the last
    storedLeft: int    ## bytes of a stored block still to copy
    copyLeft: int      ## bytes of a back-reference still to copy...
    copyDist: int      ## ...from this far back
    win: string        ## the last `WSize` bytes produced, as a ring
    total: int         ## bytes produced over the whole stream
    lit, dist: Huff

proc build(h: var Huff; lens: openArray[int]): bool =
  ## Build the decoder for a code given by its lengths. `false` for an
  ## over-subscribed code — one with more codes than bit patterns, which no
  ## encoder produces. An incomplete code is accepted: the format allows one
  ## for a distance code that is barely used, and a pattern nobody was
  ## assigned is caught when it is decoded.
  for i in 0..15: h.counts[i] = 0
  for l in lens: inc h.counts[l]
  h.counts[0] = 0
  var left = 1
  for len in 1..15:
    left = left shl 1
    left = left - h.counts[len]
    if left < 0: return false
  var offs = default(array[16, int])
  for len in 1..14: offs[len + 1] = offs[len] + h.counts[len]
  for sym in 0..<lens.len:
    let l = lens[sym]
    if l != 0:
      h.symbols[offs[l]] = sym
      inc offs[l]
  for i in 0..<FastSize: h.fast[i] = 0'u16
  var code = 0
  var idx = 0
  for len in 1..FastBits:
    for k in 0..<h.counts[len]:
      let entry = uint16((h.symbols[idx] shl 4) or len)
      var j = reverseBits(code, len)
      while j < FastSize:
        h.fast[j] = entry
        j = j + (1 shl len)
      inc idx
      inc code
    code = code shl 1
  result = true

proc fill(br: var BitReader) {.inline.} =
  while br.bitcnt <= 56 and br.ip < br.inp.len:
    br.bitbuf = br.bitbuf or (uint64(ord(br.inp[br.ip])) shl br.bitcnt)
    inc br.ip
    br.bitcnt += 8

proc needBits(br: var BitReader; n: int): bool {.inline.} =
  if br.bitcnt < n: fill(br)
  result = br.bitcnt >= n

proc getBits(br: var BitReader; n: int): int {.inline.} =
  ## `n` bits that `needBits` has vouched for.
  result = int(br.bitbuf and ((1'u64 shl n) - 1'u64))
  br.bitbuf = br.bitbuf shr n
  br.bitcnt -= n

proc checkpoint(br: BitReader): Checkpoint {.inline.} =
  Checkpoint(ip: br.ip, bitbuf: br.bitbuf, bitcnt: br.bitcnt)

proc restore(br: var BitReader; cp: Checkpoint) {.inline.} =
  ## Valid because bits only ever enter `bitbuf` from `inp[ip]` onwards: a
  ## saved `ip` and the bits that were ahead of it are the whole position.
  br.ip = cp.ip
  br.bitbuf = cp.bitbuf
  br.bitcnt = cp.bitcnt

proc decodeSlow(br: var BitReader; h: Huff): int =
  ## One code, a bit at a time against the counts (`puff.c`'s method). Only
  ## reached for codes longer than `FastBits`, or near the end of the input.
  var code = 0
  var first = 0
  var index = 0
  for len in 1..15:
    if len > br.bitcnt: return SymNeedInput
    code = code or int((br.bitbuf shr (len - 1)) and 1'u64)
    let count = h.counts[len]
    if code - count < first:
      br.bitbuf = br.bitbuf shr len
      br.bitcnt -= len
      return h.symbols[index + (code - first)]
    index += count
    first = (first + count) shl 1
    code = code shl 1
  result = SymBad

proc decodeSym(br: var BitReader; h: Huff): int {.inline.} =
  fill(br)
  let e = int(h.fast[int(br.bitbuf and uint64(FastMask))])
  if e != 0:
    let n = e and 15
    # The bits past `bitcnt` read as zero, so a hit on a code longer than
    # what has arrived might be a different code once the rest is here.
    if n <= br.bitcnt:
      br.bitbuf = br.bitbuf shr n
      br.bitcnt -= n
      result = e shr 4
    else:
      result = SymNeedInput
  else:
    result = decodeSlow(br, h)

proc initInflater*(): Inflater =
  Inflater(br: BitReader(inp: "", ip: 0, bitbuf: 0'u64, bitcnt: 0),
           stage: stBlockHead, final: false, storedLeft: 0,
           copyLeft: 0, copyDist: 0, win: newString(WSize), total: 0,
           lit: default(Huff), dist: default(Huff))

proc feed*(s: var Inflater; input: openArray[char]) =
  ## More compressed bytes. Whatever an earlier `inflate` left unread stays
  ## ahead of them.
  let rest = s.br.inp.len - s.br.ip
  if s.br.ip > 0:
    for k in 0..<rest: s.br.inp[k] = s.br.inp[s.br.ip + k]
    s.br.inp.setLen rest
    s.br.ip = 0
  for c in input: s.br.inp.add c

proc buildFixed(s: var Inflater) =
  var lens = default(array[288, int])
  for i in 0..143: lens[i] = 8
  for i in 144..255: lens[i] = 9
  for i in 256..279: lens[i] = 7
  for i in 280..287: lens[i] = 8
  discard build(s.lit, lens)
  var dl = default(array[30, int])
  for i in 0..29: dl[i] = 5
  discard build(s.dist, dl)

proc readDynamic(s: var Inflater): int =
  ## A dynamic block's code tables. `1` when read, `0` when the input ran out
  ## part-way (the caller rewinds), `-1` for a header that is not valid.
  if not needBits(s.br, 14): return 0
  let hlit = getBits(s.br, 5) + 257
  let hdist = getBits(s.br, 5) + 1
  let hclen = getBits(s.br, 4) + 4
  if hlit > 286 or hdist > 30: return -1
  var lens = default(array[320, int])
  for i in 0..<hclen:
    if not needBits(s.br, 3): return 0
    lens[ClOrder[i]] = getBits(s.br, 3)
  var cl = default(Huff)
  if not build(cl, toOpenArray(lens, 0, 18)): return -1
  for i in 0..18: lens[i] = 0
  var i = 0
  while i < hlit + hdist:
    let sym = decodeSym(s.br, cl)
    if sym == SymNeedInput: return 0
    if sym < 0: return -1
    if sym < 16:
      lens[i] = sym
      inc i
    else:
      var rep = 0
      var val = 0
      if sym == 16:
        if i == 0: return -1
        val = lens[i - 1]
        if not needBits(s.br, 2): return 0
        rep = 3 + getBits(s.br, 2)
      elif sym == 17:
        if not needBits(s.br, 3): return 0
        rep = 3 + getBits(s.br, 3)
      else:
        if not needBits(s.br, 7): return 0
        rep = 11 + getBits(s.br, 7)
      if i + rep > hlit + hdist: return -1
      for k in 0..<rep:
        lens[i] = val
        inc i
  # A block that cannot end is not a block.
  if lens[256] == 0: return -1
  if not build(s.lit, toOpenArray(lens, 0, hlit - 1)): return -1
  if not build(s.dist, toOpenArray(lens, hlit, hlit + hdist - 1)): return -1
  result = 1

proc emit(s: var Inflater; dest: var string; c: char) {.inline.} =
  s.win[s.total and WMask] = c
  inc s.total
  dest.add c

proc inflate*(s: var Inflater; dest: var string;
              limit = high(int)): InflateStatus =
  ## Decode what has been fed, appending to `dest`, until the input runs out,
  ## the stream ends, or `limit` bytes have been appended.
  var budget = limit
  while true:
    case s.stage
    of stDone:
      return infDone
    of stError:
      return infError
    of stBlockHead:
      let cp = checkpoint(s.br)
      if not needBits(s.br, 3): return infNeedInput
      s.final = getBits(s.br, 1) == 1
      let kind = getBits(s.br, 2)
      if kind == 0:
        # Stored: the rest of this byte is padding, then LEN and its
        # complement.
        discard getBits(s.br, s.br.bitcnt and 7)
        if not needBits(s.br, 32):
          restore(s.br, cp)
          return infNeedInput
        let n = getBits(s.br, 16)
        let ncomp = getBits(s.br, 16)
        if n != (ncomp xor 0xFFFF):
          s.stage = stError
          return infError
        s.storedLeft = n
        s.stage = stStored
      elif kind == 1:
        buildFixed(s)
        s.stage = stCodes
      elif kind == 2:
        let r = readDynamic(s)
        if r == 0:
          restore(s.br, cp)
          return infNeedInput
        if r < 0:
          s.stage = stError
          return infError
        s.stage = stCodes
      else:
        s.stage = stError
        return infError
    of stStored:
      while s.storedLeft > 0:
        if budget == 0: return infOutputFull
        var c = '\0'
        if s.br.bitcnt >= 8:
          c = char(getBits(s.br, 8))
        elif s.br.ip < s.br.inp.len:
          c = s.br.inp[s.br.ip]
          inc s.br.ip
        else:
          return infNeedInput
        emit(s, dest, c)
        dec s.storedLeft
        dec budget
      s.stage = if s.final: stDone else: stBlockHead
    of stCodes:
      while s.copyLeft > 0:
        if budget == 0: return infOutputFull
        emit(s, dest, s.win[(s.total - s.copyDist) and WMask])
        dec s.copyLeft
        dec budget
      # The budget is tested after decoding, not before: a stream whose
      # output is exactly `limit` bytes still gets to see its end.
      let cp = checkpoint(s.br)
      let sym = decodeSym(s.br, s.lit)
      if sym < 256:
        if sym == SymNeedInput:
          restore(s.br, cp)
          return infNeedInput
        if sym < 0:
          s.stage = stError
          return infError
        if budget == 0:
          restore(s.br, cp)
          return infOutputFull
        emit(s, dest, char(sym))
        dec budget
      elif sym == 256:
        s.stage = if s.final: stDone else: stBlockHead
      else:
        let li = sym - 257
        if li >= 29:
          s.stage = stError
          return infError
        if not needBits(s.br, LenExtra[li]):
          restore(s.br, cp)
          return infNeedInput
        let mlen = LenBase[li] + getBits(s.br, LenExtra[li])
        let dsym = decodeSym(s.br, s.dist)
        if dsym == SymNeedInput:
          restore(s.br, cp)
          return infNeedInput
        if dsym < 0 or dsym >= 30:
          s.stage = stError
          return infError
        if not needBits(s.br, DistExtra[dsym]):
          restore(s.br, cp)
          return infNeedInput
        let dist = DistBase[dsym] + getBits(s.br, DistExtra[dsym])
        # Reaching back before the start of the stream is the one thing a
        # well-formed back-reference cannot do.
        if dist > s.total:
          s.stage = stError
          return infError
        s.copyLeft = mlen
        s.copyDist = dist

proc isDone*(s: Inflater): bool {.inline.} = s.stage == stDone

proc produced*(s: Inflater): int {.inline.} =
  ## Bytes decoded over the whole stream so far.
  s.total

proc takeRest*(s: var Inflater): string =
  ## Once `inflate` has answered `infDone`: the input that came after the
  ## stream — a container's trailer, or whatever follows that. The bits left
  ## in the last byte are padding and are dropped.
  result = ""
  discard getBits(s.br, s.br.bitcnt and 7)
  while s.br.bitcnt >= 8:
    result.add char(getBits(s.br, 8))
  for k in s.br.ip..<s.br.inp.len: result.add s.br.inp[k]
  s.br.inp.setLen 0
  s.br.ip = 0

# ================================================================ deflate ==

const
  HashBits = 15
  HashSize = 1 shl HashBits
  BlockSize = 65536
    ## Input per block. Big enough that a dynamic block's header (~60 bytes)
    ## is noise, small enough that the code tables follow the data.

type
  FlushMode = enum
    fmNone, fmSync, fmFinal

  Deflater* = object
    ## Compresses one DEFLATE stream, fed in pieces.
    level: int
    maxChain, niceLen, lazyLen: int
    buf: string
      ## Up to `WSize` bytes already compressed (the history matches reach
      ## into), then the input not compressed yet, from `pos`.
    pos: int
    base: int          ## the stream offset of `buf[0]`
    head: seq[uint32]
      ## By hash: the stream offset + 1 of the latest position with it, `0`
      ## for none. Offsets are kept mod 2^32 and every candidate is checked
      ## against the buffer, so a stale entry costs a probe, never a wrong
      ## match.
    prev: seq[uint32]
      ## By offset mod `WSize`: the previous position with the same hash.
    toks: seq[uint32]  ## the block being built: literals, `len shl 16 or dist`
    litFreq: array[286, int]
    distFreq: array[30, int]
    bitbuf: uint64
    bitcnt: int
    done: bool

proc initDeflater*(level = 6): Deflater =
  ## `level` trades speed for size as zlib's does: `0` stores, `1` is
  ## fastest, `9` tries hardest. `6` is what everybody's default means.
  let lv = if level < 0: 6 elif level > 9: 9 else: level
  # zlib's parameters for the same level, minus the knobs that only tune
  # its particular loop.
  const
    Chain: array[10, int] = [0, 4, 8, 32, 16, 32, 128, 256, 1024, 4096]
    Nice: array[10, int] = [0, 8, 16, 32, 16, 32, 128, 128, 258, 258]
    Lazy: array[10, int] = [0, 0, 0, 0, 4, 16, 16, 32, 128, 258]
  result = Deflater(level: lv, maxChain: Chain[lv], niceLen: Nice[lv],
                    lazyLen: Lazy[lv], buf: "", pos: 0, base: 0,
                    head: newSeq[uint32](if lv > 0: HashSize else: 0),
                    prev: newSeq[uint32](if lv > 0: WSize else: 0),
                    toks: @[], litFreq: default(array[286, int]),
                    distFreq: default(array[30, int]),
                    bitbuf: 0'u64, bitcnt: 0, done: false)

# -------------------------------------------------------------- bits out --

proc putBits(s: var Deflater; dest: var string; v: int; n: int) {.inline.} =
  s.bitbuf = s.bitbuf or (uint64(v) shl s.bitcnt)
  s.bitcnt += n
  while s.bitcnt >= 8:
    dest.add char(int(s.bitbuf and 0xFF'u64))
    s.bitbuf = s.bitbuf shr 8
    s.bitcnt -= 8

proc alignByte(s: var Deflater; dest: var string) {.inline.} =
  if s.bitcnt > 0:
    dest.add char(int(s.bitbuf and 0xFF'u64))
    s.bitbuf = 0'u64
    s.bitcnt = 0

# ------------------------------------------------------------- matching ---

proc hashAt(buf: string; p: int): int {.inline.} =
  let x = uint32(ord(buf[p])) or (uint32(ord(buf[p + 1])) shl 8) or
          (uint32(ord(buf[p + 2])) shl 16)
  result = int((x * 2654435761'u32) shr (32 - HashBits))

proc insert(s: var Deflater; p: int) {.inline.} =
  if p + MinMatch <= s.buf.len:
    let h = hashAt(s.buf, p)
    let a = s.base + p
    s.prev[a and WMask] = s.head[h]
    s.head[h] = uint32(uint(a + 1) and 0xFFFF_FFFF'u)

proc findMatch(s: Deflater; p: int; bestLen, bestDist: var int) =
  ## The longest earlier occurrence of what starts at `p`, walking at most
  ## `maxChain` candidates. `bestLen` stays below `MinMatch` when there is
  ## none worth having.
  bestLen = MinMatch - 1
  bestDist = 0
  let maxLen = min(MaxMatch, s.buf.len - p)
  if maxLen < MinMatch: return
  let cur = uint32(uint(s.base + p + 1) and 0xFFFF_FFFF'u)
  var cand = s.head[hashAt(s.buf, p)]
  var chain = s.maxChain
  var lastDist = 0
  while chain > 0:
    let dist = int(cur - cand)
    # Distances grow along a live chain; one that does not is a slot that
    # was overwritten by a newer position, and the chain ends there.
    if dist <= lastDist or dist > WSize or dist > p: break
    let q = p - dist
    if s.buf[q + bestLen] == s.buf[p + bestLen] and s.buf[q] == s.buf[p] and
        s.buf[q + 1] == s.buf[p + 1]:
      var l = 2
      while l < maxLen and s.buf[q + l] == s.buf[p + l]: inc l
      if l > bestLen:
        bestLen = l
        bestDist = dist
        if l >= s.niceLen or l == maxLen: break
    lastDist = dist
    cand = s.prev[(s.base + q) and WMask]
    dec chain

proc addLiteral(s: var Deflater; c: char) {.inline.} =
  s.toks.add uint32(ord(c))
  inc s.litFreq[ord(c)]

proc addMatch(s: var Deflater; mlen, dist: int) {.inline.} =
  s.toks.add (uint32(mlen) shl 16) or uint32(dist)
  inc s.litFreq[lengthSymbol(mlen)]
  inc s.distFreq[distSymbol(dist)]

proc tokenize(s: var Deflater; stop: int): int =
  ## Turn `buf[pos ..< stop]` into tokens. A match may run past `stop` into
  ## the input after it, so the answer is where tokenizing actually ended.
  var p = s.pos
  var carry = false
  var carryLen = 0
  var carryDist = 0
  let lazy = s.lazyLen > 0
  while p < stop:
    var len = 0
    var dist = 0
    if carry:
      len = carryLen
      dist = carryDist
      carry = false
    else:
      findMatch(s, p, len, dist)
    insert(s, p)
    if lazy and len >= MinMatch and len < s.lazyLen:
      # One step of lazy evaluation: a longer match starting at the next
      # byte is worth a literal here.
      var len2 = 0
      var dist2 = 0
      findMatch(s, p + 1, len2, dist2)
      if len2 > len:
        addLiteral(s, s.buf[p])
        inc p
        carry = true
        carryLen = len2
        carryDist = dist2
        continue
    if len >= MinMatch:
      addMatch(s, len, dist)
      for q in p + 1 ..< p + len: insert(s, q)
      p += len
    else:
      addLiteral(s, s.buf[p])
      inc p
  result = p

# -------------------------------------------------------------- huffman ---

proc buildLengths(freq: openArray[int]; lens: var openArray[int]; maxLen: int) =
  ## Length-limited Huffman code lengths for `freq`.
  ##
  ## Optimal lengths first, from the two-queue construction over the
  ## frequency-sorted symbols; then the counts are pushed under `maxLen`
  ## (miniz's adjustment), and the lengths dealt back out, longest to the
  ## rarest. Not package-merge, and not quite optimal when the limit bites —
  ## which takes thousands of symbols in a skewed block and costs a fraction
  ## of a percent when it does.
  for i in 0..<lens.len: lens[i] = 0
  var keys: seq[int] = @[]
  for sym in 0..<freq.len:
    if freq[sym] > 0: keys.add freq[sym] * 512 + sym
  let n = keys.len
  if n == 0:
    # Only a distance code is ever empty. Two one-bit codes rather than
    # none, which is what zlib sends and what every inflater accepts.
    lens[0] = 1
    lens[1] = 1
    return
  if n == 1:
    # A one-symbol code is not a complete code; give it a partner so every
    # decoder accepts it, the way zlib does.
    let sym = keys[0] and 511
    lens[sym] = 1
    lens[if sym == 0: 1 else: 0] = 1
    return
  sort(keys)
  var weight = newSeq[int](2 * n - 1)
  var parent = newSeq[int](2 * n - 1)
  for i in 0..<n: weight[i] = keys[i] shr 9
  var leaf = 0
  var node = n
  var next = n
  for k in 0..<n - 1:
    var pair = default(array[2, int])
    for j in 0..1:
      if leaf < n and (node >= next or weight[leaf] <= weight[node]):
        pair[j] = leaf
        inc leaf
      else:
        pair[j] = node
        inc node
    weight[next] = weight[pair[0]] + weight[pair[1]]
    parent[pair[0]] = next
    parent[pair[1]] = next
    inc next
  # Internal nodes were created in increasing order and the root is last,
  # so a parent's depth is always known before its children's.
  var depth = newSeq[int](2 * n - 1)
  for i in countdown(2 * n - 3, 0): depth[i] = depth[parent[i]] + 1
  var count = default(array[16, int])
  for i in 0..<n: inc count[min(depth[i], maxLen)]
  var total = 0
  for l in 1..maxLen: total += count[l] shl (maxLen - l)
  while total > (1 shl maxLen):
    dec count[maxLen]
    for l in countdown(maxLen - 1, 1):
      if count[l] > 0:
        dec count[l]
        count[l + 1] += 2
        break
    dec total
  var k = 0
  for l in countdown(maxLen, 1):
    for c in 0..<count[l]:
      lens[keys[k] and 511] = l
      inc k

proc makeCodes(lens: openArray[int]; codes: var openArray[int]) =
  ## Canonical codes for `lens`, already bit-reversed for the LSB-first
  ## writer.
  var count = default(array[16, int])
  for l in lens: inc count[l]
  count[0] = 0
  var next = default(array[16, int])
  var code = 0
  for l in 1..15:
    code = (code + count[l - 1]) shl 1
    next[l] = code
  for sym in 0..<lens.len:
    let l = lens[sym]
    if l != 0:
      codes[sym] = reverseBits(next[l], l)
      inc next[l]
    else:
      codes[sym] = 0

type
  BlockCodes = object
    litLen: array[286, int]
    litCode: array[286, int]
    distLen: array[30, int]
    distCode: array[30, int]

proc fixedCodes(): BlockCodes =
  result = default(BlockCodes)
  for i in 0..143: result.litLen[i] = 8
  for i in 144..255: result.litLen[i] = 9
  for i in 256..279: result.litLen[i] = 7
  for i in 280..285: result.litLen[i] = 8
  for i in 0..29: result.distLen[i] = 5
  makeCodes(result.litLen, result.litCode)
  makeCodes(result.distLen, result.distCode)

proc dataBits(s: Deflater; c: BlockCodes): int =
  ## The size of the block's symbols under `c`, extra bits included.
  result = 0
  for i in 0..285: result += s.litFreq[i] * c.litLen[i]
  for i in 257..285: result += s.litFreq[i] * LenExtra[i - 257]
  for i in 0..29: result += s.distFreq[i] * (c.distLen[i] + DistExtra[i])

type
  DynHeader = object
    hlit, hdist, hclen: int
    rle: seq[int]          ## code-length symbols, `sym or extra shl 8`
    clLen: array[19, int]
    clCode: array[19, int]

proc dynHeader(c: BlockCodes): DynHeader =
  ## The code-length encoding of `c`'s lengths: runs of zeros as 17/18, runs
  ## of a repeated length as 16.
  result = default(DynHeader)
  result.hlit = 286
  while result.hlit > 257 and c.litLen[result.hlit - 1] == 0: dec result.hlit
  result.hdist = 30
  while result.hdist > 1 and c.distLen[result.hdist - 1] == 0: dec result.hdist
  var all: seq[int] = @[]
  for i in 0..<result.hlit: all.add c.litLen[i]
  for i in 0..<result.hdist: all.add c.distLen[i]
  var clFreq = default(array[19, int])
  var i = 0
  while i < all.len:
    let v = all[i]
    var run = 1
    while i + run < all.len and all[i + run] == v: inc run
    if v == 0 and run >= 3:
      let r = min(run, 138)
      if r >= 11:
        result.rle.add 18 or ((r - 11) shl 8)
        inc clFreq[18]
      else:
        result.rle.add 17 or ((r - 3) shl 8)
        inc clFreq[17]
      i += r
    elif v != 0 and run >= 4:
      result.rle.add v
      inc clFreq[v]
      let r = min(run - 1, 6)
      result.rle.add 16 or ((r - 3) shl 8)
      inc clFreq[16]
      i += 1 + r
    else:
      result.rle.add v
      inc clFreq[v]
      inc i
  buildLengths(clFreq, result.clLen, 7)
  makeCodes(result.clLen, result.clCode)
  result.hclen = 19
  while result.hclen > 4 and result.clLen[ClOrder[result.hclen - 1]] == 0:
    dec result.hclen

proc headerBits(h: DynHeader): int =
  result = 5 + 5 + 4 + 3 * h.hclen
  for x in h.rle:
    let sym = x and 255
    result += h.clLen[sym]
    if sym == 16: result += 2
    elif sym == 17: result += 3
    elif sym == 18: result += 7

proc writeHeader(s: var Deflater; dest: var string; h: DynHeader) =
  putBits(s, dest, h.hlit - 257, 5)
  putBits(s, dest, h.hdist - 1, 5)
  putBits(s, dest, h.hclen - 4, 4)
  for i in 0..<h.hclen: putBits(s, dest, h.clLen[ClOrder[i]], 3)
  for x in h.rle:
    let sym = x and 255
    putBits(s, dest, h.clCode[sym], h.clLen[sym])
    if sym == 16: putBits(s, dest, x shr 8, 2)
    elif sym == 17: putBits(s, dest, x shr 8, 3)
    elif sym == 18: putBits(s, dest, x shr 8, 7)

proc writeTokens(s: var Deflater; dest: var string; c: BlockCodes) =
  for i in 0..<s.toks.len:
    let t = s.toks[i]
    if t < 256'u32:
      let x = int(t)
      putBits(s, dest, c.litCode[x], c.litLen[x])
    else:
      let mlen = int(t shr 16)
      let dist = int(t and 0xFFFF'u32)
      let ls = lengthSymbol(mlen)
      putBits(s, dest, c.litCode[ls], c.litLen[ls])
      let le = LenExtra[ls - 257]
      if le > 0: putBits(s, dest, mlen - LenBase[ls - 257], le)
      let ds = distSymbol(dist)
      putBits(s, dest, c.distCode[ds], c.distLen[ds])
      let de = DistExtra[ds]
      if de > 0: putBits(s, dest, dist - DistBase[ds], de)
  putBits(s, dest, c.litCode[256], c.litLen[256])

proc writeStored(s: var Deflater; dest: var string; a, b: int; final: bool) =
  ## `buf[a ..< b]` as stored blocks of at most 65535 bytes each.
  var p = a
  while true:
    let n = min(65535, b - p)
    let last = final and p + n == b
    putBits(s, dest, (if last: 1 else: 0), 3)
    alignByte(s, dest)
    dest.add char(n and 0xFF)
    dest.add char(n shr 8)
    dest.add char((n xor 0xFFFF) and 0xFF)
    dest.add char((n xor 0xFFFF) shr 8)
    for k in p ..< p + n: dest.add s.buf[k]
    p += n
    if p >= b: break

proc emitBlock(s: var Deflater; dest: var string; a, b: int; final: bool) =
  ## One block for `buf[a ..< b]`, whose tokens are in `toks`, in whichever
  ## of the three encodings is smallest for it.
  inc s.litFreq[256]
  var dyn = default(BlockCodes)
  buildLengths(s.litFreq, dyn.litLen, 15)
  buildLengths(s.distFreq, dyn.distLen, 15)
  makeCodes(dyn.litLen, dyn.litCode)
  makeCodes(dyn.distLen, dyn.distCode)
  let hdr = dynHeader(dyn)
  let fixed = fixedCodes()
  let dynCost = 3 + headerBits(hdr) + dataBits(s, dyn)
  let fixedCost = 3 + dataBits(s, fixed)
  let storedCost = (b - a) * 8 + ((b - a) div 65535 + 1) * 40
  let fin = if final: 1 else: 0
  if s.level == 0 or (storedCost < dynCost and storedCost < fixedCost):
    writeStored(s, dest, a, b, final)
  elif fixedCost <= dynCost:
    putBits(s, dest, fin or (1 shl 1), 3)
    writeTokens(s, dest, fixed)
  else:
    putBits(s, dest, fin or (2 shl 1), 3)
    writeHeader(s, dest, hdr)
    writeTokens(s, dest, dyn)
  s.toks.setLen 0
  for i in 0..<s.litFreq.len: s.litFreq[i] = 0
  for i in 0..<s.distFreq.len: s.distFreq[i] = 0

proc slide(s: var Deflater) =
  ## Drop what no match can reach any more. Only once a whole window's worth
  ## has piled up, so the copy is paid once per `WSize` bytes, not per call.
  let drop = s.pos - WSize
  if drop >= WSize:
    let rest = s.buf.len - drop
    for k in 0..<rest: s.buf[k] = s.buf[drop + k]
    s.buf.setLen rest
    s.base += drop
    s.pos -= drop

proc process(s: var Deflater; dest: var string; mode: FlushMode) =
  while true:
    let pending = s.buf.len - s.pos
    # Without a flush, wait until a whole block plus a full match's worth of
    # lookahead is here: a block cut short is a worse block, and a match cut
    # short at the end of the input is a shorter match.
    if mode == fmNone and pending < BlockSize + MaxMatch: return
    if pending == 0: break
    let a = s.pos
    var b = min(s.buf.len, a + BlockSize)
    if s.level > 0: b = tokenize(s, b)
    s.pos = b
    emitBlock(s, dest, a, b, mode == fmFinal and b == s.buf.len)
    slide(s)
    if mode == fmFinal and s.pos == s.buf.len:
      alignByte(s, dest)
      s.done = true
      return
  if mode == fmFinal:
    # Nothing left, and the last block sent was not marked final: a final
    # fixed block with nothing in it but its end.
    putBits(s, dest, 1 or (1 shl 1), 3)
    putBits(s, dest, 0, 7)
    alignByte(s, dest)
    s.done = true
  elif mode == fmSync:
    # An empty stored block. Its LEN is byte-aligned, so everything before
    # it is decodable by a peer that has only what was sent so far.
    putBits(s, dest, 0, 3)
    alignByte(s, dest)
    dest.add "\x00\x00\xFF\xFF"

proc compress*(s: var Deflater; input: openArray[char]; dest: var string) =
  ## Compress `input`, appending to `dest` whatever whole blocks are ready.
  ## Most of a small input stays buffered until `flush` or `finish`.
  var i = 0
  while i < input.len:
    # In block-sized pieces, so the buffer stays bounded by a few blocks
    # however much is passed at once.
    let n = min(input.len - i, BlockSize)
    for k in i ..< i + n: s.buf.add input[k]
    i += n
    process(s, dest, fmNone)

proc flush*(s: var Deflater; dest: var string) =
  ## Compress everything buffered and byte-align the output, so that what
  ## `dest` holds decodes to everything given so far (a zlib "sync flush").
  ## Costs a block boundary and five bytes; matches still reach back across
  ## it.
  if not s.done: process(s, dest, fmSync)

proc finish*(s: var Deflater; dest: var string) =
  ## End the stream. Nothing may be compressed after this.
  if not s.done: process(s, dest, fmFinal)

proc isFinished*(s: Deflater): bool {.inline.} = s.done

# ------------------------------------------------------------- one shot ---

proc deflate*(data: openArray[char]; level = 6): string =
  ## `data` as a raw DEFLATE stream.
  result = ""
  var d = initDeflater(level)
  d.compress(data, result)
  d.finish(result)

proc inflate*(data: openArray[char]; dest: var string;
              limit = high(int)): InflateStatus =
  ## Decode a whole raw DEFLATE stream from `data`, appending to `dest`.
  ## `infDone` on success; `infNeedInput` means `data` was cut short;
  ## `infOutputFull` that the result would exceed `limit` bytes.
  var s = initInflater()
  s.feed(data)
  result = s.inflate(dest, limit)

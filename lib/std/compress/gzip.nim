# (c) 2026 Andreas Rumpf
#
# The containers DEFLATE travels in: gzip (RFC 1952) and zlib (RFC 1950).
# They are what HTTP's `Content-Encoding: gzip` and `Content-Encoding:
# deflate` actually mean — the latter is zlib, not raw DEFLATE, despite the
# name, and enough servers got that wrong that a decoder has to accept both.
#
#   let packed = compress(page, fmtGzip)
#   let page2 = uncompress(packed)           # detects gzip, zlib or raw
#
#   var e = initEncoder(fmtGzip)             # the same, a piece at a time
#   e.compress(part1, packed)
#   e.compress(part2, packed)
#   e.finish(packed)
#
#   var d = initDecoder()
#   d.feed(packed)
#   var plain = ""
#   case d.decode(plain, limit = 1 shl 20)
#   of infDone: discard
#   ...
#
# The streaming objects are the real API and the one-shot procs are three
# lines over them. A container's header and trailer are parsed as bytes
# arrive, like the DEFLATE between them, so a decoder never needs the whole
# body before it can produce the first byte of output.

import ./deflate
import ./checksums

export deflate.InflateStatus

type
  Format* = enum
    fmtRaw     ## DEFLATE with no container
    fmtZlib    ## 2-byte header, Adler-32 trailer: HTTP's `deflate`
    fmtGzip    ## 10-byte header, CRC-32 and length trailer: HTTP's `gzip`
    fmtDetect
      ## Decoding only: gzip or zlib by their headers, else raw. What a
      ## decoder for HTTP's `deflate` wants, since both forms are in the wild.

# ================================================================ encoder ==

type
  Encoder* = object
    ## One compressed stream in a container, built a piece at a time.
    fmt: Format
    level: int
    d: Deflater
    crc: Crc32
    adler: Adler32
    size: int
    started: bool

proc initEncoder*(fmt = fmtGzip; level = 6): Encoder =
  ## `fmtDetect` is not a format anything can be written in; it is taken to
  ## mean gzip.
  Encoder(fmt: (if fmt == fmtDetect: fmtGzip else: fmt), level: level,
          d: initDeflater(level), crc: initCrc32(), adler: initAdler32(),
          size: 0, started: false)

proc writeHeader(e: var Encoder; dest: var string) =
  case e.fmt
  of fmtGzip:
    # No name, no mtime: a timestamp makes identical bodies differ, which
    # defeats every cache and every ETag computed over the encoded bytes.
    # XFL says which extreme the level was; OS 255 is "unknown".
    let xfl = if e.level >= 9: 2 elif e.level == 1: 4 else: 0
    dest.add "\x1F\x8B\x08\x00\x00\x00\x00\x00"
    dest.add char(xfl)
    dest.add '\xFF'
  of fmtZlib:
    # CM 8 with a 32K window, and FLEVEL as zlib reports it. FCHECK makes the
    # pair a multiple of 31, which is the header's whole integrity check.
    let flevel = if e.level <= 1: 0 elif e.level <= 5: 1 elif e.level == 6: 2 else: 3
    let cmf = 0x78
    var flg = flevel shl 6
    flg = flg + (31 - (cmf * 256 + flg) mod 31) mod 31
    dest.add char(cmf)
    dest.add char(flg)
  else:
    discard
  e.started = true

proc compress*(e: var Encoder; input: openArray[char]; dest: var string) =
  ## Compress `input`, appending whatever output is ready to `dest`.
  if not e.started: writeHeader(e, dest)
  case e.fmt
  of fmtGzip: e.crc = update(e.crc, input)
  of fmtZlib: e.adler = update(e.adler, input)
  else: discard
  e.size += input.len
  e.d.compress(input, dest)

proc flush*(e: var Encoder; dest: var string) =
  ## Make everything compressed so far decodable from what `dest` holds. See
  ## `deflate.flush`.
  if not e.started: writeHeader(e, dest)
  e.d.flush(dest)

proc addLE32(dest: var string; x: uint32) =
  for k in 0..3: dest.add char(int((x shr (8 * k)) and 0xFF'u32))

proc addBE32(dest: var string; x: uint32) =
  for k in countdown(3, 0): dest.add char(int((x shr (8 * k)) and 0xFF'u32))

proc finish*(e: var Encoder; dest: var string) =
  ## End the stream and write the trailer. Idempotent.
  if e.d.isFinished: return
  if not e.started: writeHeader(e, dest)
  e.d.finish(dest)
  case e.fmt
  of fmtGzip:
    addLE32(dest, finish(e.crc))
    addLE32(dest, uint32(uint(e.size) and 0xFFFF_FFFF'u))
  of fmtZlib:
    addBE32(dest, finish(e.adler))
  else:
    discard

proc compress*(data: openArray[char]; fmt = fmtGzip; level = 6): string =
  ## `data`, compressed, in one call.
  result = ""
  var e = initEncoder(fmt, level)
  e.compress(data, result)
  e.finish(result)

# ================================================================ decoder ==

const
  MaxHeaderLen = 65536
    ## How much gzip header — file name, comment, extra field — is buffered
    ## before the stream is refused. Nothing that legitimately travels over
    ## HTTP comes close, and without a ceiling a name that never ends is a
    ## buffer the peer grows for as long as it likes.

type
  DecStage = enum
    dsHeader, dsBody, dsTrailer, dsDone, dsError

  Decoder* = object
    ## One compressed stream in a container, decoded as it arrives.
    fmt: Format
    stage: DecStage
    pending: string      ## header or trailer bytes not yet complete
    inf: Inflater
    crc: Crc32
    adler: Adler32
    members: int         ## gzip members finished so far

proc initDecoder*(fmt = fmtDetect): Decoder =
  Decoder(fmt: fmt, stage: dsHeader, pending: "", inf: initInflater(),
          crc: initCrc32(), adler: initAdler32(), members: 0)

proc feed*(d: var Decoder; input: openArray[char]) =
  ## More compressed bytes.
  if d.stage == dsBody:
    d.inf.feed(input)
  else:
    for c in input: d.pending.add c

proc byteAt(s: string; i: int): int {.inline.} = ord(s[i])

proc gzipHeaderLen(h: string): int =
  ## The length of the gzip header at the start of `h`: `0` while it is
  ## incomplete, `-1` when it is not a gzip header this decoder can read.
  if h.len >= 1 and byteAt(h, 0) != 0x1F: return -1
  if h.len >= 2 and byteAt(h, 1) != 0x8B: return -1
  if h.len >= 3 and byteAt(h, 2) != 8: return -1       # CM: only DEFLATE exists
  if h.len < 10: return 0
  let flg = byteAt(h, 3)
  if (flg and 0xE0) != 0: return -1                     # reserved bits
  var p = 10
  if (flg and 4) != 0:                                  # FEXTRA
    if h.len < p + 2: return 0
    p += 2 + (byteAt(h, p) or (byteAt(h, p + 1) shl 8))
  for bit in [8, 16]:                                   # FNAME, FCOMMENT
    if (flg and bit) != 0:
      while true:
        if p >= h.len: return 0
        inc p
        if h[p - 1] == '\0': break
  if (flg and 2) != 0: p += 2                           # FHCRC
  result = if p <= h.len: p else: 0

proc isZlibHeader(a, b: int): bool {.inline.} =
  ## CM 8, a window that DEFLATE allows, and the FCHECK that makes the pair a
  ## multiple of 31. A raw stream passes this by accident about once in a
  ## thousand — which is the price every decoder of HTTP `deflate` pays.
  (a and 0x0F) == 8 and (a shr 4) <= 7 and (a * 256 + b) mod 31 == 0

proc startBody(d: var Decoder; skip: int) =
  ## The header is `pending[0 ..< skip]`; what follows it is DEFLATE.
  d.stage = dsBody
  d.inf.feed(toOpenArray(d.pending, skip, d.pending.len - 1))
  d.pending.setLen 0

proc readHeader(d: var Decoder): bool =
  ## `false` when more input is needed or the stream failed; the stage says
  ## which.
  result = false
  if d.fmt == fmtDetect:
    if d.pending.len < 2: return false
    let a = byteAt(d.pending, 0)
    let b = byteAt(d.pending, 1)
    d.fmt =
      if a == 0x1F and b == 0x8B: fmtGzip
      elif isZlibHeader(a, b): fmtZlib
      else: fmtRaw
  case d.fmt
  of fmtGzip:
    let n = gzipHeaderLen(d.pending)
    if n < 0 or (n == 0 and d.pending.len > MaxHeaderLen):
      d.stage = dsError
    elif n > 0:
      startBody(d, n)
      result = true
  of fmtZlib:
    if d.pending.len < 2: return false
    let a = byteAt(d.pending, 0)
    let b = byteAt(d.pending, 1)
    # FDICT asks for a preset dictionary nobody over HTTP ever has.
    if not isZlibHeader(a, b) or (b and 0x20) != 0:
      d.stage = dsError
    else:
      startBody(d, 2)
      result = true
  else:
    startBody(d, 0)
    result = true

proc readLE32(s: string; i: int): uint32 =
  result = 0'u32
  for k in 0..3: result = result or (uint32(byteAt(s, i + k)) shl (8 * k))

proc readBE32(s: string; i: int): uint32 =
  result = 0'u32
  for k in 0..3: result = (result shl 8) or uint32(byteAt(s, i + k))

proc readTrailer(d: var Decoder): bool =
  let need = if d.fmt == fmtGzip: 8 elif d.fmt == fmtZlib: 4 else: 0
  if d.pending.len < need: return false
  var ok = true
  if d.fmt == fmtGzip:
    ok = readLE32(d.pending, 0) == finish(d.crc) and
         readLE32(d.pending, 4) == uint32(uint(d.inf.produced) and 0xFFFF_FFFF'u)
  elif d.fmt == fmtZlib:
    ok = readBE32(d.pending, 0) == finish(d.adler)
  if not ok:
    d.stage = dsError
    return false
  let rest = d.pending.len - need
  for k in 0..<rest: d.pending[k] = d.pending[need + k]
  d.pending.setLen rest
  d.stage = dsDone
  inc d.members
  result = true

proc decode*(d: var Decoder; dest: var string;
             limit = high(int)): InflateStatus =
  ## Decode what has been fed, appending to `dest`: `infNeedInput` when the
  ## input ran out, `infOutputFull` after `limit` bytes, `infDone` once the
  ## stream and its trailer have been read and checked.
  ##
  ## A stream that ends without `infDone` is truncated. That is the caller's
  ## call to make — only it knows the input has ended.
  let start = dest.len
  while true:
    case d.stage
    of dsError:
      return infError
    of dsHeader:
      if not readHeader(d):
        return (if d.stage == dsError: infError else: infNeedInput)
    of dsBody:
      let before = dest.len
      let st = d.inf.inflate(dest, limit - (dest.len - start))
      if dest.len > before:
        let fresh = toOpenArray(dest, before, dest.len - 1)
        if d.fmt == fmtGzip: d.crc = update(d.crc, fresh)
        elif d.fmt == fmtZlib: d.adler = update(d.adler, fresh)
      if st != infDone:
        if st == infError: d.stage = dsError
        return st
      d.pending = d.inf.takeRest()
      d.stage = dsTrailer
    of dsTrailer:
      if not readTrailer(d):
        return (if d.stage == dsError: infError else: infNeedInput)
    of dsDone:
      if d.pending.len == 0: return infDone
      if d.fmt == fmtGzip:
        # A gzip file may be several members back to back, and decodes to
        # their concatenation (RFC 1952 §2.2).
        d.stage = dsHeader
        d.inf = initInflater()
        d.crc = initCrc32()
      else:
        # Bytes after the end of the only stream there can be.
        d.stage = dsError

proc isDone*(d: Decoder): bool {.inline.} =
  ## Whether a complete stream has been decoded and checked. Asked once the
  ## input has ended: `false` then means it was cut short.
  d.stage == dsDone and d.pending.len == 0

proc uncompress*(data: openArray[char]; fmt = fmtDetect;
                 limit = high(int)): string {.raises.} =
  ## `data`, decompressed. `SyntaxError` for a stream that is corrupt or cut
  ## short, `ContentTooLong` when the result would pass `limit` bytes.
  ##
  ## Pass a `limit` whenever `data` came from someone else: it is the only
  ## thing between a kilobyte of input and a gigabyte of output.
  result = ""
  var d = initDecoder(fmt)
  d.feed(data)
  let st = d.decode(result, limit)
  if st == infOutputFull: raise ContentTooLong
  if st != infDone or not d.isDone: raise SyntaxError

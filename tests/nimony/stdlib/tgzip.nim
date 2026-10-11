# std/compress: checksums, raw DEFLATE, and the gzip/zlib containers.
# Interoperability is checked against bytes GNU gzip and zlib produced; the
# rest is round trips, streaming in awkward pieces, and the failures a
# decoder has to notice.

import std / [syncio, assertions, compress/checksums, compress/deflate,
              compress/gzip]

proc corpus(): string =
  ## Text with long and short repeats, and a stretch of noise that has none.
  result = ""
  for i in 0..<3000:
    result.add "line " & $i & ": the quick brown fox jumps over the lazy dog\n"
  var x = 7'u32
  for i in 0..<20000:
    x = x * 1103515245'u32 + 12345'u32
    result.add char(int((x shr 16) and 255'u32))
  for i in 0..<70000: result.add 'z'

proc checksums() =
  assert crc32("The quick brown fox jumps over the lazy dog") == 0x414FA339'u32
  assert adler32("Wikipedia") == 0x11E60398'u32
  assert crc32("") == 0'u32
  assert adler32("") == 1'u32
  var c = initCrc32()
  c = update(c, "The quick brown ")
  c = update(c, "fox jumps over the lazy dog")
  assert finish(c) == 0x414FA339'u32
  echo "checksums ok"

proc foreign() {.raises.} =
  # `gzip -9` of a file named hi.txt: FNAME set, mtime set.
  let gz = "\x1F\x8B\x08\x08\x6F\x63\xC7\x6A\x02\x03\x68\x69\x2E\x74\x78\x74" &
           "\x00\xCB\x48\xCD\xC9\xC9\x57\xC8\x40\x27\x75\x14\xD2\xAB\x32\x0B" &
           "\x14\xB9\x00\x95\xDA\x2F\xA5\x1F\x00\x00\x00"
  assert uncompress(gz) == "hello hello hello hello, gzip!\n"
  # zlib.compress(..., 9)
  let zl = "\x78\xDA\xCB\x48\xCD\xC9\xC9\x57\xC8\x40\x27\x75\x14\xAA\x72\x32" &
           "\x93\x14\xB9\x00\xB7\xD9\x0A\xD9"
  assert uncompress(zl) == "hello hello hello hello, zlib!\n"
  assert uncompress(zl, fmtZlib) == "hello hello hello hello, zlib!\n"
  # Two gzip members decode to their concatenation.
  assert uncompress(compress("hello ") & compress("world")) == "hello world"
  echo "foreign ok"

proc roundTrips() {.raises.} =
  let data = corpus()
  for fmt in [fmtRaw, fmtZlib, fmtGzip]:
    for level in [0, 1, 4, 6, 9]:
      let packed = compress(data, fmt, level)
      assert uncompress(packed, fmt) == data
      if fmt != fmtRaw: assert uncompress(packed) == data   # detected
      if level > 0: assert packed.len < data.len div 3
    assert uncompress(compress("", fmt), fmt) == ""
    assert uncompress(compress("a", fmt), fmt) == "a"
  echo "round trips ok"

proc streaming() =
  ## Compressed in odd-sized pieces with a flush in the middle, decoded one
  ## byte of input at a time with a tiny output budget: every place a
  ## symbol, a header or a trailer can be cut in two is cut in two.
  let data = corpus()
  var e = initEncoder(fmtGzip)
  var packed = ""
  var i = 0
  var step = 1
  while i < data.len:
    let n = min(step, data.len - i)
    e.compress(toOpenArray(data, i, i + n - 1), packed)
    if i < 5000 and i + n >= 5000:
      e.flush(packed)
      # Everything so far is decodable from what has been sent.
      var d0 = initDecoder()
      d0.feed(packed)
      var part = ""
      discard d0.decode(part)
      assert part.len == i + n
    i += n
    step = step * 3 + 1
    if step > 40000: step = 7
  e.finish(packed)

  var d = initDecoder()
  var o = ""
  var k = 0
  var st = infNeedInput
  while true:
    st = d.decode(o, 5)
    if st == infNeedInput:
      if k >= packed.len: break
      d.feed(toOpenArray(packed, k, k))
      inc k
    elif st != infOutputFull:
      break
  assert st == infDone
  assert d.isDone
  assert o == data
  echo "streaming ok"

proc failures() =
  let data = corpus()
  let packed = compress(data)
  var e = Success
  # Cut short: the decoder cannot tell a prefix from a whole without the
  # trailer, which is why `uncompress` insists on one.
  try: discard uncompress(toOpenArray(packed, 0, packed.len - 9))
  except ErrorCode as x: e = x
  assert e == SyntaxError
  # One bit flipped in the CRC.
  var bad = packed
  bad[bad.len - 6] = char(ord(bad[bad.len - 6]) xor 1)
  e = Success
  try: discard uncompress(bad)
  except ErrorCode as x: e = x
  assert e == SyntaxError
  # Output past the limit is refused rather than produced.
  e = Success
  try: discard uncompress(packed, limit = 1000)
  except ErrorCode as x: e = x
  assert e == ContentTooLong
  # Exactly at the limit is fine.
  e = Success
  try: discard uncompress(packed, limit = data.len)
  except ErrorCode as x: e = x
  assert e == Success
  # Not DEFLATE at all.
  var out1 = ""
  assert inflate("\xFF\xFF\xFF\xFF", out1) == infError
  echo "failures ok"

checksums()
try:
  foreign()
  roundTrips()
except ErrorCode as e:
  echo "unexpected ", e
streaming()
failures()

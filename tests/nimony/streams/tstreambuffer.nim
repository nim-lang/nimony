import std/[streamio, syncio, assertions, paths, dirs]
import std/compress/gzip

type PrefixAdapter = ref object of Adapter
  left: int

method run(a: PrefixAdapter; src: openArray[char]; used: var int;
           dest: var string; final: bool) {.raises.} =
  # A bounded body delivered in small pieces, leaving the next body below it.
  used = min(min(a.left, src.len), 2)
  for i in 0..<used: dest.add src[i]
  a.left -= used
  if final and a.left > 0: raise EndOfStreamError

method hasMore(a: PrefixAdapter): bool = a.left > 0

proc contents(r: Reader): string =
  result = ""
  for c in r.peek: result.add c

proc main() {.raises.} =
  block plain:
    var r = openReader("first\r\nsecond\nlast")
    assert r.buffered == 0 and r.peek.len == 0
    r.consume(0)
    assert r.fill() == 18
    assert r.buffered == 18 and contents(r) == "first\r\nsecond\nlast"
    r.consume(7)
    # Source end does not discard the unread tail.
    assert r.fill() == 0
    assert r.buffered == 11 and contents(r) == "second\nlast"
    assert r.readLine() == "second"
    assert contents(r) == "last"
    assert r.readAll() == "last"
    assert r.fill() == 0 and r.fill() == 0
    assert r.buffered == 0 and r.peek.len == 0
    echo "plain buffer and EOF"

  block adapted:
    var r = openReader("abcdefgh")
    r.setAdapter PrefixAdapter(left: 5)
    assert r.fill() == 2 and contents(r) == "ab"
    r.consume(1)
    assert r.fill() == 2 and contents(r) == "bcd"
    assert r.fill() == 1 and contents(r) == "bcde"
    # Adapter end is independent of both buffered data and source EOF.
    assert r.fill() == 0 and r.buffered == 4
    var pair = default(array[2, char])
    assert r.read(pair) == 2 and pair == ['b', 'c']
    assert contents(r) == "de"
    r.consume(2)
    assert r.fill() == 0 and r.peek.len == 0
    r.setAdapter(nil)
    assert r.fill() == 3 and contents(r) == "fgh"
    assert r.readAll() == "fgh"
    echo "adapter end and handoff"

  block decoded:
    let text = "hello\nworld\n"
    var w = openWriter()
    w.setAdapter gzipAdapter(fmtGzip)
    w.write text
    let compressed = extract(w)
    var r = openReader(compressed)
    r.setAdapter gzipAdapter(fmtGzip)
    assert r.fill() == text.len
    assert contents(r) == text
    r.consume(6)
    assert r.fill() == 0 and contents(r) == "world\n"
    assert r.readLine() == "world"
    assert r.buffered == 0

    var cut = openReader(compressed.substr(0, compressed.len - 5))
    cut.setAdapter gzipAdapter(fmtGzip)
    var rejected = false
    try:
      while cut.fill() > 0: cut.consume(cut.buffered)
    except ErrorCode as e:
      assert e == SyntaxError
      rejected = true
    assert rejected
    echo "decoded buffer and errors"

  block file:
    let p = path"tstreambuffer_tmp.txt"
    var text = ""
    for i in 0..<40000: text.add char(ord('a') + i mod 26)
    writeFile($p, text)
    var r = openReader(p)
    let first = r.fill()
    assert first > 7 and first < text.len
    r.consume(7)
    let second = r.fill()
    assert second > 0
    assert r.buffered == first + second - 7
    assert contents(r) == text.substr(7, first + second - 1)
    r.consume(r.buffered)
    assert r.readAll() == text.substr(first + second)
    r.close()
    removeFile(p)
    echo "fill preserves file input"

  block closed:
    var r = default(Reader)
    assert r.buffered == 0 and r.peek.len == 0
    var rejected = false
    try:
      discard r.fill()
    except ErrorCode as e:
      assert e == BadOperation
      rejected = true
    assert rejected
    var s = openReader("closed")
    discard s.fill()
    s.close()
    rejected = false
    try:
      discard s.fill()
    except ErrorCode as e:
      assert e == BadOperation
      rejected = true
    assert rejected
    echo "invalid reader errors"

try:
  main()
except ErrorCode as e:
  echo "FAILED: ", e

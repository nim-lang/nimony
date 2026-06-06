import std/[lexbase, streams, syncio, assertions, strutils]

# Scan an entire input the way a real lexer would: advance a *local* pos char
# by char (passing `L.bufpos` directly would alias the mutable `L`), calling
# handleCR/handleLF at line endings (which is where the buffer may refill).
proc scan(s: string; bufLen: int): tuple[text: string; lines: int] {.raises.} =
  var L = default(BaseLexer)
  open(L, newStringStream(s), bufLen)
  var text = ""
  var pos = L.bufpos
  while true:
    let c = L.buf[pos]
    if c == EndOfFile: break
    case c
    of '\c':
      pos = handleCR(L, pos); text.add '\n'
    of '\L':
      pos = handleLF(L, pos); text.add '\n'
    else:
      text.add c
      inc pos
  result = (text, L.lineNumber)
  close(L)

proc main() {.raises.} =
  block: # the buffer is reassembled correctly regardless of its size
    let sample = "abc\ndef\r\nghi\njkl"
    let normalized = "abc\ndef\nghi\njkl" # CRLF collapses to one logical line end
    for bufLen in [4, 5, 7, 8, 16, 8192]: # small sizes force refills mid-input
      let (text, lines) = scan(sample, bufLen)
      assert text == normalized
      assert lines == 4

  block: # a line longer than the buffer triggers the grow-and-retry path
    let long = "x" & repeat('y', 100) & "\nz"
    let (t2, l2) = scan(long, 8)
    assert t2 == long
    assert l2 == 2

  block: # column tracking, line numbers and getCurrentLine
    var L = default(BaseLexer)
    open(L, newStringStream("hello\nworld"), 8192)
    var pos = L.bufpos
    while L.buf[pos] != '\L': inc pos     # advance to just before the newline
    assert getColNumber(L, pos) == 5      # after "hello"
    pos = handleLF(L, pos)
    L.bufpos = pos
    assert L.lineNumber == 2
    assert getColNumber(L, pos) == 0      # start of "world"
    assert getCurrentLine(L) == "world\n^\n"
    close(L)

  echo "ok"

try:
  main()
except:
  echo "unexpected exception"

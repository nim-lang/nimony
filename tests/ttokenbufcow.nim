import "../src/lib/nifcursors"

proc main() =
  # 1. Basic COW: cursor keeps old data alive after mutation
  block:
    var buf = parseFromBuffer("(call foo)", "", 8)
    block:
      let snapshot = beginRead(buf)
      buf.addDotToken()
      doAssert toString(snapshot, false) == "(call foo)"
    doAssert toString(buf, false) == "(call foo)."
    doAssert buf.len == 4

  # 2. Cursor dies before mutation — exercises rc==1 detach path
  block:
    var buf = parseFromBuffer("(call foo)", "", 8)
    block:
      let snapshot = beginRead(buf)
      doAssert toString(snapshot, false) == "(call foo)"
    buf.addDotToken()
    doAssert toString(buf, false) == "(call foo)."
    doAssert buf.len == 4

  # 3. Multiple cursors, all alive during mutation
  block:
    var buf = parseFromBuffer("(call foo)", "", 8)
    let snap1 = beginRead(buf)
    let snap2 = beginRead(buf)
    buf.addDotToken()
    doAssert toString(snap1, false) == "(call foo)"
    doAssert toString(snap2, false) == "(call foo)"
    doAssert toString(buf, false) == "(call foo)."
    doAssert buf.len == 4

  # 4. Some cursors die before mutation
  block:
    var buf = parseFromBuffer("(call bar)", "", 8)
    let snap1 = beginRead(buf)
    doAssert toString(snap1, false) == "(call bar)"
    let snap2 = beginRead(buf)
    buf.addDotToken()
    doAssert toString(snap2, false) == "(call bar)"
    doAssert toString(buf, false) == "(call bar)."
    doAssert buf.len == 4

  # 5. Repeated mutations — COW fires first time only
  block:
    var buf = parseFromBuffer("(a)", "", 8)
    let snap = beginRead(buf)
    buf.addDotToken()
    doAssert toString(snap, false) == "(a)"
    doAssert toString(buf, false) == "(a)."
    buf.addDotToken()
    doAssert toString(buf, false) == "(a).."
    doAssert toString(snap, false) == "(a)"
    doAssert buf.len == 4

  # 6. beginRead after mutation — re-attaches owner
  block:
    var buf = parseFromBuffer("(call x)", "", 8)
    buf.addDotToken()
    doAssert toString(buf, false) == "(call x)."
    let snap = beginRead(buf)
    buf.addDotToken()
    doAssert toString(snap, false) == "(call x)"
    doAssert toString(buf, false) == "(call x).."
    doAssert buf.len == 5

  # 7. cursorAt — same COW semantics
  block:
    var buf = parseFromBuffer("(call foo)", "", 8)
    let c = cursorAt(buf, 0)
    buf.addDotToken()
    doAssert toString(c, false) == "(call foo)"
    doAssert toString(buf, false) == "(call foo)."
    doAssert buf.len == 4

  # 8. readonlyCursorAt — no COW triggered
  block:
    var buf = parseFromBuffer("(call baz)", "", 8)
    let c = readonlyCursorAt(buf, 0)
    doAssert toString(c, false) == "(call baz)"

  # 9. TokenBuf destroyed while cursors alive — cursors keep data alive
  block:
    var snap: Cursor
    block:
      var buf = parseFromBuffer("(lives)", "", 8)
      snap = beginRead(buf)
    doAssert toString(snap, false) == "(lives)"

  # 10. Many beginRead calls stack up rc correctly
  block:
    var buf = parseFromBuffer("(stack)", "", 8)
    let s1 = beginRead(buf)
    let s2 = beginRead(buf)
    let s3 = beginRead(buf)
    buf.addDotToken()
    doAssert toString(s1, false) == "(stack)"
    doAssert toString(s2, false) == "(stack)"
    doAssert toString(s3, false) == "(stack)"
    doAssert toString(buf, false) == "(stack)."
    doAssert buf.len == 3

  # 11. Mutation without any beginRead — owner stays nil
  block:
    var buf = parseFromBuffer("(noop)", "", 8)
    buf.addDotToken()
    doAssert toString(buf, false) == "(noop)."
    doAssert buf.len == 3

  # 12. Shrink after COW
  block:
    var buf = parseFromBuffer("(shrink me)", "", 8)
    let snap = beginRead(buf)
    buf.shrink(2)
    doAssert buf.len == 2
    doAssert toString(snap, false) == "(shrink me)"

  # 13. Mutation triggers realloc of fresh data
  block:
    var buf = createTokenBuf(4)
    buf.addDotToken()
    let snap = beginRead(buf)
    for i in 0..<20:
      buf.addDotToken()
    doAssert buf.len == 21
    doAssert toString(snap, false) == "."

  # 14. Cursor =copy hook
  block:
    var buf = parseFromBuffer("(copy)", "", 8)
    let s1 = beginRead(buf)
    var s2 = s1
    buf.addDotToken()
    doAssert toString(s1, false) == "(copy)"
    doAssert toString(s2, false) == "(copy)"

main()
echo "All tests passed!"

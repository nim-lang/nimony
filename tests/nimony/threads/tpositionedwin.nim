# Windows positioned-file regression: submitReadAt/submitWriteAt take the
# ring's own file handle (from submitOpen) and leave its file pointer alone.
# No shutdown: other joined tests use the pool.
when defined(windows):
  import std/[ioring, assertions, syncio, commonio, widestrs]
  import std/windows/winlean

  const ErrorInvalidParameter = 87

  proc receive(id: SeqNum; expected: int) =
    var comps = default(array[8, IoCompletion])
    let stop = afterMs(10000)
    while monoNow() < stop:
      let n = pollCompletions(comps)
      for i in 0..<n:
        assert comps[i].id == id
        if comps[i].result != expected:
          echo "positioned: expected ", expected, " got ", comps[i].result
        assert comps[i].result == expected
        return
    assert false, "missing positioned completion"

  proc receiveFd(id: SeqNum): cint =
    var comps = default(array[8, IoCompletion])
    let stop = afterMs(10000)
    while monoNow() < stop:
      let n = pollCompletions(comps)
      for i in 0..<n:
        assert comps[i].id == id
        assert comps[i].result >= 0, "open failed"
        return cint(comps[i].result)
    assert false, "missing open completion"
    result = -1

  var name = "tpositionedwin.tmp"
  let fd = receiveFd(submitOpen(name.toCString, fmReadWrite,
                                {fpUserRead, fpUserWrite}, afterMs(5000)))

  var text = "positioned"
  receive(submitWriteAt(fd, text.toCString, text.len, 17, afterMs(5000)), text.len)
  var input: array[32, char]
  receive(submitReadAt(fd, addr input[0], input.len, 17, afterMs(5000)), text.len)
  for i in 0..<text.len: assert input[i] == text[i]
  receive(submitReadAt(fd, addr input[0], input.len, 4096, afterMs(5000)), 0)
  # Sparse write checks the high half of the 64-bit offset.
  let far = 4_294_967_296'i64 + 123
  receive(submitWriteAt(fd, text.toCString, text.len, far, afterMs(5000)), text.len)
  receive(submitReadAt(fd, addr input[0], input.len, far, afterMs(5000)), text.len)
  for i in 0..<text.len: assert input[i] == text[i]
  receive(submitReadAt(fd, addr input[0], input.len, -1, afterMs(5000)),
          -ErrorInvalidParameter)
  receive(submitReadAt(fd, addr input[0], input.len, 17, Deadline(0)), IoTimedOut)

  # The positioned ops must not have moved the handle's file pointer: a plain
  # read still starts at offset 0 and sees the gap before the first write.
  var head: array[27, char]
  receive(submitRead(fd, addr head[0], head.len, afterMs(5000)), head.len)
  for i in 0..<17: assert head[i] == '\0'
  for i in 0..<text.len: assert head[17 + i] == text[i]

  discard closeHandle(cast[Handle](uint(cast[uint32](fd))))
  discard deleteFileW(newWideCString(name.toCString).toWideCString)

# Portable POSIX positioned-file regression (illumos AIO, io_uring,
# epoll fallback and kqueue). No shutdown: other joined tests use the pool.
when defined(posix):
  import std/[ioring, assertions, syncio]
  from std/posix/posix import unlink, SEEK_SET, SEEK_CUR, EINVAL, EBADF, Off

  proc lseek(fd: cint; offset: Off; whence: cint): Off {.importc: "lseek".}
  proc mkstemp(path: cstring): cint {.importc: "mkstemp".}
  var name = "/tmp/nimony-positioned-XXXXXX"
  let fd = mkstemp(name.toCString)
  assert fd >= 0
  assert unlink(name.toCString) == 0

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
  receive(submitReadAt(fd, addr input[0], input.len, -1, afterMs(5000)), -int(EINVAL))
  receive(submitReadAt(fd, addr input[0], -1, 0, afterMs(5000)), -int(EINVAL))
  receive(submitReadAt(-1, addr input[0], input.len, 0, afterMs(5000)), -int(EBADF))
  receive(submitReadAt(fd, addr input[0], input.len, 17, Deadline(0)), IoTimedOut)
  # Explicit positions must not change the shared open-file cursor.
  assert lseek(fd, 3, SEEK_SET) == 3
  receive(submitReadAt(fd, addr input[0], text.len, 17, afterMs(5000)), text.len)
  assert lseek(fd, 0, SEEK_CUR) == 3
  receive(submitWriteAt(fd, text.toCString, text.len, 17, afterMs(5000)), text.len)
  assert lseek(fd, 0, SEEK_CUR) == 3
  closeFd(fd)

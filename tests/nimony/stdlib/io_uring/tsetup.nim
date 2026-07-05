import std/[syncio, assertions, posix/io_uring]

try:
  var q = newQueue(16)
  assert q.params.sqOff == SqringOffsets(
    head: 0, tail: 4, ringMask: 16, ringEntries: 24,
    flags: 36, dropped: 32, array: 576, resv1: 0, resv2: 0)
  assert q.params.cqOff == CqringOffsets(
    head: 8, tail: 12, ringMask: 20, ringEntries: 28, overflow: 44,
    cqes: 64, flags: 40, resv1: 0, resv2: 0)
  var x = q.getSqe()
  if x != nil:
    var buf = "Hello world\n"
    discard x.write(stdout.getFileHandle, buf)
    discard q.submit(1)
except:
  echo "error"

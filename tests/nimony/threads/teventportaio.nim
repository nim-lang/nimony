# Event-port integration: positional AIO, deadline/cancel ownership, slot
# reuse, socket readiness and shutdown. Runs alone because shutdown stops
# the shared worker pool permanently.
when defined(illumos):
  import std/[ioring, assertions, syncio]
  from std/posix/posix import unlink, EINVAL, EBADF

  proc mkstemp(path: cstring): cint {.importc: "mkstemp".}
  var name = "/tmp/nimony-ring-aio-XXXXXX"
  let fd = mkstemp(name.toCString)
  assert fd >= 0
  assert unlink(name.toCString) == 0
  var comps = default(array[64, IoCompletion])

  proc receive(id: SeqNum; expected: int) =
    var comps = default(array[64, IoCompletion])
    var found = false
    let stop = afterMs(10000)
    while not found and monoNow() < stop:
      let n = pollCompletions(comps)
      for i in 0..<n:
        assert comps[i].id == id
        assert comps[i].result == expected
        found = true
    assert found, "missing event-port completion"

  var data = "positioned aio"
  let wr = submitWriteAt(fd, data.toCString, data.len, 17, afterMs(5000))
  receive(wr, data.len)
  var input = default(array[32, char])
  let rd = submitReadAt(fd, addr input[0], input.len, 17, afterMs(5000))
  receive(rd, data.len)
  for i in 0..<data.len: assert input[i] == data[i]
  receive(submitReadAt(fd, addr input[0], input.len, 4096, afterMs(5000)), 0)
  receive(submitReadAt(fd, addr input[0], input.len, -1, afterMs(5000)), -int(EINVAL))
  receive(submitReadAt(-1, addr input[0], input.len, 0, afterMs(5000)), -int(EBADF))
  receive(submitPollAdd(-1, afterMs(5000)), -int(EBADF))
  # Sequential files follow the shared ring's synchronous-file path.
  receive(submitRead(fd, addr input[0], 1, afterMs(5000)), 1)
  assert input[0] == '\0' # the hole before the positioned write

  # Already-expired operations must never touch caller storage.
  input[0] = '!'
  receive(submitReadAt(fd, addr input[0], input.len, 17, Deadline(0)), IoTimedOut)
  assert input[0] == '!'

  # Race cached-file completion with public cancel-without-close and reuse.
  # Either completion wins, but a retired request must never write into the
  # caller's buffer after its one logical completion.
  for repeat in 0..<32:
    let id = submitReadAt(fd, addr input[0], input.len, 17, afterMs(5000))
    discard submitPollRemove(fd)
    var found = false
    let stop = afterMs(5000)
    while not found and monoNow() < stop:
      let n = pollCompletions(comps)
      for i in 0..<n:
        assert comps[i].id == id
        assert comps[i].result == data.len or comps[i].result == ECancelled
        found = true
    assert found
    input[0] = '!'
    receive(submitNop(afterMs(5000)), 0) # reuse the freed logical slot
    assert input[0] == '!'

  # Network operations use X/Open socket symbols and illumos option values.
  let listener = listenTcp(0)
  let client = socketNonBlocking()
  var address = default(Sockaddr_storage)
  var length = SockLen(0)
  loopbackAddr(address, length, boundPort(listener))
  let accepting = submitAccept(listener, afterMs(5000))
  let connecting = submitConnect(client, address, length, afterMs(5000))
  var accepted = -1
  var connected = false
  let stop = afterMs(10000)
  while (accepted < 0 or not connected) and monoNow() < stop:
    let n = pollCompletions(comps)
    for i in 0..<n:
      if comps[i].id == accepting:
        accepted = comps[i].result
        assert accepted >= 0
      elif comps[i].id == connecting:
        assert comps[i].result == 0
        connected = true
      else: assert false
  assert accepted >= 0 and connected
  # A newly accepted socket must already be nonblocking. Mix readiness and
  # AIO notifications on the same port without dispatching a file read twice.
  var network = "net"
  var netInput = default(array[8, char])
  let netWrite = submitWrite(client, network.toCString, network.len, afterMs(5000))
  let netRead = submitRead(cint(accepted), addr netInput[0], netInput.len, afterMs(5000))
  let fileRead = submitReadAt(fd, addr input[0], input.len, 17, afterMs(5000))
  var seen = default(array[3, bool])
  var got = 0
  let mixedStop = afterMs(10000)
  while got < 3 and monoNow() < mixedStop:
    let n = pollCompletions(comps)
    for i in 0..<n:
      let k = if comps[i].id == netWrite: 0 elif comps[i].id == netRead: 1 else: 2
      assert k != 2 or comps[i].id == fileRead
      assert not seen[k], "duplicate completion"
      seen[k] = true
      assert comps[i].result == (if k == 2: data.len else: network.len)
      inc got
  assert got == 3
  for i in 0..<network.len: assert netInput[i] == network[i]
  closeFd(cint(accepted)); closeFd(client); closeFd(listener)

  # Shutdown must drain physical requests even if logical cancellation freed
  # their slots. The duplicate descriptor keeps the original file alive.
  var closingIds = default(array[64, SeqNum])
  for i in 0..<closingIds.len:
    closingIds[i] = submitReadAt(fd, addr input[0], input.len, 17, never)
  closeFd(fd)
  input[0] = '!'
  var closingSeen = default(array[64, bool])
  var closedCount = 0
  let closeStop = afterMs(10000)
  while closedCount < closingIds.len and monoNow() < closeStop:
    let n = pollCompletions(comps)
    for i in 0..<n:
      var k = 0
      while k < closingIds.len and closingIds[k] != comps[i].id: inc k
      assert k < closingIds.len
      assert not closingSeen[k]
      closingSeen[k] = true
      assert comps[i].result == data.len or comps[i].result == ECancelled
      inc closedCount
  assert closedCount == closingIds.len
  shutdown()
  assert input[0] == '!', "late AIO touched caller storage after close"

import std/syncio
echo "event-port ring ok"

## Connection setup has one deadline, while later I/O uses a renewable deadline
## shared by the pair. All peers are local; the TLS doubles exercise the stream
## handshake/record pump without certificates or an external server.
import std/[streamio, socket, ioring, threadpool, atomics, assertions, syncio]

type
  SilentTls = ref object of TlsEngine
  ExpandingTls = ref object of TlsEngine
    pending: string
    cleanFinish: bool
    failWrite: bool

method start*(e: SilentTls; host: string) {.raises.} = discard
method recordsOut*(e: SilentTls; dest: var string) {.raises.} = discard

method start*(e: ExpandingTls; host: string) {.raises.} = discard
method isConnected*(e: ExpandingTls): bool = true
method appIn*(e: ExpandingTls; src: openArray[char]; used: var int) {.raises.} =
  if e.failWrite: raise IOError
  used = src.len
  e.pending = newString(16 * 1024 * 1024)
method recordsOut*(e: ExpandingTls; dest: var string) {.raises.} =
  dest.add e.pending
  e.pending = ""
method close*(e: ExpandingTls) {.raises.} = e.cleanFinish = true

var listener: Listener
var thePort: uint16
var serverDone: int
var clientDone: int
var requestDone: array[2, int]
var failures: int

# Catch selectors are types, so bind ErrorCode when checking an exact enum value.
proc closedAccessors(r: var Reader; w: var Writer) {.raises.} =
  var caught = 0
  try: discard r.deadline
  except ErrorCode as e:
    assert e == BadOperation
    inc caught
  try: r.renew(never)
  except ErrorCode as e:
    assert e == BadOperation
    inc caught
  try: discard w.deadline
  except ErrorCode as e:
    assert e == BadOperation
    inc caught
  try: w.renew(never)
  except ErrorCode as e:
    assert e == BadOperation
    inc caught
  assert caught == 4

proc memoryAccessors() {.raises.} =
  var r = openReader("buffered")
  var w = openWriter()
  assert r.deadline == never and w.deadline == never
  let dl = afterMs(0)
  r.renew(dl)
  w.renew(dl)
  assert r.deadline == dl and w.deadline == dl
  # A deadline bounds descriptor I/O, not data already available in memory.
  assert r.readAll() == "buffered"
  w.write "memory"
  w.flush()
  r.renew(never)
  w.renew(never)
  assert r.deadline == never and w.deadline == never
  r.close()
  w.abort()
  closedAccessors(r, w)
  var emptyReader = default(Reader)
  var emptyWriter = default(Writer)
  closedAccessors(emptyReader, emptyWriter)

proc waitForRequest(index: int) {.passive.} =
  let limit = afterMs(10_000)
  while atomicLoad(requestDone[index], moAcquire) == 0:
    assert monoNow() < limit, "request constructor did not finish"
    var res = 0
    let c = delay()
    discard submitTimeout(afterMs(1), c, addr res)
    suspend()

proc serverChecks() {.passive, raises.} =
  for index in 0 ..< 11:
    var peer = listener.accept(afterMs(10_000))
    case index
    of 0:
      # Withhold a reply until the client's first read expires and it renews.
      assert peer.readLine(afterMs(5000)) == "renewed"
      peer.writeLine("reply", afterMs(5000))
    of 1:
      assert peer.readLine(afterMs(5000)) == "request"
      peer.writeLine("response", afterMs(5000))
    of 2:
      peer.writeLine("reader", afterMs(5000))
    of 3:
      assert peer.readLine(afterMs(5000)) == "writer"
    of 4 .. 7, 10:
      # A silent handshake or a failed TLS request must release the connection.
      var one = default(array[1, char])
      assert peer.read(one, afterMs(5000)) == 0, "TLS construction did not release its peer"
    else:
      # Do not drain requests until their constructors have finished.
      waitForRequest(index - 8)
    peer.close()

proc expiredConstruction(address: string) {.passive, raises.} =
  var caught = 0
  let dl = afterMs(0)
  try:
    var (r, w) = streamio.connect(address, dl = dl)
    w.abort()
    r.close()
  except ErrorCode as e:
    assert e == TimeoutError, "expired connect: " & $e
    inc caught
  try:
    var (r, w) = streamio.connect(address, "request", dl = dl)
    w.abort()
    r.close()
  except ErrorCode as e:
    assert e == TimeoutError, "expired request connect: " & $e
    inc caught
  try:
    var r = connectReader(address, dl = dl)
    r.close()
  except ErrorCode as e:
    assert e == TimeoutError, "expired reader connect: " & $e
    inc caught
  try:
    var w = connectWriter(address, dl = dl)
    w.abort()
  except ErrorCode as e:
    assert e == TimeoutError, "expired writer connect: " & $e
    inc caught
  assert caught == 4

proc clientChecks() {.passive, raises.} =
  memoryAccessors()
  # Expiry precedes DNS: do not depend on an external lookup or resolver config.
  expiredConstruction("not-resolved.invalid:1")
  let address = "127.0.0.1:" & $thePort
  block:
    var (r, w) = streamio.connect(address, dl = afterMs(5000))
    assert r.deadline == never and w.deadline == never
    let dl = afterMs(20)
    r.renew(dl)
    assert r.deadline == dl and w.deadline == dl
    var expired = false
    var one = default(array[1, char])
    try: discard r.read(one)
    except ErrorCode as e:
      assert e == TimeoutError, "shared read deadline: " & $e
      expired = true
    assert expired, "shared read deadline did not expire"
    let next = afterMs(5000)
    w.renew(next)
    assert r.deadline == next and w.deadline == next
    w.write "renewed\n"
    w.flush()
    assert r.readLine() == "reply"
    r.renew(never)
    assert w.deadline == never
    w.abort()
    r.close()
  block:
    var (r, w) = streamio.connect(address, "request\n", dl = afterMs(5000))
    assert r.deadline == never and w.deadline == never
    assert r.readLine() == "response"
    w.abort()
    r.close()
  block:
    var r = connectReader("localhost:" & $thePort, dl = afterMs(5000))
    assert r.deadline == never
    r.renew(afterMs(5000))
    assert r.readLine() == "reader"
    r.close()
  block:
    var w = connectWriter(address, dl = afterMs(5000))
    assert w.deadline == never
    w.renew(afterMs(5000))
    w.write "writer\n"
    w.close()
  var caught = 0
  for index in 0 ..< 4:
    let engine = SilentTls()
    let dl = afterMs(50)
    try:
      case index
      of 0:
        var (r, w) = streamio.connect(address, engine, dl)
        w.abort()
        r.close()
      of 1:
        var (r, w) = streamio.connect(address, "request", engine, dl)
        w.abort()
        r.close()
      of 2:
        var r = connectReader(address, engine, dl)
        r.close()
      else:
        var w = connectWriter(address, engine, dl)
        w.abort()
    except ErrorCode as e:
      assert e == TimeoutError, "TLS constructor " & $index & ": " & $e
      inc caught
  assert caught == 4
  # Large plain requests flush inside write; small TLS requests flush explicitly.
  # Windows may buffer the entire payload. Both timely success and timeout are
  # valid; a blocked constructor must still finish under its setup deadline.
  let payload = newString(16 * 1024 * 1024)
  let engine = ExpandingTls(pending: "", cleanFinish: false)
  for index in 0 ..< 2:
    try:
      if index == 0:
        var (r, w) = streamio.connect(address, payload, dl = afterMs(100))
        assert r.deadline == never and w.deadline == never
        w.abort()
        r.close()
      else:
        var (r, w) = streamio.connect(address, "small request", engine, afterMs(100))
        assert r.deadline == never and w.deadline == never
        w.abort()
        r.close()
    except ErrorCode as e:
      assert e == TimeoutError, "request constructor " & $index & ": " & $e
    atomicStore(requestDone[index], 1, moRelease)
  assert not engine.cleanFinish, "request teardown must not cleanly finish TLS"
  # Exercise failed-request cleanup independently of platform socket buffers.
  let failing = ExpandingTls(pending: "", cleanFinish: false, failWrite: true)
  var rejected = false
  try:
    var (r, w) = streamio.connect(address, "failed request", failing, afterMs(5000))
    w.abort()
    r.close()
  except ErrorCode as e:
    assert e == IOError, "injected request failure: " & $e
    rejected = true
  assert rejected, "request constructor did not propagate the TLS write failure"
  assert not failing.cleanFinish, "failed request must not cleanly finish TLS"

proc server() {.passive.} =
  try: serverChecks()
  except ErrorCode as e:
    echo "server deadline checks failed: ", e
    atomicStore(failures, 1)
  atomicStore(serverDone, 1, moRelease)

proc client() {.passive.} =
  try: clientChecks()
  except ErrorCode as e:
    echo "client deadline checks failed: ", e
    atomicStore(failures, 1)
  atomicStore(clientDone, 1, moRelease)

initIoRing()
listener = listen(0)
thePort = listener.boundPort
submit(delay(server()), 0)
submit(delay(client()), 1)
let limit = afterMs(30_000)
while atomicLoad(serverDone, moAcquire) == 0 or atomicLoad(clientDone, moAcquire) == 0:
  assert monoNow() < limit, "stream deadline tests did not finish"
  discard poolHelp()
  discard gReactor(1)
listener.close()
shutdown()
assert atomicLoad(failures) == 0
echo "stream deadlines passed"

{.feature: "assumeSync".}  # test program: globals published by atomic flags

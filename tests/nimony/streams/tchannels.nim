import std/[syncio, streamio, channels, threadpool, atomics, typetraits]

# --- a byte pipe: another task produces the body --------------------------

proc produceLines(dest: sink Writer; n: int) {.passive.} =
  var w = dest
  try:
    for i in 0 ..< n:
      w.write "row " & $i & "\n"
  except ErrorCode:
    echo "producer failed"
  # the destructor ends the stream

proc pipeDemo() {.raises.} =
  var (r, w) = pipe(64)           # small: the producer has to wait for us
  submit(delay(produceLines(w, 100)))
  var lines = 0
  var last = ""
  while true:
    let line = r.readLine()
    if line.len == 0: break
    last = line
    inc lines
  echo "pipe: ", lines, " lines, last: ", last

# --- a channel: several senders, values with heap parts --------------------

var sendersDone: int

proc produce(s: Sender[string]; base, n: int) {.passive.} =
  try:
    for i in 0 ..< n:
      discard s.send($(base + i))
  except ErrorCode:
    echo "send failed"
  atomicInc sendersDone

proc startSenders(rx: var Receiver[string]) =
  let (r, tx) = channel[string](4)
  rx = r
  for k in 0 ..< 3:
    submit(delay(produce(tx, k * 1000, 50)))
  # `tx` dies here: only the copies in the tasks keep the channel open

proc channelDemo() {.raises.} =
  var rx = Receiver[string]()
  startSenders(rx)
  var count = 0
  var sum = 0
  var msg = ""
  while rx.recv(msg):
    inc count
    for ch in msg: sum = sum + ord(ch)
  echo "channel: ", count, " messages, checksum ", sum

# --- many senders, many receivers, on the pool's threads -------------------

const
  Producers = 4
  Consumers = 4
  PerProducer = 2000

var received: int
var receivedSum: int
var consumersDone: int

proc produceInts(s: Sender[int]; base: int) {.passive.} =
  try:
    for i in 1 .. PerProducer:
      discard s.send(base + i)
  except ErrorCode:
    echo "send failed"

proc consumeInts(r: Receiver[int]) {.passive.} =
  var n = 0
  var sum = 0
  var x = 0
  try:
    while r.recv(x):
      inc n
      sum = sum + x
  except ErrorCode:
    echo "recv failed"
  discard atomicFetchAdd(received, n, moRelaxed)
  discard atomicFetchAdd(receivedSum, sum, moRelaxed)
  atomicInc consumersDone

proc startStress() =
  let (rx, tx) = channel[int](16)
  for k in 0 ..< Consumers:
    submit(delay(consumeInts(rx)))
  for k in 0 ..< Producers:
    submit(delay(produceInts(tx, k * PerProducer)))

proc stressDemo() =
  startStress()
  while atomicLoad(consumersDone, moAcquire) < Consumers: discard
  let total = Producers * PerProducer
  echo "stress: ", atomicLoad(received, moAcquire) == total, " ",
       atomicLoad(receivedSum, moAcquire) == total * (total + 1) div 2

# --- leftovers are destroyed, sends fail once no receiver is left ----------

var alive: int

type
  Tracked = object
    id: int

proc `=destroy`(t: Tracked) =
  if t.id != 0: atomicDec alive

proc `=wasMoved`(t: var Tracked) = t.id = 0

proc `=dup`(t: Tracked): Tracked {.error.}
proc `=copy`(dest: var Tracked; src: Tracked) {.error.}

proc track(id: int): Tracked =
  atomicInc alive
  Tracked(id: id)

proc fill(tx: var Sender[Tracked]) {.raises.} =
  let (rx, s) = channel[Tracked](8)
  tx = s
  for i in 1 .. 5: discard tx.send(track(i))
  var t = Tracked(id: 0)
  discard rx.recv(t)
  echo "received ", t.id, ", alive ", atomicLoad(alive, moAcquire)
  # `rx` and `t` die here; the four left in the ring go too

proc leftoverDemo() {.raises.} =
  var tx = Sender[Tracked]()
  fill(tx)
  echo "after the receiver: alive ", atomicLoad(alive, moAcquire)
  echo "send without a receiver: ", tx.send(track(6)),
       ", alive ", atomicLoad(alive, moAcquire)

# --- values without pointers over any stream -------------------------------

type
  Point = object
    x, y: int32

proc wireDemo() {.raises.} =
  echo "supportsCopyMem: ", supportsCopyMem(Point), " ", supportsCopyMem(string),
       " ", supportsCopyMem(Tracked)
  # over a byte pipe: the ends take turns on the shared Reader/Writer
  let (r, w) = pipe()
  let rx = receiver[Point](r)
  block:
    let tx = sender[Point](w)
    for i in 0'i32 ..< 3'i32: discard tx.send(Point(x: i, y: i * 10))
  var p = Point(x: 0, y: 0)
  while rx.recv(p): echo "point ", p.x, " ", p.y
  let cut = receiver[Point](openReader("12345"))
  try:
    discard cut.recv(p)
    echo "no error?"
  except ErrorCode as e:
    echo "cut short: ", e

try:
  pipeDemo()
  channelDemo()
  stressDemo()
  leftoverDemo()
  wireDemo()
except ErrorCode as e:
  echo "FAILED: ", e

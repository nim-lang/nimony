#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Typed channels (nim-lang/nimony#2635): copyable `Sender[T]`/`Receiver[T]`
## ends, over an in-process ring (`channel[T]`) or over any `std/streamio`
## stream (`sender[T]`, `receiver[T]`). Waiting parks the task, it does not
## block a thread.
##
## .. code-block:: nim
##   let (r, s) = channel[Job](16)
##   submit(delay(worker(r)))
##   discard s.send Job(id: 1)
##
## A `Sender[T]` and a `Receiver[T]` are copyable; all copies share ONE end, a
## `Writer` or `Reader`, so a stream never has more than one of each and its
## lifecycle stays that of a connection: the last copy gone is the end gone.

import streamio {.cyclic.}
import std/[threadpool, ticketlocks, typetraits]

export streamio.Reader, streamio.Writer

# On a channel's own ring, a send or a receive is one whole record under the
# ring's lock, so copies on different threads need nothing more.
# Over any other stream, the end's buffer and a partial write would mix up
# messages, so the copies take turns: `EndLock` parks the ones waiting.

type
  EndLock = object
    spin: TicketLock
    held: bool
    waiters: seq[Continuation]

proc lockWait(L: ptr EndLock) {.passive.} =
  let k = delay()
  var ready = false
  acquire L.spin
  if L.held:
    L.waiters.add k
  else:
    L.held = true
    ready = true
  release L.spin
  if ready: submit(k)
  suspend()

proc lockEnd(L: ptr EndLock) {.passive.} =
  var got = false
  acquire L.spin
  if not L.held:
    L.held = true
    got = true
  release L.spin
  if not got: lockWait(L)          # `unlockEnd` hands the lock over

proc unlockEnd(L: ptr EndLock) =
  var wake: seq[Continuation] = @[]
  acquire L.spin
  if L.waiters.len > 0:            # stays held: the oldest waiter has it now
    wake.add L.waiters[0]
    var rest: seq[Continuation] = @[]
    for i in 1 ..< L.waiters.len: rest.add L.waiters[i]
    L.waiters = rest
  else: L.held = false
  release L.spin
  for k in wake: submit(k)

type
  SendEndObj = object
    lock: EndLock
    w: Writer
    direct: bool          ## sends go straight into the writer's ring

  SendEnd = ref SendEndObj

  RecvEndObj = object
    lock: EndLock
    r: Reader
    direct: bool

  RecvEnd = ref RecvEndObj

  Sender*[T] = object
    ## The sending end of a channel. Copies share the end.
    e: nil SendEnd

  Receiver*[T] = object
    ## The receiving end of a channel. Copies share the end.
    e: nil RecvEnd

proc newEndLock(): EndLock =
  EndLock(spin: TicketLock(), held: false, waiters: @[])

proc sendEnd(w: sink Writer; recSize: int): SendEnd {.raises.} =
  if not isOpen(w): raise BadOperation
  let direct = hasRecordRing(w, recSize)
  result = SendEnd(lock: newEndLock(), w: w, direct: direct)

proc recvEnd(r: sink Reader; recSize: int): RecvEnd {.raises.} =
  if not isOpen(r): raise BadOperation
  let direct = hasRecordRing(r, recSize)
  result = RecvEnd(lock: newEndLock(), r: r, direct: direct)

proc sendBytes(e: SendEnd; p: pointer; n: int): bool {.passive, raises.} =
  if e.direct:
    result = writeRecord(e.w, p)
  else:
    lockEnd(addr e.lock)
    try:
      write(e.w, toOpenArray(cast[ptr UncheckedArray[char]](p), 0, n - 1))
      result = true
    except ErrorCode as err:
      if err == BadDescriptor and isPipe(e.w):
        result = false           # a byte pipe whose reader is gone
      else:
        unlockEnd(addr e.lock)
        raise err
    unlockEnd(addr e.lock)

proc recvBytes(e: RecvEnd; p: pointer; n: int): bool {.passive, raises.} =
  if e.direct:
    result = readRecord(e.r, p)
  else:
    let dest = cast[ptr UncheckedArray[char]](p)
    lockEnd(addr e.lock)
    var got = 0
    try:
      while got < n:
        var rest = toOpenArray(dest, got, n - 1)
        let k = read(e.r, rest)
        if k == 0:
          if got > 0: raise ProtocolError     # a message cut short
          break
        got += k
    except ErrorCode as err:
      unlockEnd(addr e.lock)
      raise err
    unlockEnd(addr e.lock)
    result = got == n

proc tryRecvBytes(e: RecvEnd; p: pointer; n: int): bool {.passive, raises.} =
  ## The same, when one value is already there. Over a ring that is the ring's
  ## own answer; over a stream it is "the reader has this much buffered", which
  ## is the only case in which the read below cannot park under the lock.
  if e.direct:
    result = tryReadRecord(e.r, p)
  else:
    result = false
    lockEnd(addr e.lock)
    if e.r.buffered >= n:
      let dest = cast[ptr UncheckedArray[char]](p)
      var got = 0
      try:
        while got < n:
          var rest = toOpenArray(dest, got, n - 1)
          let k = read(e.r, rest)
          if k == 0: break
          got += k
      except ErrorCode as err:
        unlockEnd(addr e.lock)
        raise err
      result = got == n
    unlockEnd(addr e.lock)

proc dropRecord[T](p: pointer) {.nimcall.} =
  var x {.noinit.}: T
  copyMem(addr x, p, sizeof(T))       # `x` owns it now and destroys it

proc channel*[T](capacity: int): (Receiver[T], Sender[T]) =
  ## A channel for up to `capacity` values. Any `T` works: a value moves as
  ## its bytes, and what it points to stays where it is. The ring underneath
  ## is never handed out, so nothing can write those pointers to a file.
  var drop: nil proc (p: pointer) {.nimcall.} = nil
  when not supportsCopyMem(T):
    drop = dropRecord[T]
  var (r, w) = recordPipe(capacity, sizeof(T), drop)
  result = (Receiver[T](e: RecvEnd(lock: newEndLock(), r: r, direct: true)),
            Sender[T](e: SendEnd(lock: newEndLock(), w: w, direct: true)))

proc sender*[T](w: sink Writer): Sender[T] {.raises.} =
  ## Values of `T` over any `Writer`, as their bytes. Between machines the
  ## layout and the byte order must match. `send` does not flush; see `flush`.
  when not supportsCopyMem(T):
    {.error: "T contains pointers; use channel[T]()".}
  result = Sender[T](e: sendEnd(w, sizeof(T)))

proc receiver*[T](r: sink Reader): Receiver[T] {.raises.} =
  ## Values of `T` from any `Reader`. A message cut short by the end of the
  ## data raises `ProtocolError`.
  when not supportsCopyMem(T):
    {.error: "T contains pointers; use channel[T]()".}
  result = Receiver[T](e: recvEnd(r, sizeof(T)))

proc send*[T](s: Sender[T]; x: sink T): bool {.passive, raises.} =
  ## Parks while the channel is full. False when no receiver is left; `x` is
  ## consumed either way. Raises what the `Writer` underneath raises.
  let e = s.e
  if e == nil: raise BadOperation
  var v = x
  result = sendBytes(e, addr v, sizeof(T))
  if result: `=wasMoved`(v)           # its bytes are the receiver's now

proc recv*[T](r: Receiver[T]; dest: var T): bool {.passive, raises.} =
  ## Parks while the channel is empty. False when no sender is left and
  ## nothing is buffered; `dest` is then untouched.
  let e = r.e
  if e == nil: raise BadOperation
  var v {.noinit.}: T
  zeroMem(addr v, sizeof(T))          # a moved-from value until filled
  result = recvBytes(e, addr v, sizeof(T))
  if result: dest = move v

proc tryRecv*[T](r: Receiver[T]; dest: var T): bool {.passive, raises.} =
  ## One value if one is already waiting; false when the channel is empty, or
  ## every sender is gone. Never parks, so a task can drain what is there, do
  ## something with the batch — put it on a socket with one flush, say — and
  ## only then `recv`.
  let e = r.e
  if e == nil: raise BadOperation
  var v {.noinit.}: T
  zeroMem(addr v, sizeof(T))          # a moved-from value until filled
  result = tryRecvBytes(e, addr v, sizeof(T))
  if result: dest = move v

proc flush*[T](s: Sender[T]) {.passive, raises.} =
  ## For a sender over a buffered `Writer`: everything sent goes out. A
  ## channel's own sends need no flush.
  let e = s.e
  if e == nil: raise BadOperation
  if not e.direct:
    lockEnd(addr e.lock)
    try:
      flush(e.w)
    except ErrorCode as err:
      unlockEnd(addr e.lock)
      raise err
    unlockEnd(addr e.lock)

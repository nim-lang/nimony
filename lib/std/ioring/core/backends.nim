# Shared poll-based backend (epoll / kqueue).
# PollBackend holds the common poll fd + arena and provides
# processFd() to dispatch I/O for all slots on a given fd.
# Subclasses override registerEvent/reArmEvent/fetchEvents.

import ../types
import ../slots

type
  PollBackend* = ref object of Backend
    pollFd*: cint
    arena*: int

const
  EvRead* = 1
  EvWrite* = 2

method registerEvent*(b: PollBackend; fd: cint; slotIdx: int; mask: int) {.base.} = discard
method reArmEvent*(b: PollBackend; fd: cint; slotIdx: int; mask: int) {.base.} = discard

method submit*(b: PollBackend; slotIdx: int; op: ptr OpContext) =
  var mask = 0
  if op.kind == opRead or op.kind == opAccept:
    mask = mask or EvRead
  if op.kind == opWrite:
    mask = mask or EvWrite
  b.registerEvent(op.fd, slotIdx, mask)

when defined(posix):
  import std / assertions
  from std/posix/posix import SockAddr, SockLen

  proc posixRead(fd: cint; buf: pointer; count: int): int {.importc: "read", header: "<unistd.h>".}
  proc posixWrite(fd: cint; buf: pointer; count: int): int {.importc: "write", header: "<unistd.h>".}
  proc posixAccept(s: cint; addr: ptr SockAddr; addrlen: ptr SockLen): cint {.
    importc: "accept", header: "<sys/socket.h>".}

  proc processFd*(b: PollBackend; fd: cint; firedEvents: int) =
    let a = cast[ptr SlotArena](b.arena)
    # collect all pending slot indices for this fd
    var pending = newSeq[int]()
    for j in 0..<MaxOps:
      if a.slots[j].inUse and a.slots[j].fd == fd:
        pending.add(j)
        if pending.len >= MaxOps:
          break
    # process each pending slot
    for j in pending:
      let s = addr a.slots[j]
      case s.kind
      of opRead:
        let r = posixRead(fd, s.buf, s.len)
        if b.completeFn != nil:
          b.completeFn(j, if r >= 0: r else: -1, b.completeEnv)
      of opWrite:
        let r = posixWrite(fd, s.buf, s.len)
        if b.completeFn != nil:
          b.completeFn(j, if r >= 0: r else: -1, b.completeEnv)
      of opAccept:
        var addrLen = s.acceptLen
        let clientFd = posixAccept(fd, cast[ptr SockAddr](addr s.acceptAddr), addr addrLen)
        if b.completeFn != nil:
          b.completeFn(j, if clientFd >= 0: clientFd else: -1, b.completeEnv)
    # re-arm if any slots remain for this fd
    var needsArm = false
    var armMask = 0
    var armSlotIdx = -1
    for j in 0..<MaxOps:
      if a.slots[j].inUse and a.slots[j].fd == fd:
        if not needsArm:
          needsArm = true
          armSlotIdx = j
          let sk = a.slots[j].kind
          if sk == opRead or sk == opAccept:
            armMask = armMask or EvRead
          if sk == opWrite:
            armMask = armMask or EvWrite
        else:
          break
    if needsArm:
      b.reArmEvent(fd, armSlotIdx, armMask)
else:
  proc processFd*(b: PollBackend; fd: cint; firedEvents: int) = discard

# Shared poll-based backend (epoll / kqueue).
# PollBackend holds the common poll fd + arena and provides
# processFd() to dispatch I/O for all slots on a given fd.
# Subclasses override registerEvent/reArmEvent/fetchEvents.

import ./types
import ./slots
import ./backend

type
  PollBackend* = ref object of Backend
    pollFd*: cint
    reArmEventFn*: proc(b: Backend; fd: cint; slotIdx: int; mask: int) {.nimcall.}

const
  EvRead* = 1
  EvWrite* = 2

when defined(posix):
  import std / assertions
  from std/posix/posix import SockLen

  proc posixRead(fd: cint; buf: nil pointer; count: int): int {.importc: "read", header: "<unistd.h>".}
  proc posixWrite(fd: cint; buf: nil pointer; count: int): int {.importc: "write", header: "<unistd.h>".}
  proc posixAccept(s: cint; `addr`: pointer; addrlen: ptr SockLen): cint {.
    importc: "accept", header: "<sys/socket.h>".}

  proc processFd*(b: Backend; fd: cint; firedEvents: int) =
    let self = PollBackend(b)
    let a = b.arena
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
          b.completeFn(b.ring, j, if r >= 0: r else: -1)
      of opWrite:
        let r = posixWrite(fd, s.buf, s.len)
        if b.completeFn != nil:
          b.completeFn(b.ring, j, if r >= 0: r else: -1)
      of opAccept:
        var addrLen = s.acceptLen
        let clientFd = posixAccept(fd, addr s.acceptAddr, addr addrLen)
        if b.completeFn != nil:
          b.completeFn(b.ring, j, if clientFd >= 0: clientFd else: -1)
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
      self.reArmEventFn(b, fd, armSlotIdx, armMask)
else:
  proc processFd*(b: Backend; fd: cint; firedEvents: int) = discard

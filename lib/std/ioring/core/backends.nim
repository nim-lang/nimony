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
    reArmEventFn*: proc(b: Backend; fd: cint; mask: int) {.nimcall.}
    ## Registers (or re-arms, for EPOLLONESHOT-style backends) readiness
    ## interest for `fd`. Takes only `fd` and the direction mask — never a
    ## specific slot index: a fd can have several ops in flight (e.g. a
    ## pending read *and* a pending write) sharing one epoll/kqueue
    ## registration, so the registration is keyed by fd, not by any one op.

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
    ## Dispatch every pending op on `fd` whose direction actually matches the
    ## readiness that just fired. `firedEvents` (EvRead/EvWrite, as delivered
    ## by the poller) is authoritative: a write-readiness wakeup must not
    ## drive a still-pending *read* op (and vice versa) — the fd may be
    ## registered for both directions at once (e.g. a socket with an
    ## in-flight read and an in-flight write), and only the direction that
    ## actually fired has data ready / a free send buffer.
    let self = PollBackend(b)
    let a = b.arena
    # O(k) in the number of ops on this fd, via the intrusive per-fd list,
    # instead of an O(MaxOps) scan of the whole arena.
    for j in a.slotsForFd(fd):
      if not a.slots[j].inUse: continue # can be freed by a racing cancel
      let s = addr a.slots[j]
      case s.kind
      of opRead:
        if (firedEvents and EvRead) == 0: continue
        let r = posixRead(fd, s.buf, s.len)
        if b.completeFn != nil:
          b.completeFn(b.ring, j, if r >= 0: r else: -1)
      of opWrite:
        if (firedEvents and EvWrite) == 0: continue
        let r = posixWrite(fd, s.buf, s.len)
        if b.completeFn != nil:
          b.completeFn(b.ring, j, if r >= 0: r else: -1)
      of opAccept:
        if (firedEvents and EvRead) == 0: continue
        var addrLen = s.acceptLen
        let clientFd = posixAccept(fd, addr s.acceptAddr, addr addrLen)
        if b.completeFn != nil:
          b.completeFn(b.ring, j, if clientFd >= 0: clientFd else: -1)
    # Re-arm for whatever directions still have an op pending on this fd
    # (completions above may have freed some slots already).
    var armMask = 0
    var stillPending = false
    for j in a.slotsForFd(fd):
      if not a.slots[j].inUse: continue
      stillPending = true
      let sk = a.slots[j].kind
      if sk == opRead or sk == opAccept:
        armMask = armMask or EvRead
      if sk == opWrite:
        armMask = armMask or EvWrite
    if stillPending:
      self.reArmEventFn(b, fd, armMask)
    # else: nothing left for this fd; the backend already consumed the
    # one-shot registration, and submitFn/registerEvent will re-add it the
    # next time an op targets this fd.
else:
  proc processFd*(b: Backend; fd: cint; firedEvents: int) = discard

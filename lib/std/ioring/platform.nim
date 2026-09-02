# Platform detection — which I/O backends are available at compile time.

import ./core/backend

when defined(linux):
  const hasEpoll* = true
  const hasKqueue* = false
  const hasIouring* = not defined(nimIoringNoUring)
    ## `-d:nimIoringNoUring` picks the epoll backend on Linux. io_uring is the
    ## better default, but it also *hides* the epoll path: many container
    ## sandboxes forbid `io_uring_setup`, so the fallback is what actually runs
    ## there and it needs to be reachable deliberately — for testing, and for
    ## anyone who has to disable io_uring for policy reasons.
elif defined(macosx) or defined(freebsd) or defined(netbsd) or
     defined(openbsd) or defined(dragonfly):
  const hasEpoll* = false
  const hasKqueue* = true
  const hasIouring* = false
else:
  const hasEpoll* = false
  const hasKqueue* = false
  const hasIouring* = false

const hasWsaPoll* = defined(windows) and not defined(nimIoringIocp)
  ## Windows default: the WSAPoll readiness backend (backends/wsapoll.nim).
const hasIocp* = defined(windows) and defined(nimIoringIocp)
  ## `-d:nimIoringIocp` selects the IOCP proactor — a stub until it lands
  ## (design of record: hashi/doc/iocp-ioring-briefing.md).
const hasIoPoll* = hasEpoll or hasKqueue or hasWsaPoll

# Imports are guarded by syntactic `defined()` tests, not by the constants
# above: nimony's module scanner prunes an import only when its `when` is a
# literal `defined(...)` expression — a branch guarded by a `const` is always
# built, so a platform backend would be compiled (and fail) on every other OS.
when defined(linux):
  when not defined(nimIoringNoUring):
    import ./backends/iouring
  else:
    import ./backends/epoll
elif defined(macosx) or defined(freebsd) or defined(netbsd) or
     defined(openbsd) or defined(dragonfly):
  import ./backends/kqueue
elif defined(windows):
  when defined(nimIoringIocp):
    import ./backends/iocp
  else:
    import ./backends/wsapoll

var backendRelays*: BackendRelays

proc initPlatformBackend*() =
  when defined(linux):
    when not defined(nimIoringNoUring):
      backendRelays = initIoUringBackendRelays()
    else:
      backendRelays = initEpollBackendRelays()
  elif defined(macosx) or defined(freebsd) or defined(netbsd) or
       defined(openbsd) or defined(dragonfly):
    backendRelays = initKqueueBackendRelays()
  elif defined(windows):
    when defined(nimIoringIocp):
      backendRelays = initIocpBackendRelays()
    else:
      backendRelays = initWsaPollBackendRelays()
  else:
    {.error: "No I/O backend available for this platform".}

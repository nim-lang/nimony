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

const hasIocp* = defined(windows) and not defined(nimIoringWsaPoll)
  ## Windows default: the IOCP proactor (backends/iocp.nim) — the ring's
  ## contract is completion-shaped and IOCP serves it directly, with no
  ## readiness emulation and no per-connection scheduler-tick stall (see the
  ## backend header for the measurements). `-d:nimIoringIocp`, the opt-in
  ## while it was new, is accepted and means nothing now.
const hasWsaPoll* = defined(windows) and defined(nimIoringWsaPoll)
  ## `-d:nimIoringWsaPoll` picks the WSAPoll readiness backend
  ## (backends/wsapoll.nim) instead — the same shape as `nimIoringNoUring`
  ## on Linux: the fallback stays reachable deliberately, for testing and for
  ## a host where completion ports misbehave (layered service providers are
  ## the usual culprit).
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
  when defined(nimIoringWsaPoll):
    import ./backends/wsapoll
  else:
    import ./backends/iocp

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
    when defined(nimIoringWsaPoll):
      backendRelays = initWsaPollBackendRelays()
    else:
      backendRelays = initIocpBackendRelays()
  else:
    {.error: "No I/O backend available for this platform".}

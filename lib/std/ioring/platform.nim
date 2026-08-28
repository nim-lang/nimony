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

const hasIocp* = defined(windows)
const hasIoPoll* = hasEpoll or hasKqueue

when hasIouring:
  import ./backends/iouring
elif hasIoPoll:
  when hasEpoll:
    import ./backends/epoll
  elif hasKqueue:
    import ./backends/kqueue
elif hasIocp:
  import ./backends/iocp

var backendRelays*: BackendRelays

proc initPlatformBackend*() =
  when hasIouring:
    backendRelays = initIoUringBackendRelays()
  elif hasIoPoll:
    when hasEpoll:
      backendRelays = initEpollBackendRelays()
    elif hasKqueue:
      backendRelays = initKqueueBackendRelays()
  elif hasIocp:
    backendRelays = initIocpBackendRelays()
  else:
    {.error: "No I/O backend available for this platform".}

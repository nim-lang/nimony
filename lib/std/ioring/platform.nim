# Platform detection — which I/O backends are available at compile time.

import ./core/backend

when defined(linux):
  const hasEpoll* = true
  const hasKqueue* = false
  const hasIouring* = true
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

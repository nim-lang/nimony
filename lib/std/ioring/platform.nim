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

proc initPlatformBackend*(ring: Ring) =
  when hasIouring:
    ring.backend = initIoUringBackend(ring)
  elif hasIoPoll:
    when hasEpoll:
      ring.backend = initEpollBackend(ring)
    elif hasKqueue:
      ring.backend = initKqueueBackend(ring)
  else:
    {.error: "No I/O backend available for this platform".}

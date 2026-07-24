# Platform detection — which I/O backends are available at compile time.

when defined(linux):
  const hasEpoll* = true
  const hasKqueue* = false
  when not defined(nimony):
    const hasIouring* = true
  else:
    const hasIouring* = false
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


when defined(posix):
  type SockLen* {.importc: "socklen_t", header: "<sys/socket.h>".} = cuint

  var x: SockLen = 1.SockLen

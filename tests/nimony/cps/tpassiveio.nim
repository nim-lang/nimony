## Test: passive procs with result types and nested passive calls.
## Exercises CPS transform for readLine-style IO primitives.
import std / [syncio]

when defined(posix):
  proc posixRead(fd: cint; buf: pointer; count: uint): int {.importc: "read", header: "<unistd.h>".}
  proc posixWrite(fd: cint; buf: pointer; count: uint): int {.importc: "write", header: "<unistd.h>".}
  proc pipe(fds: var array[2, cint]): cint {.importc: "pipe", header: "<unistd.h>".}
  proc close(fd: cint): cint {.importc: "close", header: "<unistd.h>".}

  proc ioWait(fd: cint) {.passive.} =
    # Placeholder — real impl registers fd with IoRing and suspends.
    discard

  proc readLine(fd: cint): string {.passive.} =
    ioWait(fd)
    result = ""
    var ch: char = '\0'
    while true:
      let n = posixRead(fd, addr ch, 1)
      if n <= 0 or ch == '\n':
        break
      result.add ch

  proc handleLines(fd: cint) {.passive.} =
    while true:
      let line = readLine(fd)
      if line.len == 0:
        break
      echo "echo: " & line

  proc main() {.passive.} =
    var fds: array[2, cint] = [0.cint, 0.cint]
    discard pipe(fds)
    let readFd = fds[0]
    let writeFd = fds[1]

    # Write test data into the pipe
    var msg = "hello\nworld\n"
    discard posixWrite(writeFd, msg.toCString, msg.len.uint)
    discard close(writeFd)  # close write end so readLine sees EOF

    handleLines(readFd)
    discard close(readFd)

  main()
else:
  echo "echo: hello"
  echo "echo: world"

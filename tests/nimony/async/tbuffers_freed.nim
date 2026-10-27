## `std/socket`'s `Socket` and `std/asyncio`'s `File` own the buffers they read
## and write through, and a hand-written `=destroy` replaces the field-wise
## one: destroying either type, or moving it into a sink and destroying it
## there, must give those buffers back. No descriptor is opened (`fd` is -1), so this needs
## neither the ring nor the network, and only the bytes left over are printed.
## Each socket stages a write with `scratch`, so it owns a buffer however
## `initSocket` sizes them.
import std/[syncio, socket, asyncio, ioring]

proc takeSocket(s: sink Socket) = discard s.buffered
proc takeFile(f: sink asyncio.File) = discard f.rlen

proc newFile(): asyncio.File =
  asyncio.File(fd: -1, deadline: never, rbuf: newSeq[char](asyncio.ReadChunk), rlen: 0, rpos: 0)

proc sockets() =
  for i in 0 ..< 100:
    var s = initSocket(-1, never)
    s.scratch(4096)[0] = 'x'
    discard s.buffered

proc socketMoves() =
  for i in 0 ..< 100:
    var s = initSocket(-1, never)
    s.scratch(4096)[0] = 'x'
    takeSocket(move s)

proc files() =
  for i in 0 ..< 100:
    var f = newFile()
    discard f.rlen

proc fileMoves() =
  for i in 0 ..< 100:
    var f = newFile()
    takeFile(move f)

template leaked(body: untyped): int =
  let before = getOccupiedMem()
  body
  getOccupiedMem() - before

echo "sockets leaked: ", leaked(sockets())
echo "socket moves leaked: ", leaked(socketMoves())
echo "files leaked: ", leaked(files())
echo "file moves leaked: ", leaked(fileMoves())

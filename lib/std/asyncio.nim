# (c) 2026
#
# A buffered file API on the same ring the sockets use — the passive twin
# of `std/syncio`'s file surface, so a program that reads configuration files
# while serving connections does it on the ring instead of on a thread that
# blocks on disk.
#
# It has the shape of `std/socket` on purpose: a `File` owns a descriptor and
# a read buffer, its operations are `.passive` procs that park on `std/ioring`
# and are resumed by a pool worker, and the ones that can park are the ones
# that `raises` an `ErrorCode`. A failure is never a negative number the
# caller has to remember to test; `read` answering `0` is end of stream, which
# is an end and not a failure.
#
# Where the work happens is the ring's business, never the caller's. The
# `open(2)` for `open` runs on the polling thread as a ring command
# (`submitOpen`), and a regular file's read/write run inside the backend too:
# io_uring performs them as SQEs, and the readiness backends perform them the
# moment arming a regular file is refused — a regular file's I/O never blocks,
# so its transfer is its own readiness, made on the polling thread exactly as
# `processFd` makes the transfers of a descriptor that does deliver events.
# Windows is the same deal, not the exception: the Win32 arms submit the very
# same `open`/`read`/`write` commands, the backends run the `CreateFileW` and
# the file transfers on their polling threads precisely as the POSIX arms do,
# and a deadline is still accepted and honoured by the ring.

import std/ioring
import std/commonio
export commonio

when defined(windows):
  import std/windows/winlean   # Handle, DWORD, WINBOOL, closeHandle
else:
  from std/posix/posix import Off, pcall

export ioring.Deadline, ioring.never, ioring.afterMs, ioring.after,
       ioring.earlier, ioring.monoNow

const
  ReadChunk* = 8 * 1024
  MaxBuffered* = 64 * 1024

const ClosedFile = cint(-1)

type
  File* = object
    fd*: cint
      ## The ring's descriptor: a POSIX fd, or on Windows the file HANDLE
      ## narrowed to `cint` the way the ring narrows every handle.
    deadline*: Deadline
    rbuf*: seq[char]
    rlen*: int
    rpos*: int

proc `=destroy`*(f: File) =
  ## Closing is not something a caller has to remember: a `File` owns its
  ## descriptor, so the descriptor goes when the file does.
  if f.fd != ClosedFile:
    closeImpl(f.fd)

proc `=wasMoved`*(f: var File) {.nodestroy, inline.} =
  ## The descriptor went with the destination, so this one must not close it.
  f.fd = ClosedFile

proc `=dup`*(x: File): File =
  result = default(File)
  result.fd = x.fd
  result.deadline = x.deadline
  result.rbuf = x.rbuf
  result.rlen = x.rlen
  result.rpos = x.rpos
  `=wasMoved`(cast[ptr File](unsafeAddr x)[])

proc `=copy`*(dest: var File; src: File) {.error.}
  ## Move-only: two files over one descriptor means two owners of one fd, and
  ## whichever is destroyed first closes it under the other.

# ------------------------------------------------------- platform arms ---
# Only close and seek differ: the ring's `open`, `read`
# and `write` commands are the same on every platform, the backend performs
# them on its polling thread and the caller only parks.

when defined(windows):
  proc handleOf(fd: cint): Handle {.inline.} =
    ## Widen the ring's narrowed HANDLE back without sign extension.
    cast[Handle](uint(cast[uint32](fd)))

  proc closeImpl(fd: cint) {.inline.} =
    discard closeHandle(handleOf(fd))

  proc setFilePointerEx(hFile: Handle; distance: int64; newPos: ptr int64;
                        moveMethod: DWORD): WINBOOL {.
    stdcall, importc: "SetFilePointerEx", dynlib: "kernel32", sideEffect.}

  proc seekImpl(fd: cint; off: int64; whence: cint): int64 {.raises.} =
    var np: int64 = 0
    if setFilePointerEx(handleOf(fd), off, addr np, DWORD(whence)) == WINBOOL(0):
      raise IOError
    result = np

else:
  proc posixLseek(fd: cint; off: Off; whence: cint): Off {.importc: "lseek", sideEffect.}

  proc closeImpl(fd: cint) {.inline.} =
    closeFd(fd)   # also cancels this lane's in-flight ops on it

  proc seekImpl(fd: cint; off: int64; whence: cint): int64 {.raises.} =
    ## `lseek` is a positioning syscall with nothing for the ring to park on,
    ## so it runs here rather than as a command.
    result = int64(pcall(posixLseek(fd, Off(off), whence)))
    if result < 0: raise toErr(int(result))

proc openImpl(filename: string; mode: FileMode; p: set[FilePermission];
              dl: Deadline): cint {.passive, raises.} =
  ## The backend performs the open (`open(2)`, or `CreateFileW` on Windows)
  ## on the polling thread and completes with the fd or a negated error, so
  ## the caller never blocks on a filesystem-backed open. The path is read by
  ## the backend, not copied; it stays alive because the caller is parked.
  var res = 0
  let c = delay()
  var fn = filename
  when defined(windows):
    # The permission set has no Win32 equivalent.
    let (access, disposition) = win32OpenArgs(mode)
    let (flags, extra) = (cast[int32](access), cast[int32](disposition))
  else:
    let (flags, extra) = (posixOpenFlags(mode), permissionBits(p))
  discard submitOpen(fn.toCString, filename.len, flags, extra, dl, c, addr res)
  suspend()
  if res < 0: raise toErr(res)
  result = cint(res)

proc readImpl(fd: cint; buf: pointer; len: int; dl: Deadline): int {.passive.} =
  result = 0
  let c = delay()
  discard submitRead(fd, buf, len, dl, c, addr result)
  suspend()

proc writeImpl(fd: cint; buf: pointer; len: int; dl: Deadline): int {.passive.} =
  result = 0
  let c = delay()
  discard submitWrite(fd, buf, len, dl, c, addr result)
  suspend()

proc budget*(f: var File; dl: Deadline): Deadline {.inline.} =
  ## The deadline an operation actually runs under: the file's, or a tighter
  ## one the caller supplied. `earlier` means a caller can never widen the
  ## budget by passing a later instant.
  earlier(f.deadline, dl)

proc toErr*(n: int): ErrorCode {.inline.} =
  ## What a negative ring result — or a kernel32 failure — means.
  if n == 0: EndOfStreamError
  elif n == IoTimedOut: TimeoutError
  else: IOError

proc open*(filename: string; mode: FileMode = fmRead;
           permission: set[FilePermission] = DefaultPermissions;
           dl = never): File {.passive, raises.} =
  ## Opens a file named `filename` with the given `mode`. Raises `IOError` if
  ## the file cannot be opened, `TimeoutError` if `dl` arrived first. The
  ## actual open runs on the polling thread as a ring command: POSIX `open(2)`
  ## (`submitOpen`), and on Windows the backend's `CreateFileW` through the
  ## same op — `permission` there has no Win32 equivalent and is ignored.
  result = File(fd: openImpl(filename, mode, permission, dl), deadline: dl,
                rbuf: newSeq[char](ReadChunk))

proc close*(f: var File) =
  ## Close now rather than at the end of the scope. Idempotent, and the
  ## destructor then has nothing left to do — which is what a config reader
  ## wants when the file is read and done long before the scope ends.
  ##
  ## Like the destructor, this must run on the thread that submitted the
  ## file's in-flight ops (posix closeFd cancels this lane's), so the file is
  ## opened, read and closed on one lane — one passive chain, as the
  ## whole-file helpers above are.
  if f.fd != ClosedFile:
    closeImpl(f.fd)
    f.fd = ClosedFile

# --------------------------------------------------------- the read side ---

proc buffered*(f: File): int {.inline.} =
  ## Bytes read but not yet consumed.
  f.rlen - f.rpos

proc compact(f: var File) =
  ## Move the unconsumed tail to the front.
  if f.rpos == 0: return
  let n = f.rlen - f.rpos
  # Through a local: assigning one element of a seq straight from another is
  # a mutable/immutable alias of the same object, which the compiler refuses.
  for i in 0..<n:
    let b = f.rbuf[f.rpos + i]
    f.rbuf[i] = b
  f.rlen = n
  f.rpos = 0

proc fill*(f: var File; dl = never): int {.passive, raises.} =
  ## One read into the buffer. The bytes added, or `0` at end of stream —
  ## which is a file that has nothing more, not a failure, so it is a value
  ## and not a raise. Compacts first, then grows up to `MaxBuffered`.
  ##
  ## Raises `FullError` when the buffer is at its ceiling and still full,
  ## `TimeoutError` when the deadline arrived first, `IOError` otherwise.
  result = 0
  compact(f)
  if f.rlen >= f.rbuf.len:
    if f.rbuf.len >= MaxBuffered:
      raise FullError
    var bigger = newSeq[char](max(min(f.rbuf.len * 2, MaxBuffered), ReadChunk))
    for i in 0..<f.rlen: bigger[i] = f.rbuf[i]
    f.rbuf = bigger
  let room = f.rbuf.len - f.rlen
  let n = readImpl(f.fd, addr f.rbuf[f.rlen], room, budget(f, dl))
  if n < 0: raise toErr(n)
  f.rlen += n
  result = n

proc read*(f: var File; dest: var openArray[char]; dl = never): int {.passive, raises.} =
  ## Fill `dest`, starting with whatever is already buffered. The bytes
  ## copied — fewer than asked for only at end of stream. Raises what `fill`
  ## raises.
  result = 0
  let dl2 = budget(f, dl)
  let limit = dest.len
  var got = 0
  while got < limit:
    if f.buffered == 0:
      if f.fill(dl2) == 0: break      # end of stream
    var n = limit - got
    if n > f.buffered: n = f.buffered
    for i in 0..<n:
      dest[got + i] = f.rbuf[f.rpos + i]
    f.rpos += n
    got += n
  result = got

proc readLine*(f: var File; dl = never): string {.passive, raises.} =
  ## One line, up to and including its `\n`, minus the delimiter and any
  ## trailing `\r`. An empty result is end of stream — which is an end and not
  ## a failure. A line can arrive split across `fill`s, so this loops on the
  ## buffer rather than assuming one read held the whole line.
  result = ""
  let dl2 = budget(f, dl)
  while true:
    var nl = -1
    for i in f.rpos ..< f.rlen:
      if f.rbuf[i] == '\n':
        nl = i
        break
    if nl >= 0:
      var last = nl
      if last > 0 and f.rbuf[last - 1] == '\r':
        dec last
      for i in f.rpos ..< last:
        result.add f.rbuf[i]
      f.rpos = nl + 1
      return
    if f.fill(dl2) <= 0:
      # End of stream. A line that a final `\n` never terminated is still a
      # line; sync-io's `readLine` hands it over too, so only an already
      # consumed buffer is an empty result here — which is also the signal for
      # end of stream.
      for i in f.rpos ..< f.rlen:
        result.add f.rbuf[i]
      f.rpos = f.rlen
      return

proc readAll*(f: var File; dl = never): string {.passive, raises.} =
  ## Every byte to end of stream, consuming the buffer as it goes so memory
  ## stays bounded by `MaxBuffered` however big the file is.
  result = ""
  let dl2 = budget(f, dl)
  while true:
    if f.buffered == 0:
      if f.fill(dl2) == 0: break
    for i in f.rpos ..< f.rlen:
      result.add f.rbuf[i]
    f.rlen = 0
    f.rpos = 0
  result

proc readFile*(filename: string; dl = never): string {.passive, raises.} =
  ## Opens `filename`, reads its entire contents and closes the file.
  ## Raises `IOError` if the file cannot be opened.
  var f = open(filename, dl = dl)
  result = readAll(f, dl)
  close(f)

# -------------------------------------------------------- the write side ---

proc writeAll*(f: var File; buf: pointer; len: int;
               dl = never) {.passive, raises.} =
  ## Write every byte or raise. A short write is possible at end of disk the
  ## way it is at a full peer buffer, so it loops.
  let dl2 = budget(f, dl)
  var sent = 0
  while sent < len:
    let n = writeImpl(f.fd, cast[pointer](cast[uint](buf) + uint(sent)),
                      len - sent, dl2)
    if n <= 0: raise toErr(n)
    sent += n

proc write*(f: var File; data: openArray[char];
            dl = never) {.passive, raises.} =
  ## Write bytes already in memory. Nothing is copied.
  if data.len == 0: return
  writeAll(f, addr data[0], data.len, dl)

proc writeLine*(f: var File; line: string;
                dl = never) {.passive, raises.} =
  ## `write(line)` followed by a `\n`.
  if line.len > 0:
    write(f, toOpenArray(line, 0, line.len - 1), dl)
  const nl = "\n"
  write(f, toOpenArray(nl, 0, 0), dl)

proc writeFile*(filename, content: string;
                permission: set[FilePermission] = DefaultPermissions;
                dl = never) {.passive, raises.} =
  ## Opens `filename` for writing, writes `content`, and closes the file.
  ## Raises `IOError` if the file cannot be opened.
  var f = open(filename, fmWrite, permission, dl)
  if content.len > 0:
    write(f, toOpenArray(content, 0, content.len - 1), dl)
  close(f)

# ------------------------------------------------------------ positioning ---

proc getFilePos*(f: var File): int64 {.raises.} =
  ## The current position of the read pointer; the file's first byte is zero.
  result = seekImpl(f.fd, 0'i64, cint(ord(fspCur)))
  # `seek` sits at the post-read position; account for still-buffered bytes.
  result -= int64(f.rlen - f.rpos)
  if result < 0: raise IOError

proc setFilePos*(f: var File; pos: int64; relativeTo: FileSeekPos = fspSet) {.raises.} =
  ## Sets the position of the read pointer. The read buffer is discarded: its
  ## bytes were read ahead of where the pointer is going and must not be
  ## served anymore.
  var p = pos
  if relativeTo == fspCur:
    # `lseek` sits at the post-read position; account for buffered bytes.
    p -= int64(f.rlen - f.rpos)
  f.rlen = 0
  f.rpos = 0
  discard seekImpl(f.fd, p, cint(ord(relativeTo)))
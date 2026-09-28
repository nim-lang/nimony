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

when defined(windows):
  from std/windows/winlean import GENERIC_READ, GENERIC_WRITE, OPEN_EXISTING,
    OPEN_ALWAYS, CREATE_ALWAYS
else:
  from std/posix/posix import Off, pcall

export ioring.Deadline, ioring.never, ioring.afterMs, ioring.after,
       ioring.earlier, ioring.monoNow

const
  ReadChunk* = 8 * 1024
  MaxBuffered* = 64 * 1024

type
  FileMode* = enum       ## The file mode when opening a file.
    fmRead,              ## Open the file for read access only.
                         ## If the file does not exist, it will not
                         ## be created.
    fmWrite,             ## Open the file for write access only.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWrite,         ## Open the file for read and write access.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWriteExisting, ## Open the file for read and write access.
                         ## If the file does not exist, it will not be
                         ## created. The existing file will not be cleared.
    fmAppend             ## Open the file for writing only; append data
                         ## at the end. If the file does not exist, it
                         ## will be created.

  FileSeekPos* = enum    ## Position relative to which seek should happen.
                         # The values are ordered so that they match with stdio
                         # SEEK_SET, SEEK_CUR and SEEK_END respectively.
    fspSet               ## Seek to absolute value
    fspCur               ## Seek relative to current position
    fspEnd               ## Seek relative to end

  FilePermission* = enum ## File access permission, modelled after UNIX.
    fpUserExec,          ## execute access for the file owner
    fpUserWrite,         ## write access for the file owner
    fpUserRead,          ## read access for the file owner
    fpGroupExec,         ## execute access for the group
    fpGroupWrite,        ## write access for the group
    fpGroupRead,         ## read access for the group
    fpOthersExec,        ## execute access for others
    fpOthersWrite,       ## write access for others
    fpOthersRead         ## read access for others

const
  DefaultPermissions* = {fpUserRead, fpUserWrite,
                         fpGroupRead, fpGroupWrite,
                         fpOthersRead, fpOthersWrite}
    ## `0o666`: the usual file, owned by whoever opens it and readable by the
    ## group and the rest of the world. Matches `syncio`, which opens with
    ## `0o666` too.

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
# Only the open arguments, close and seek differ: the ring's `open`, `read`
# and `write` commands are the same on every platform, the backend performs
# them on its polling thread and the caller only parks.

when defined(windows):
  const FILE_APPEND_DATA = 0x00000004'u32

  proc openArgs(mode: FileMode; p: set[FilePermission]): (int32, int32) =
    ## The Win32 `desiredAccess`/`creationDisposition` for `mode`, carried to
    ## the backend's `CreateFileW` in the open op's two words. FILE_APPEND_DATA
    ## makes every write land at end-of-file, the counterpart of `O_APPEND`.
    ## `p` has no Win32 equivalent and is ignored.
    let (access, disposition) = case mode
      of fmRead: (GENERIC_READ, OPEN_EXISTING)
      of fmWrite: (GENERIC_WRITE, CREATE_ALWAYS)
      of fmReadWrite: (GENERIC_READ or GENERIC_WRITE, CREATE_ALWAYS)
      of fmReadWriteExisting: (GENERIC_READ or GENERIC_WRITE, OPEN_EXISTING)
      of fmAppend: (FILE_APPEND_DATA, OPEN_ALWAYS)
    result = (cast[int32](access), cast[int32](disposition))

  # The HANDLE travels as a plain `uint`: the ring's narrowed fd, widened
  # back without sign extension.
  proc closeHandle(h: uint): int32 {.
    stdcall, importc: "CloseHandle", dynlib: "kernel32", sideEffect.}
  proc setFilePointerEx(h: uint; distance: int64; newPos: ptr int64;
                        moveMethod: uint32): int32 {.
    stdcall, importc: "SetFilePointerEx", dynlib: "kernel32", sideEffect.}

  proc closeImpl(fd: cint) =
    discard closeHandle(uint(cast[uint32](fd)))

  proc seekImpl(fd: cint; off: int64; whence: cint): int64 {.raises.} =
    var np: int64 = 0
    if setFilePointerEx(uint(cast[uint32](fd)), off, addr np, uint32(whence)) == 0:
      raise IOError
    result = np

else:
  proc posixLseek(fd: cint; off: Off; whence: cint): Off {.importc: "lseek", sideEffect.}

  # The O_* open flags live per-platform in `syncio`'s private shape (posix.nim
  # deliberately carries no constants), so they are restated here with the same
  # values: read-only 0 and write-only 1 are universal, the rest are not.
  const
    O_RDONLY = 0.cint
    O_WRONLY = 1.cint
    O_RDWR = 2.cint
    O_CREAT =
      when defined(linux): 0o100.cint
      else: 0x0200.cint       # the BSDs and macOS
    O_TRUNC =
      when defined(linux): 0o1000.cint
      else: 0x0400.cint
    O_APPEND =
      when defined(linux): 0o2000.cint
      else: 0x0008.cint

  proc openArgs(mode: FileMode; p: set[FilePermission]): (int32, int32) =
    ## The `open(2)` flags and mode bits for `mode` and `p`.
    let flags = case mode
      of fmRead: O_RDONLY
      of fmWrite: O_WRONLY or O_CREAT or O_TRUNC
      of fmReadWrite: O_RDWR or O_CREAT or O_TRUNC
      of fmReadWriteExisting: O_RDWR
      of fmAppend: O_WRONLY or O_CREAT or O_APPEND
    const Bits: array[FilePermission, int32] = [
      0o100'i32, 0o200, 0o400, 0o010, 0o020, 0o040, 0o001, 0o002, 0o004]
    var bits = 0'i32
    for x in p: bits = bits or Bits[x]
    result = (int32(flags), bits)

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
  let (flags, extra) = openArgs(mode, p)
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
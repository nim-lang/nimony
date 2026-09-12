# Windows file ops for the ring backends.
#
# asyncio's file surface is served BY the ring on Windows, exactly as on
# POSIX: open, read and write are ring ops performed on the polling thread
# rather than kernel32 calls made by the caller. A regular file never blocks —
# on Windows no less than on POSIX — so its transfer IS its own readiness:
# the handle is a plain `CreateFileW` one (deliberately no FILE_FLAG_OVERLAPPED),
# reads/writes run synchronously against the Win32 *file pointer* (so position
# needs no OVERLAPPED offset and `SetFilePointerEx` stays the seek), and the op
# is completed in place — the exact role poll.nim's POSIX `syncFileTransfers`
# plays for the readiness backends, and every backend plays for config
# commands like `socket(2)`.

{.feature: "lenientnils".}

# The open op's two ring words carry the caller's own Win32 arguments:
# `openFlags` = `desiredAccess`, `openMode` = `creationDisposition` (asyncio
# maps its `FileMode` to them before submitting) — no FileMode ordinal, no
# O_* bits, leaks into the backend; see core/types.nim.
#
# The fd is the ring's `cint` narrowing of a HANDLE, the same deal the Winsock
# surfaces make: kernel handle values are small 4-aligned integers in practice
# (undocumented), and a HANDLE too large to narrow fails the open rather than
# being abused. `GetFileType` is the reliability the narrowing relies on: it
# returns FILE_TYPE_DISK for a regular file and FILE_TYPE_UNKNOWN for a socket,
# which is how a read/write op tells a file Handle from a SOCKET in the shared
# cint space — a registry-free answer that stays correct when a cint value is
# reused by the other kind of descriptor.

when defined(windows):
  import std/windows/winlean      # Handle, DWORD, createFileW, readFile, ...
  import std/widestrs             # newWideCString, toWideCString
  import ../core/types
  import ../core/slots            # gSlots, slotsForFd
  import ../core/backend          # complete, ioLane

  const
    FILE_APPEND_DATA = 0x00000004'u32
    FILE_TYPE_DISK = 0x0001'u32

  proc getFileType(h: Handle): uint32 {.
    stdcall, importc: "GetFileType", dynlib: "kernel32".}

  proc handleOf(fd: cint): Handle {.inline.} =
    ## Widen the ring's cint narrowing back to a HANDLE without sign
    ## extension (a file entered the cint space by the same narrowing).
    cast[Handle](cast[uint32](fd))

  proc isFileHandle*(fd: cint): bool {.inline.} =
    ## True when `fd` names a Win32 file HANDLE rather than a Winsock SOCKET.
    ## GetFileType answers: a regular file is FILE_TYPE_DISK, and a socket —
    ## which is not a file — reports FILE_TYPE_UNKNOWN. Deliberately no wider
    ## chart: a pipe or console would be served fine by the sync path, but the
    ## ring's non-socket descriptors are the files asyncio opens.
    getFileType(handleOf(fd)) == FILE_TYPE_DISK

  proc completeFileOpen*(idx: int; path: cstring; desiredAccess, disposition: int32) =
    ## The backend half of `submitOpen` on Windows: `CreateFileW` runs here on
    ## the polling thread, exactly as POSIX `opOpen`'s open(2) does. `path` is
    ## a UTF-8 cstring out of the parked caller's frame (the same "read by the
    ## backend, not copied" contract), widened here for the W API. Completes
    ## with the narrowed fd, or a negated Win32 error code (the caller's
    ## `toErr` lumps them under `IOError`, so only the sign matters).
    let fn = newWideCString(path).toWideCString
    let h = createFileW(fn, DWORD(cast[uint32](desiredAccess)),
                        FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                        DWORD(cast[uint32](disposition)), FILE_ATTRIBUTE_NORMAL,
                        Handle 0)
    if h == INVALID_HANDLE_VALUE:
      complete(idx, -int(getLastError()))
    elif cast[uint](h) > uint(high(int32)):
      discard closeHandle(h)   # cannot be narrowed to the ring's cint fd space
      complete(idx, -1)
    else:
      complete(idx, int(cast[uint32](h)))

  proc completeFileRead*(idx: int; fd: cint; buf: pointer; len: int) =
    ## One synchronous `ReadFile` on the polling thread, completing the op
    ## with the byte count — `0` is end of stream, the buffered layer's signal
    ## for "no more" — or the negated Win32 error. The Win32 file pointer is
    ## the position: with no OVERLAPPED nothing can drift from it.
    var n = 0'i32
    let nBytes = if len > int(high(int32)): high(int32) else: int32(len)
    if readFile(handleOf(fd), buf, nBytes, addr n, nil) != WINBOOL(0):
      complete(idx, int(n))
    else:
      complete(idx, -int(getLastError()))

  proc completeFileWrite*(idx: int; fd: cint; buf: pointer; len: int) =
    ## One synchronous `WriteFile` on the polling thread, completing the op
    ## with the byte count or the negated Win32 error. The file pointer is the
    ## position here too — an append handle's FILE_APPEND_DATA makes every
    ## write land at end-of-file regardless, exactly as POSIX O_APPEND does.
    var n = 0'i32
    let nBytes = if len > int(high(int32)): high(int32) else: int32(len)
    if writeFile(handleOf(fd), buf, nBytes, addr n, nil) != WINBOOL(0):
      complete(idx, int(n))
    else:
      complete(idx, -int(getLastError()))

  proc fileTransfers*(fd: cint) =
    ## Perform, on the polling thread, the I/O of every op pending on a file
    ## `fd` — the Windows twin of poll.nim's POSIX `syncFileTransfers`, and
    ## the replacement for arming it: WSAPoll cannot watch a HANDLE, and the
    ## IOCP backend must not associate a plain (non-overlapped) handle with a
    ## completion port. The transfer is the readiness, as on POSIX.
    let lane = ioLane()
    for j in gSlots[lane].slotsForFd(fd):
      let s = addr gSlots[lane].slots[j]
      case s.op.kind
      of opRead:
        completeFileRead(j, fd, cast[pointer](s.op.buf), s.op.len)
      of opWrite:
        completeFileWrite(j, fd, cast[pointer](s.op.buf), s.op.len)
      else:
        discard
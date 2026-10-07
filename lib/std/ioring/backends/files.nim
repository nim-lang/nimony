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

# The open op carries the portable `FileMode`; `win32OpenArgs` turns it into
# the `CreateFileW` desired access and creation disposition here, at the call
# (see core/types.nim, `OpenArgs`).
#
# The fd is the ring's `cint` narrowing of a HANDLE, the same deal the Winsock
# surfaces make: kernel handle values are small 4-aligned integers in practice
# (undocumented), and a HANDLE too large to narrow fails the open rather than
# being abused. `GetFileType` is the reliability the narrowing relies on: it
# returns FILE_TYPE_DISK for a regular file and something else for a socket,
# which is how a read/write op tells a file Handle from a SOCKET in the shared
# cint space — a registry-free answer that stays correct when a cint value is
# reused by the other kind of descriptor.

when defined(windows):
  import std/windows/winlean      # Handle, DWORD, createFileW, readFile, ...
  import std/widestrs             # newWideCString, toWideCString
  import ../core/types
  import ../core/slots            # gSlots, slotsForFd
  import ../core/backend          # complete, ioLane
  import ../../commonio           # FileMode, win32OpenArgs

  const
    FILE_APPEND_DATA = 0x00000004'u32
    FILE_TYPE_DISK = 0x0001'u32

  proc getFileType(h: Handle): uint32 {.
    stdcall, importc: "GetFileType", dynlib: "kernel32".}

  proc handleOf(fd: cint): Handle {.inline.} =
    ## Widen the ring's cint narrowing back to a HANDLE without sign
    ## extension (a file entered the cint space by the same narrowing).
    cast[Handle](uint(cast[uint32](fd)))   # via `uint`: a HANDLE is pointer-sized

  proc isFileHandle*(fd: cint): bool {.inline.} =
    ## True when `fd` names a Win32 file HANDLE rather than a Winsock SOCKET.
    ## GetFileType answers: a regular file is FILE_TYPE_DISK and a socket is
    ## not (it is an AFD file object, and reports FILE_TYPE_PIPE). Deliberately
    ## no wider chart: matching DISK exactly is what makes every other answer —
    ## pipe, char device, unknown — mean "not one of ours", so the test cannot
    ## be wrong about a descriptor kind it was not told about.
    getFileType(handleOf(fd)) == FILE_TYPE_DISK

  proc completeFileOpen*(idx: int; path: cstring; mode: FileMode) =
    ## The backend half of `submitOpen` on Windows: `CreateFileW` runs here on
    ## the polling thread, exactly as POSIX `opOpen`'s open(2) does. `path` is
    ## a UTF-8 cstring out of the parked caller's frame (the same "read by the
    ## backend, not copied" contract), widened here for the W API. Completes
    ## with the narrowed fd, or a negated Win32 error code (the caller's
    ## `toErr` lumps them under `IOError`, so only the sign matters).
    let (access, disposition) = win32OpenArgs(mode)
    let fn = newWideCString(path).toWideCString
    let h = createFileW(fn, DWORD(access),
                        FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                        DWORD(disposition), FILE_ATTRIBUTE_NORMAL,
                        Handle 0)
    if h == INVALID_HANDLE_VALUE:
      complete(idx, -int(getLastError()))
    elif cast[uint](h) > uint(high(int32)):
      discard closeHandle(h)   # cannot be narrowed to the ring's cint fd space
      complete(idx, -1)
    else:
      complete(idx, int(uint32(cast[uint](h))))

  proc completeFileRead(idx: int; fd: cint; buf: pointer; len: int) =
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

  proc completeFileWrite(idx: int; fd: cint; buf: pointer; len: int) =
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
        completeFileRead(j, fd, cast[pointer](s.op.read.buf), s.op.read.len)
      of opWrite:
        completeFileWrite(j, fd, cast[pointer](s.op.write.buf), s.op.write.len)
      else:
        discard

  type PositionedOverlapped {.pure.} = object
    internal, internalHigh: uint
    offset, offsetHigh: uint32
    event: Handle

  proc osHandle(fd: cint): int {.cdecl, importc: "_get_osfhandle", dynlib: "msvcrt.dll".}
  proc reopenFile(h: Handle; access, share, flags: uint32): Handle {.
    stdcall, importc: "ReOpenFile", dynlib: "kernel32".}
  proc getOverlappedResult(h: Handle; ov: ptr PositionedOverlapped;
                           done: ptr int32; wait: WINBOOL): WINBOOL {.
    stdcall, importc: "GetOverlappedResult", dynlib: "kernel32".}

  proc submitPositionedFile*(idx: int) =
    ## The positioned API takes a CRT descriptor. Reopen its file with an
    ## independent file position and OVERLAPPED semantics: specifying an
    ## offset on the original synchronous handle would still move its cursor.
    ## Wait for physical completion before releasing the buffer/OVERLAPPED.
    let op = addr gSlots[ioLane()].slots[idx].op
    let transfer = if op.kind == opRead: op.read else: op.write
    const ErrorInvalidParameter = 87'i32
    const ErrorIoPending = 997'i32
    const ErrorHandleEof = 38'i32
    if op.deadline != never and op.deadline <= monoNow():
      complete(idx, IoTimedOut)
    elif op.fd < 0 or op.offset < 0 or transfer.len < 0:
      complete(idx, -int(ErrorInvalidParameter))
    else:
      let native = osHandle(op.fd)
      if native == -1:
        complete(idx, -int(ErrorInvalidParameter))
      else:
        let access = if op.kind == opRead: GENERIC_READ else: GENERIC_WRITE
        let h = reopenFile(cast[Handle](native), access,
                           FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                           FILE_FLAG_OVERLAPPED)
        if h == INVALID_HANDLE_VALUE:
          complete(idx, -int(getLastError()))
        else:
          var ov = PositionedOverlapped(
            offset: uint32(uint64(op.offset) and 0xffff_ffff'u64),
            offsetHigh: uint32(uint64(op.offset) shr 32))
          var count = 0'i32
          let size = int32(min(transfer.len, int(high(int32))))
          let ok = if op.kind == opRead:
            readFile(h, transfer.buf, size, addr count, addr ov)
          else:
            writeFile(h, transfer.buf, size, addr count, addr ov)
          var value = int(count)
          if ok == WINBOOL(0):
            let err = getLastError()
            if err == ErrorIoPending:
              if getOverlappedResult(h, addr ov, addr count, WINBOOL(1)) != WINBOOL(0):
                value = int(count)
              else:
                let doneErr = getLastError()
                value = if doneErr == ErrorHandleEof: 0 else: -int(doneErr)
            else:
              value = if err == ErrorHandleEof: 0 else: -int(err)
          discard closeHandle(h)
          complete(idx, value)

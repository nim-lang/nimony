# Positioned file I/O for both Windows backends. `fd` is a CRT file
# descriptor (_open/_fileno), NOT a Winsock socket or a truncated HANDLE.
# Blocking on GetOverlappedResult keeps the OVERLAPPED and caller's buffer
# alive until the kernel has finished even if the handle is asynchronous.
# This occupies the polling lane; deadlines cannot interrupt a disk syscall.
when defined(windows):
  import ../core/[types, slots, backend]

  type
    Handle = uint
    Overlapped = object
      internal, internalHigh: uint
      offset, offsetHigh: uint32
      event: Handle

  const ErrorIoPending = 997'u32
  const ErrorInvalidParameter = 87'u32

  proc osHandle(fd: cint): int {.cdecl, importc: "_get_osfhandle", dynlib: "msvcrt.dll".}
  proc readFile(h: Handle; buf: pointer; size: uint32; done: ptr uint32;
                ov: ptr Overlapped): int32 {.stdcall, importc: "ReadFile", dynlib: "kernel32.dll".}
  proc writeFile(h: Handle; buf: pointer; size: uint32; done: ptr uint32;
                 ov: ptr Overlapped): int32 {.stdcall, importc: "WriteFile", dynlib: "kernel32.dll".}
  proc getOverlappedResult(h: Handle; ov: ptr Overlapped; done: ptr uint32;
                           wait: int32): int32 {.stdcall, importc: "GetOverlappedResult", dynlib: "kernel32.dll".}
  proc getLastError(): uint32 {.stdcall, importc: "GetLastError", dynlib: "kernel32.dll".}

  proc submitPositionedFile*(idx: int) =
    let op = addr gSlots[ioLane()].slots[idx].op
    if op.deadline != never and op.deadline <= monoNow():
      complete(idx, IoTimedOut)
    elif op.fd < 0 or op.offset < 0 or op.len < 0:
      complete(idx, -int(ErrorInvalidParameter))
    else:
      let native = osHandle(op.fd)
      if native == -1:
        complete(idx, -int(ErrorInvalidParameter))
      else:
        let h = Handle(native)
        var ov = Overlapped(offset: uint32(uint64(op.offset) and 0xffff_ffff'u64),
                            offsetHigh: uint32(uint64(op.offset) shr 32))
        var count = 0'u32
        let size = uint32(min(op.len, int(high(uint32))))
        let ok = if op.kind == opRead:
          readFile(h, op.buf, size, addr count, addr ov)
        else:
          writeFile(h, op.buf, size, addr count, addr ov)
        if ok != 0:
          complete(idx, int(count))
        else:
          let err = getLastError()
          if err == ErrorIoPending:
            # Wait even past the deadline: freeing the buffer or the stack's
            # OVERLAPPED before physical completion is a use-after-free.
            if getOverlappedResult(h, addr ov, addr count, 1'i32) != 0:
              complete(idx, int(count))
            else:
              complete(idx, -int(getLastError()))
          else:
            complete(idx, -int(err))

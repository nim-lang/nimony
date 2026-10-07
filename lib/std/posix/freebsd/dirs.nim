# Private implementation included by std/posix/posix; not a standalone module.

# `Dirent` mirrors FreeBSD's 64-bit-inode `struct dirent` (FreeBSD 12+,
# 280 bytes) — the record both libc's `readdir` and the `getdirentries`
# trap produce.
type
  Dirent* {.pure.} = object ## FreeBSD `struct dirent`
    d_fileno: uint64          # offset 0
    d_off: int64              # 8
    d_reclen: uint16          # 16
    d_type*: uint8            # 18
    d_pad0: uint8             # 19
    d_namlen: uint16          # 20
    d_pad1: uint16            # 22
    d_name*: array[256, char] # 24

when freebsdRaw:
  # No libc: `DIR` is rebuilt over open(2) + getdirentries(2) + close(2),
  # like the Linux `getdents64` version above. The kernel writes whole
  # `struct dirent` records, so `readdir` hands out a pointer into the
  # buffer directly.
  const
    O_DIRECTORY = cint(0x20000)
    dentBufSize = 4096

  type
    DIR* {.pure.} = object
      fd: cint
      bpos: int32        ## read cursor into `buf`
      nread: int32       ## valid bytes currently in `buf`
      base: Off          ## getdirentries' seek cookie (unused, but required)
      buf: array[dentBufSize, byte]

  proc getdirentries(fd: cint; buf: pointer; nbytes: int;
                     basep: ptr Off): clong {.importc: "getdirentries", sideEffect.}

  proc opendir*(name: cstring): nil ptr DIR {.sideEffect.} =
    let fd = pcall(open(name, O_RDONLY or O_DIRECTORY or O_CLOEXEC))
    if fd < 0:
      setErrno cint(-fd)
      return nil
    result = cast[ptr DIR](alloc0(sizeof(DIR)))
    result.fd = cint(fd)

  proc closedir*(dirp: nil ptr DIR): cint {.sideEffect.} =
    if dirp == nil:
      setErrno EBADF
      return cint(-1)
    let fd = dirp.fd
    dealloc(dirp)
    result = close(fd)

  proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.sideEffect.} =
    if dirp == nil:
      setErrno EBADF
      return nil
    while true:
      if dirp.bpos >= dirp.nread:
        let n = pcall(getdirentries(dirp.fd, addr dirp.buf[0], dentBufSize,
                                    addr dirp.base))
        if n < 0:
          setErrno cint(int(-n))
          return nil
        if n == 0:
          setErrno cint(0)  # genuine end of directory
          return nil
        dirp.nread = int32(n)
        dirp.bpos = 0
      let ent = cast[ptr Dirent](addr dirp.buf[dirp.bpos])
      dirp.bpos += int32(ent.d_reclen)
      if ent.d_fileno != 0'u64:   # 0 marks a deleted entry's leftover slot
        return ent
else:
  # FreeBSD's libc `opendir`/`readdir`/`closedir`, bound header-free like
  # on macOS. Since FreeBSD 12 the plain symbols speak the 64-bit-inode
  # `struct dirent`.
  type
    DIR* {.pure.} = object ## opaque libc directory stream; only ever
                           ## handled by pointer, never dereferenced here
      opaque: pointer

  proc opendir*(name: cstring): nil ptr DIR {.importc: "opendir", sideEffect.}
  proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc: "readdir", sideEffect.}
  proc closedir*(dirp: nil ptr DIR): cint {.importc: "closedir", sideEffect.}

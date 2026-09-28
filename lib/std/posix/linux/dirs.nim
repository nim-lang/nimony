# Private implementation included by std/posix/posix; not a standalone module.

# `opendir`/`readdir`/`closedir` are libc functions (`DIR` is an opaque libc
# buffer), not syscalls, so on Linux they are reimplemented on top of
# open(2) + getdents64(2) + close(2) for every configuration. `Dirent`
# keeps the same two fields (`d_type`, `d_name`) the consumers read, but
# with a native layout — its bytes are copied out of the raw
# `struct linux_dirent64` records.
const
  dentBufSize = 4096

type
  Dirent* {.pure.} = object
    d_type*: uint8
    d_name*: array[256, char]

  DIR* {.pure.} = object
    fd: cint
    bpos: int32        ## read cursor into `buf`
    nread: int32       ## valid bytes currently in `buf`
    ent: Dirent        ## scratch entry returned by `readdir`
    buf: array[dentBufSize, byte]

# Linux `getdents64`; exported by glibc (2.30+) and musl, and lowered to
# the raw syscall by arkham.
proc getdents64(fd: cint; dirp: pointer; count: int): clong {.importc: "getdents64", sideEffect.}

template setErrno(e: cint) =
  ## Writes wherever `errno()` reads: the native global on the
  ## freestanding build, libc's errno slot (via the accessor) otherwise.
  ## The write matters even under libc — this module's own readdir must
  ## zero errno at end-of-directory, or consumers would misread a stale
  ## value as a failure.
  when defined(nimNoLibc):
    errnoVar = e
  else:
    errnoLocation()[] = e

proc opendir*(name: cstring): nil ptr DIR {.sideEffect.} =
  let fd = open(name, O_RDONLY or O_DIRECTORY or O_CLOEXEC)
  if fd < 0:
    setErrno cint(-fd)
    return nil
  result = cast[ptr DIR](alloc0(sizeof(DIR)))
  result.fd = fd
  result.bpos = 0
  result.nread = 0

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
      let n = pcall(getdents64(dirp.fd, addr dirp.buf[0], dentBufSize))
      if n < 0:
        setErrno cint(int(-n))
        return nil
      if n == 0:
        setErrno cint(0)  # genuine end of directory
        return nil
      dirp.nread = int32(n)
      dirp.bpos = 0
    # One `struct linux_dirent64` starts at buf[bpos]:
    #   d_ino  @0 (u64), d_off @8 (s64), d_reclen @16 (u16),
    #   d_type @18 (u8), d_name @19 (NUL-terminated, variable length).
    let base = cast[uint](addr dirp.buf[0]) + uint(dirp.bpos)
    let reclen = cast[ptr uint16](base + 16'u)[]
    dirp.ent.d_type = cast[ptr uint8](base + 18'u)[]
    let namePtr = cast[ptr UncheckedArray[char]](base + 19'u)
    dirp.bpos += int32(reclen)
    var i = 0
    while i < 255 and namePtr[i] != '\0':
      dirp.ent.d_name[i] = namePtr[i]
      inc i
    dirp.ent.d_name[i] = '\0'
    return addr dirp.ent

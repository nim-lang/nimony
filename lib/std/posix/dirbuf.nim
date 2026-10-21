# Private implementation included by std/posix/posix; not a standalone module.

# A directory stream read straight from the kernel's "read directory records"
# call: `getdents64(2)` on Linux, `getdirentries(2)` on FreeBSD, `getdents(2)`
# on illumos. Every call here reports a failure as its result (-errno, see
# `pcall`), so errno is neither read nor written and the same code serves the
# libc and the freestanding builds.
#
# The including OS file provides, before including this one:
#   proc readDirRecords(fd: cint; buf: pointer; size: int; cookie: ptr Off): clong
#     the kernel call, normalized by `pcall`: bytes read, 0 at the end, -errno
# and exports, for `tposixabi`:
#   const direntReclenAt: the offset of the record length (uint16)
#   const direntTypeAt: the offset of d_type (uint8), or -1 when there is none
#   const direntNameAt: the offset of the NUL-terminated name
#   const direntSkipsZeroIno: whether a record with inode 0 is a hole to skip

const
  dirBufSize = 4096

type
  DirStream* = object
    ## A directory opened by `openDirStream`. `default(DirStream)` is not
    ## open: its `fd` of 0 is no directory of ours.
    isOpen: bool
    fd: cint
    bpos: int32        ## read cursor into `buf`
    nread: int32       ## valid bytes currently in `buf`
    cookie: Off        ## `getdirentries`' seek cookie (unused, but required)
    buf: array[dirBufSize, byte]

proc openDirStream*(d: var DirStream; path: cstring): clong {.sideEffect.} =
  ## Opens the directory `path`. Returns 0, or -errno.
  d.bpos = 0
  d.nread = 0
  let fd = pcall(open(path, O_RDONLY or O_DIRECTORY or O_CLOEXEC))
  d.isOpen = fd >= 0
  if d.isOpen:
    d.fd = cint(fd)
    result = 0
  else:
    result = fd

proc nextDirEntry*(d: var DirStream; name: var cstring; dType: var uint8): clong {.sideEffect.} =
  ## Reads the next entry: 1 with `name` and `dType` set, 0 at the end, or
  ## -errno. `name` points into `d` and is valid until the next call. `dType`
  ## is `DT_UNKNOWN` where the kernel does not report it.
  result = if d.isOpen: 0 else: -clong(EBADF)
  while d.isOpen:
    if d.bpos >= d.nread:
      let n = readDirRecords(d.fd, addr d.buf[0], dirBufSize, addr d.cookie)
      if n <= 0:
        result = n
        break
      d.nread = int32(n)
      d.bpos = 0
    let base = cast[uint](addr d.buf[0]) + uint(d.bpos)
    d.bpos += int32(cast[ptr uint16](base + uint(direntReclenAt))[])
    if not direntSkipsZeroIno or cast[ptr uint64](base)[] != 0'u64:
      when direntTypeAt < 0:
        dType = DT_UNKNOWN
      else:
        dType = cast[ptr uint8](base + uint(direntTypeAt))[]
      name = cast[cstring](base + uint(direntNameAt))
      result = 1
      break

proc closeDirStream*(d: var DirStream): clong {.sideEffect.} =
  ## Closes `d`. Returns 0, or -errno. Closing a stream that is not open
  ## does nothing.
  if d.isOpen:
    d.isOpen = false
    result = pcall(close(d.fd))
  else:
    result = 0

# Private implementation included by std/posix/posix; not a standalone module.

# macOS provides no stable raw directory syscall: the `getdirentries(2)`
# syscall returns the legacy 32-bit-inode record, while everything modern
# speaks the 64-bit-inode `struct dirent`. Rather than reimplement that, we
# call libSystem's `opendir`/`readdir`/`closedir` header-free (real symbols,
# so they link with no <dirent.h>) — libSystem is mandatory on macOS anyway —
# behind the `DirStream` interface the other systems implement on the kernel
# call (`../dirbuf`). `Dirent` mirrors the 64-bit-inode layout so
# `d_type`/`d_name` overlay the record libSystem hands back.
type
  Dirent* {.pure.} = object ## macOS `struct dirent` (64-bit inode)
    d_ino: uint64             # offset 0
    d_seekoff: uint64         # 8
    d_reclen: uint16          # 16
    d_namlen: uint16          # 18
    d_type*: uint8            # 20
    d_name*: array[1024, char] # 21

  DirStream* = object
    ## An open directory. Not open (closed, or `openDirStream` failed) when
    ## `stream` is nil.
    stream: nil pointer

# On x86_64 the PLAIN `opendir`/`readdir` symbols are the legacy
# 32-bit-inode ABI; the 64-bit-inode variants carry the `$INODE64` suffix
# (what <dirent.h> transparently rewrote to). arm64 shipped 64-bit-inode
# only, under the plain names.
when defined(amd64):
  include "amd64/bindings"
else:
  include "arm64/bindings"
proc libcClosedir(dirp: pointer): cint {.importc: "closedir", sideEffect.}

proc openDirStream*(d: var DirStream; path: cstring): clong {.sideEffect.} =
  ## Opens the directory `path`. Returns 0, or -errno.
  d.stream = libcOpendir(path)
  result = if d.stream == nil: -clong(errno()) else: 0

proc nextDirEntry*(d: var DirStream; name: var cstring; dType: var uint8): clong {.sideEffect.} =
  ## Reads the next entry: 1 with `name` and `dType` set, 0 at the end, or
  ## -errno. `name` points into libSystem's buffer and is valid until the
  ## next call.
  let stream = d.stream
  if stream == nil:
    result = -clong(EBADF)
  else:
    # `readdir` returns nil both at the end and on an error and tells them
    # apart only through errno, so errno has to start out clear.
    errnoLocation()[] = 0
    let e = libcReaddir(stream)
    if e == nil:
      result = -clong(errno())
    else:
      name = cast[cstring](addr e.d_name[0])
      dType = e.d_type
      result = 1

proc closeDirStream*(d: var DirStream): clong {.sideEffect.} =
  ## Closes `d`. Returns 0, or -errno. Closing a stream that is not open
  ## does nothing.
  let stream = d.stream
  if stream == nil:
    result = 0
  else:
    result = pcall(libcClosedir(stream))
    d.stream = nil

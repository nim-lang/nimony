# Private implementation included by std/posix/posix; not a standalone module.

# macOS provides no stable raw directory syscall: the `getdirentries(2)`
# syscall returns the legacy 32-bit-inode record, while everything modern
# speaks the 64-bit-inode `struct dirent`. Rather than reimplement that, we
# call libSystem's `opendir`/`readdir`/`closedir` header-free (real symbols,
# so they link with no <dirent.h>) — libSystem is mandatory on macOS anyway.
# `Dirent` mirrors the 64-bit-inode layout so `d_type`/`d_name` overlay
# the record libSystem hands back.
type
  Dirent* {.pure.} = object ## macOS `struct dirent` (64-bit inode)
    d_ino: uint64             # offset 0
    d_seekoff: uint64         # 8
    d_reclen: uint16          # 16
    d_namlen: uint16          # 18
    d_type*: uint8            # 20
    d_name*: array[1024, char]  # 21

  DIR* {.pure.} = object ## opaque libSystem directory stream; only ever
                         ## handled by pointer, never dereferenced here
    opaque: pointer

# On x86_64 the PLAIN `opendir`/`readdir` symbols are the legacy
# 32-bit-inode ABI; the 64-bit-inode variants carry the `$INODE64` suffix
# (what <dirent.h> transparently rewrote to). arm64 shipped 64-bit-inode
# only, under the plain names.
when defined(amd64):
  include "amd64/bindings"
else:
  include "arm64/bindings"
proc closedir*(dirp: nil ptr DIR): cint {.importc: "closedir", sideEffect.}

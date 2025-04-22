## This is a raw POSIX interface module. It does not not provide any
## convenience: cstrings are used instead of proper Nim strings and
## return codes indicate errors. If you want exceptions
## and a proper Nim-like interface, use the OS module or write a wrapper.

type
  Mode* {.importc: "mode_t", header: "<sys/types.h>".} = (
    when defined(android) or defined(macos) or defined(macosx) or
        (defined(bsd) and not defined(openbsd) and not defined(netbsd)):
      uint16
    else:
      uint32
  )

  Off* {.importc: "off_t", header: "<sys/types.h>".} = int64

  InAddrScalar* = uint32

  Sighandler = proc (a: cint) {.noconv.}

  Stat* {.importc: "struct stat",
           header: "<sys/stat.h>", final, pure.} = object ## struct stat
    st_size* {.importc: "st_size".}: Off  ## For regular files, the file size in bytes.
                                          ## For symbolic links, the length in bytes of the
                                          ## pathname contained in the symbolic link.
                                          ## For a shared memory object, the length in bytes.
                                          ## For a typed memory object, the length in bytes.
                                          ## For other file types, the use of this field is
                                          ## unspecified.

include posix_other

proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc, header: "<fcntl.h>", sideEffect.}
proc open*(a1: cstring; a2: cint; mode: Mode): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}
proc open*(a1: cstring; a2: cint): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}

proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate", header: "<unistd.h>".}
when defined(osx):              # 2001 POSIX evidently does not concern Apple
  type FStore {.importc: "fstore_t", header: "<fcntl.h>", bycopy.} = object
    fst_flags: uint32           ## IN: flags word
    fst_posmode: cint           ## IN: indicates offset field
    fst_offset,                 ## IN: start of the region
      fst_length,               ## IN: size of the region
      fst_bytesalloc: Off       ## OUT: number of bytes allocated
  var F_PEOFPOSMODE {.importc, header: "<fcntl.h>".}: cint
  var F_ALLOCATEALL {.importc, header: "<fcntl.h>".}: uint32
  var F_PREALLOCATE {.importc, header: "<fcntl.h>".}: cint
  proc posix_fallocate*(a1: cint, a2, a3: Off): cint =
    var fst = FStore(fst_flags: F_ALLOCATEALL, fst_posmode: F_PEOFPOSMODE,
                     fst_offset: a2, fst_length: a3)
    # Must also call ftruncate to match what POSIX does. Unlike posix_fallocate,
    # this can shrink files.  Could guard w/getFileSize, but caller likely knows
    # present size & has no good reason to call this unless it is growing.
    if fcntl(a1, F_PREALLOCATE, fst.addr) != cint(-1): ftruncate(a1, a2 + a3)
    else: cint(-1)
else:
  proc posix_fallocate*(a1: cint, a2, a3: Off): cint {.
    importc: "posix_fallocate", header: "<fcntl.h>".}

proc close*(a1: cint): cint {.importc: "close", header: "<unistd.h>".}

proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", header: "<sys/stat.h>", sideEffect.}

proc mmap*(a1: pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
  importc: "mmap", header: "<sys/mman.h>".}
proc munmap*(a1: pointer, a2: int): cint {.importc: "munmap", header: "<sys/mman.h>".}

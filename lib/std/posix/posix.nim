## This is a raw POSIX interface module. It does not not provide any
## convenience: cstrings are used instead of proper Nim strings and
## return codes indicate errors. If you want exceptions
## and a proper Nim-like interface, use the OS module or write a wrapper.

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(posix):
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

    Dev* {.importc: "dev_t", header: "<sys/types.h>".} = uint
    Ino* {.importc: "ino_t", header: "<sys/types.h>".} = uint

    Stat* {.importc: "struct stat",
             header: "<sys/stat.h>", final, pure.} = object ## struct stat
      st_dev* {.importc: "st_dev".} : Dev          ## Device ID of device containing file.
      st_ino* {.importc: "st_ino".} : Ino          ## File serial number.
      st_size* {.importc: "st_size".} : Off  ## For regular files, the file size in bytes.
                                            ## For symbolic links, the length in bytes of the
                                            ## pathname contained in the symbolic link.
                                            ## For a shared memory object, the length in bytes.
                                            ## For a typed memory object, the length in bytes.
                                            ## For other file types, the use of this field is
                                            ## unspecified.
      st_mode* {.importc: "st_mode".} : Mode        ## Mode of file (see below).


  const StatHasNanoseconds* = defined(linux) or defined(freebsd) or
      defined(osx) or defined(openbsd) or defined(dragonfly) or defined(haiku) ## \
    ## Boolean flag that indicates if the system supports nanosecond time
    ## resolution in the fields of `Stat`. Note that the nanosecond based fields
    ## (`Stat.st_atim`, `Stat.st_mtim` and `Stat.st_ctim`) can be accessed
    ## without checking this flag, because this module defines fallback procs
    ## when they are not available.



  include posix_other

  proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc, header: "<fcntl.h>", sideEffect.}
  proc open*(a1: cstring; a2: cint; mode: Mode): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}
  proc open*(a1: cstring; a2: cint): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}

  proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate", header: "<unistd.h>".}
  when defined(osx):              # 2001 POSIX evidently does not concern Apple
    type FStore {.importc: "fstore_t", header: "<fcntl.h>", bycopy.} = object
      fst_flags {.importc.}: uint32     ## IN: flags word
      fst_posmode {.importc.}: cint     ## IN: indicates offset field
      fst_offset {.importc.}: Off       ## IN: start of the region
      fst_length {.importc.}: Off       ## IN: size of the region
      fst_bytesalloc {.importc.}: Off   ## OUT: number of bytes allocated
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
  proc lstat*(a1: cstring, a2: var Stat): cint {.importc, header: "<sys/stat.h>", sideEffect.}
  proc stat*(a1: cstring, a2: var Stat): cint {.importc, header: "<sys/stat.h>".}


  proc S_ISBLK*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a block special file.
  proc S_ISCHR*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a character special file.
  proc S_ISDIR*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a directory.
  proc S_ISFIFO*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a pipe or FIFO special file.
  proc S_ISREG*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a regular file.
  proc S_ISLNK*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a symbolic link.
  proc S_ISSOCK*(m: Mode): bool {.importc, header: "<sys/stat.h>".}
    ## Test for a socket.

  proc mmap*(a1: pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
    importc: "mmap", header: "<sys/mman.h>".}
  proc munmap*(a1: pointer, a2: int): cint {.importc: "munmap", header: "<sys/mman.h>".}

  proc clock_gettime*(a1: ClockId, a2: var Timespec): cint {.
    importc, header: "<time.h>", sideEffect.}

  proc getcwd*(a1: cstring, a2: int): cstring {.importc, header: "<unistd.h>", sideEffect.}
  proc chdir*(path: cstring): cint {.importc, header: "<unistd.h>", sideEffect.}

  when not defined(nintendoswitch):
    proc readlink*(a1, a2: cstring, a3: int): int {.importc, header: "<unistd.h>".}

    proc symlink*(a1, a2: cstring): cint {.importc, header: "<unistd.h>".}
  else:
    proc symlink*(a1, a2: cstring): cint = -1

  # Directory operations
  type
    DIR* {.importc: "DIR", header: "<dirent.h>", incompleteStruct.} = object
    Dirent* {.importc: "struct dirent", header: "<dirent.h>".} = object
      d_type* {.importc: "d_type".}: uint8
      d_name* {.importc: "d_name".}: array[256, char]

  proc opendir*(name: cstring): ptr DIR {.importc, header: "<dirent.h>", sideEffect.}
  proc closedir*(dirp: ptr DIR): cint {.importc, header: "<dirent.h>", sideEffect.}
  proc readdir*(dirp: ptr DIR): ptr Dirent {.importc, header: "<dirent.h>", sideEffect.}

  proc mkdir*(path: cstring, mode: Mode): cint {.importc, header: "<sys/stat.h>", sideEffect.}
  proc rmdir*(path: cstring): cint {.importc, header: "<unistd.h>", sideEffect.}
  proc unlink*(path: cstring): cint {.importc, header: "<unistd.h>", sideEffect.}

  # POSIX d_type constants
  const
    DT_UNKNOWN* = 0'u8 ## Unknown file type.
    DT_FIFO* = 1'u8    ## Named pipe, or FIFO.
    DT_CHR* = 2'u8     ## Character device.
    DT_DIR* = 4'u8     ## Directory.
    DT_BLK* = 6'u8     ## Block device.
    DT_REG* = 8'u8     ## Regular file.
    DT_LNK* = 10'u8    ## Symbolic link.
    DT_SOCK* = 12'u8   ## UNIX domain socket.
    DT_WHT* = 14'u8


  proc sysconf*(a1: cint): int {.importc, header: "<unistd.h>".}

  # <sys/wait.h>
  proc WEXITSTATUS*(s: cint): cint =  (s and 0xff00) shr 8
  proc WTERMSIG*(s:cint): cint = s and 0x7f
  proc WSTOPSIG*(s:cint): cint = WEXITSTATUS(s)
  proc WIFEXITED*(s:cint) : bool = WTERMSIG(s) == 0
  proc WIFSIGNALED*(s:cint) : bool = (cast[int8]((s and 0x7f) + 1) shr 1) > 0
  proc WIFSTOPPED*(s:cint) : bool = (s and 0xff) == 0x7f
  proc WIFCONTINUED*(s:cint) : bool = s == WCONTINUED

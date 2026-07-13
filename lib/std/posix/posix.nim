## This is a raw POSIX interface module. It does not not provide any
## convenience: cstrings are used instead of proper Nim strings and
## return codes indicate errors. If you want exceptions
## and a proper Nim-like interface, use the OS module or write a wrapper.

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(posix):
  # Included first so `Time`/`Timespec` (and the posix consts) are defined
  # before `Stat` references `Timespec` — nimony does not resolve that forward
  # reference across the conditional `type` sections below.
  include posix_other

  type
    InAddrScalar* = uint32
    Sighandler = proc (a: cint) {.noconv.}
    FileHandle* = cint
    SocketHandle* = cint

  when defined(nimNativeIo) and defined(amd64):
    # Hardcoded Linux/amd64 ABI so the freestanding/native build needs no
    # <sys/types.h>/<sys/stat.h>. `struct stat` is 144 bytes; only the fields
    # other modules actually read are exposed — the rest are correctly-sized
    # private padding so `fstat` writes land at the right offsets.
    type
      Mode* = uint32   ## mode_t
      Off* = int64     ## off_t
      Dev* = uint      ## dev_t
      Ino* = uint      ## ino_t

      Stat* {.pure.} = object ## Linux/amd64 `struct stat`
        st_dev*: Dev              # offset 0
        st_ino*: Ino              # 8
        st_nlink: uint            # 16
        st_mode*: Mode            # 24
        st_uid: uint32            # 28
        st_gid: uint32            # 32
        pad0: int32               # 36
        st_rdev: Dev              # 40
        st_size*: Off             # 48
        st_blksize: int64         # 56
        st_blocks: int64          # 64
        st_atim: Timespec         # 72
        st_mtim*: Timespec        # 88
        st_ctim: Timespec         # 104
        glibcReserved: array[3, int64]  # 120 .. 143
  elif defined(nimNativeIo) and defined(osx):
    # Hardcoded macOS (`__DARWIN_64_BIT_INO_T`) `struct stat` so the
    # freestanding build needs no <sys/types.h>/<sys/stat.h>. The layout is the
    # same on arm64 and x86_64 (both use the 64-bit-inode ABI); `struct stat`
    # is 144 bytes. Only the fields other modules read are exposed by name —
    # the rest are correctly-sized private padding so `fstat` writes land at the
    # right offsets.
    type
      Mode* = uint16   ## mode_t
      Off* = int64     ## off_t
      Dev* = int32     ## dev_t
      Ino* = uint64    ## ino_t

      Stat* {.pure.} = object ## macOS `struct stat`
        st_dev*: Dev              # offset 0
        st_mode*: Mode            # 4
        st_nlink: uint16          # 6
        st_ino*: Ino              # 8
        st_uid: uint32            # 16
        st_gid: uint32            # 20
        st_rdev: Dev              # 24
        pad0: int32               # 28 (pad to 8-align the timespecs)
        st_atim: Timespec         # 32
        st_mtim*: Timespec        # 48
        st_ctim: Timespec         # 64
        st_birthtim: Timespec     # 80
        st_size*: Off             # 96
        st_blocks: int64          # 104
        st_blksize: int32         # 112
        st_flags: uint32          # 116
        st_gen: uint32            # 120
        st_lspare: int32          # 124
        st_qspare: array[2, int64]  # 128 .. 143
  else:
    type
      Mode* {.importc: "mode_t", header: "<sys/types.h>".} = (
        when defined(android) or defined(macos) or defined(macosx) or
            (defined(bsd) and not defined(openbsd) and not defined(netbsd)):
          uint16
        else:
          uint32
      )

      Off* {.importc: "off_t", header: "<sys/types.h>".} = int64

      Dev* {.importc: "dev_t", header: "<sys/types.h>".} = uint
      Ino* {.importc: "ino_t", header: "<sys/types.h>".} = uint

      Stat* {.importc: "struct stat",
               header: "<sys/stat.h>", final, pure.} = object ## struct stat
        st_dev* {.importc: "st_dev".}: Dev          ## Device ID of device containing file.
        st_ino* {.importc: "st_ino".}: Ino          ## File serial number.
        st_size* {.importc: "st_size".}: Off  ## For regular files, the file size in bytes.
                                              ## For symbolic links, the length in bytes of the
                                              ## pathname contained in the symbolic link.
                                              ## For a shared memory object, the length in bytes.
                                              ## For a typed memory object, the length in bytes.
                                              ## For other file types, the use of this field is
                                              ## unspecified.
        st_mode* {.importc: "st_mode".}: Mode        ## Mode of file (see below).
        when defined(osx):
          # On macOS `st_mtime` is a macro that expands to `st_mtimespec.tv_sec`,
          # so declaring it as a separate field would alias `st_mtim` and trigger
          # a `-Winitializer-overrides` warning whenever a `Stat` value is zeroed.
          st_mtim* {.importc: "st_mtimespec".}: Timespec ## Time of last data modification with nanosecond precision.
        else:
          st_mtime* {.importc: "st_mtime".}: int64     ## Time of last data modification (seconds since epoch).
          st_mtim* {.importc: "st_mtim".}: Timespec      ## Time of last data modification with nanosecond precision.


  const StatHasNanoseconds* = defined(linux) or defined(freebsd) or
      defined(osx) or defined(openbsd) or defined(dragonfly) or defined(haiku) ## \
    ## Boolean flag that indicates if the system supports nanosecond time
    ## resolution in the fields of `Stat`. Note that the nanosecond based fields
    ## (`Stat.st_atim`, `Stat.st_mtim` and `Stat.st_ctim`) can be accessed
    ## without checking this flag, because this module defines fallback procs
    ## when they are not available.



  when defined(osx) or (defined(nimNativeIo) and defined(amd64)):
    template st_mtime*(s: Stat): int64 = int64(s.st_mtim.tv_sec)
      ## Time of last data modification (seconds since epoch).

  when defined(nimNativeIo):
    proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc: "fcntl", sideEffect.}
  else:
    proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc, header: "<fcntl.h>", sideEffect.}
  # Under -d:nimNativeIo these syscalls are declared header-less (bare names),
  # so the C backend links the thin wrapper and arkham lowers them to bare
  # `syscall` instructions — no <fcntl.h>/<unistd.h>/<sys/stat.h>/<sys/mman.h>.
  when defined(nimNativeIo):
    # A single FIXED-ARITY decl with a defaulted `mode`. A `varargs` form would be
    # monomorphized by sem into separate 2- and 3-argument variants that both
    # `importc "open"` — and `nimony n` lowers each importc syscall to ONE register-
    # signature stub keyed by the C name, so two different arities would collapse to
    # a single stub and the shorter call would leave a declared arg unbound. With a
    # default, `open(fc, flags)` and `open(fc, flags, mode)` are the SAME 3-arg call
    # (the kernel ignores `mode` unless O_CREAT is set in `a2`).
    proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
  else:
    proc open*(a1: cstring; a2: cint; mode: Mode): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}
    proc open*(a1: cstring; a2: cint): cint {.importc: "open", header: "<fcntl.h>", sideEffect.}

  when defined(nimNativeIo):
    proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
  else:
    proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate", header: "<unistd.h>".}
  when defined(nimNativeIo):
    discard # no posix_fallocate (a libc func, not a syscall); memfiles etc.
            # fall back to a plain ftruncate via `when declared(posix_fallocate)`.
  elif defined(osx):              # 2001 POSIX evidently does not concern Apple
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

  when defined(nimNativeIo):
    proc close*(a1: cint): cint {.importc: "close".}
  else:
    proc close*(a1: cint): cint {.importc: "close", header: "<unistd.h>".}

  when defined(nimNativeIo):
    proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
    proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat", sideEffect.}
    proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat".}
  else:
    proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", header: "<sys/stat.h>", sideEffect.}
    proc lstat*(a1: cstring, a2: var Stat): cint {.importc, header: "<sys/stat.h>", sideEffect.}
    proc stat*(a1: cstring, a2: var Stat): cint {.importc, header: "<sys/stat.h>".}


  when defined(nimNativeIo):
    # The libc `S_IS*` are header macros, so under -d:nimNativeIo we reimplement
    # them natively from the file-type bits (`st_mode and S_IFMT`). The S_IF*
    # values are standard POSIX and identical across Linux/amd64+arm64.
    template fileType(m: Mode): uint32 = uint32(m) and 0o170000'u32 # S_IFMT
    proc S_ISBLK*(m: Mode): bool = fileType(m) == 0o060000'u32  ## block special
    proc S_ISCHR*(m: Mode): bool = fileType(m) == 0o020000'u32  ## char special
    proc S_ISDIR*(m: Mode): bool = fileType(m) == 0o040000'u32  ## directory
    proc S_ISFIFO*(m: Mode): bool = fileType(m) == 0o010000'u32 ## FIFO/pipe
    proc S_ISREG*(m: Mode): bool = fileType(m) == 0o100000'u32  ## regular file
    proc S_ISLNK*(m: Mode): bool = fileType(m) == 0o120000'u32  ## symlink
    proc S_ISSOCK*(m: Mode): bool = fileType(m) == 0o140000'u32 ## socket
  else:
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

  when defined(nimNativeIo):
    proc mmap*(a1: nil pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
      importc: "mmap".}
    proc munmap*(a1: nil pointer, a2: int): cint {.importc: "munmap".}
  else:
    proc mmap*(a1: nil pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
      importc: "mmap", header: "<sys/mman.h>".}
    proc munmap*(a1: nil pointer, a2: int): cint {.importc: "munmap", header: "<sys/mman.h>".}

  when not defined(nimNativeIo):
    var errnoVar {.importc: "errno", header: "<errno.h>".}: cint
  else:
    var errno*: cint = 0
      ## Native errno maintained by this module's freestanding syscall wrappers
      ## (currently the directory ops). Mirrors libc's `errno` so consumers like
      ## `std/dirs` can keep reporting errors via `posixToErrorCode(errno)`
      ## without pulling `<errno.h>`. NOTE: on the C-backend native build the
      ## bare-importc syscalls below still set libc's `errno`, not this one, so
      ## error codes are only fully accurate on the raw-syscall (arkham) target
      ## — happy paths work in both (see `pcall`'s caveat).

  template pcall*(x: untyped): clong {.untyped.} =
    ## Normalizes a syscall-style call to the Linux raw convention: returns the
    ## non-negative result on success, or `-errno` on failure. Hides whether the
    ## error is signalled by the raw syscall's negative return (freestanding /
    ## arkham, `-d:nimNativeIo`) or by libc's `-1` + the `errno` global.
    when defined(nimNativeIo):
      clong(x)
    else:
      let r = clong(x)
      if r < 0: clong(-errnoVar) else: r

  template mmapFailed*(p: pointer): bool =
    ## True if an `mmap` result indicates failure. The kernel signals failure as
    ## an address in `[-4095, -1]`; libc maps that to `MAP_FAILED` (`(void*)-1`,
    ## itself in range), so one range check covers both conventions.
    cast[int](p) >= -4095 and cast[int](p) <= -1

  template mmapErrno*(p: pointer): cint =
    ## `errno` for a failed `mmap` (see `mmapFailed`).
    when defined(nimNativeIo): cint(-cast[int](p))
    else: errnoVar

  when defined(nimNativeIo):
    proc clock_gettime*(a1: ClockId, a2: var Timespec): cint {.importc: "clock_gettime", sideEffect.}
  else:
    proc clock_gettime*(a1: ClockId, a2: var Timespec): cint {.
      importc, header: "<time.h>", sideEffect.}

  when defined(nimNativeIo):
    proc getcwd*(a1: cstring, a2: int): cstring {.importc: "getcwd", sideEffect.}
    proc chdir*(path: cstring): cint {.importc: "chdir", sideEffect.}
  else:
    proc getcwd*(a1: cstring, a2: int): cstring {.importc, header: "<unistd.h>", sideEffect.}
    proc chdir*(path: cstring): cint {.importc, header: "<unistd.h>", sideEffect.}

  when defined(nimNativeIo):
    proc realpath*(path, resolved: cstring): cstring {.importc: "realpath", sideEffect.}
  else:
    proc realpath*(path, resolved: cstring): cstring {.importc, header: "<stdlib.h>", sideEffect.}

  when not defined(nintendoswitch):
    when defined(nimNativeIo):
      proc readlink*(a1, a2: cstring, a3: int): int {.importc: "readlink".}
      proc symlink*(a1, a2: cstring): cint {.importc: "symlink".}
    else:
      proc readlink*(a1, a2: cstring, a3: int): int {.importc, header: "<unistd.h>".}
      proc symlink*(a1, a2: cstring): cint {.importc, header: "<unistd.h>".}
  else:
    proc symlink*(a1, a2: cstring): cint = -1

  # Directory operations
  when defined(nimNativeIo) and defined(linux):
    # `opendir`/`readdir`/`closedir` are libc functions (`DIR` is an opaque libc
    # buffer), not syscalls, so under -d:nimNativeIo we reimplement them on top
    # of open(2) + getdents64(2) + close(2). `Dirent` keeps the same two fields
    # (`d_type`, `d_name`) the consumers read, but with a native layout — its
    # bytes are copied out of the raw `struct linux_dirent64` records.
    const
      O_DIRECTORY = cint(0o200000)  ## asm-generic O_DIRECTORY (shared amd64/arm64)
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

    # Linux `getdents64`; not a libc-portable name historically, so declared as
    # a bare syscall wrapper (links to glibc's getdents64 on the C backend).
    proc getdents64(fd: cint; dirp: pointer; count: int): clong {.importc: "getdents64", sideEffect.}

    proc opendir*(name: cstring): nil ptr DIR {.sideEffect.} =
      let fd = open(name, O_RDONLY or O_DIRECTORY or O_CLOEXEC)
      if fd < 0:
        errno = cint(-fd)
        return nil
      result = cast[ptr DIR](alloc0(sizeof(DIR)))
      result.fd = fd
      result.bpos = 0
      result.nread = 0

    proc closedir*(dirp: nil ptr DIR): cint {.sideEffect.} =
      if dirp == nil:
        errno = cint(9)  # EBADF
        return cint(-1)
      let fd = dirp.fd
      dealloc(dirp)
      result = close(fd)

    proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.sideEffect.} =
      if dirp == nil:
        errno = cint(9)  # EBADF
        return nil
      while true:
        if dirp.bpos >= dirp.nread:
          let n = getdents64(dirp.fd, addr dirp.buf[0], dentBufSize)
          if n < 0:
            errno = cint(int(-n))
            return nil
          if n == 0:
            errno = cint(0)  # genuine end of directory
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
  elif defined(nimNativeIo) and defined(osx):
    # macOS provides no stable raw directory syscall: the `getdirentries(2)`
    # syscall returns the legacy 32-bit-inode record, while everything modern
    # speaks the 64-bit-inode `struct dirent`. Rather than reimplement that, we
    # call libSystem's `opendir`/`readdir`/`closedir` header-free (real symbols,
    # so they link with no <dirent.h>) — consistent with how this module already
    # binds `open`/`close`/`stat` on macOS, where libSystem is mandatory anyway.
    # `Dirent` mirrors the arm64 64-bit-inode layout so `d_type`/`d_name` overlay
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

    proc opendir*(name: cstring): nil ptr DIR {.importc: "opendir", sideEffect.}
    proc closedir*(dirp: nil ptr DIR): cint {.importc: "closedir", sideEffect.}
    proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc: "readdir", sideEffect.}
  else:
    type
      DIR* {.importc: "DIR", header: "<dirent.h>", incompleteStruct.} = object
      Dirent* {.importc: "struct dirent", header: "<dirent.h>".} = object
        d_type* {.importc: "d_type".}: uint8
        d_name* {.importc: "d_name".}: array[256, char]

    proc opendir*(name: cstring): nil ptr DIR {.importc, header: "<dirent.h>", sideEffect.}
    proc closedir*(dirp: nil ptr DIR): cint {.importc, header: "<dirent.h>", sideEffect.}
    proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc, header: "<dirent.h>", sideEffect.}

  when defined(nimNativeIo):
    # `mkdir`'s <sys/stat.h> would re-declare `stat`/`lstat` (whose native
    # bare-importc prototypes use our hardcoded `struct stat`) → conflicting
    # types. All three are syscalls, so strip the headers.
    proc mkdir*(path: cstring, mode: Mode): cint {.importc: "mkdir", sideEffect.}
    proc rmdir*(path: cstring): cint {.importc: "rmdir", sideEffect.}
    proc unlink*(path: cstring): cint {.importc: "unlink", sideEffect.}
  else:
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


  when defined(nimNativeIo):
    proc sysconf*(a1: cint): int {.importc: "sysconf".}
  else:
    proc sysconf*(a1: cint): int {.importc, header: "<unistd.h>".}

  # <sys/wait.h>
  proc WEXITSTATUS*(s: cint): cint =  (s and 0xff00) shr 8
  proc WTERMSIG*(s: cint): cint = s and 0x7f
  proc WSTOPSIG*(s: cint): cint = WEXITSTATUS(s)
  proc WIFEXITED*(s: cint): bool = WTERMSIG(s) == 0
  proc WIFSIGNALED*(s: cint): bool = (cast[int8]((s and 0x7f) + 1) shr 1) > 0
  proc WIFSTOPPED*(s: cint): bool = (s and 0xff) == 0x7f
  proc WIFCONTINUED*(s: cint): bool = s == WCONTINUED

  # -------- Process / pipe / exec bindings needed by std/osproc --------
  # The syscall-based subset (pipe/dup2/fork/exec/waitpid/kill/...) is provided
  # in BOTH modes; under -d:nimNativeIo it is header-stripped so the
  # freestanding posix.c pulls no libc headers, and native osproc drives a
  # fork+exec path. The posix_spawn / sigset_t family are libc-only opaque
  # types (they would pull <spawn.h>/<signal.h> → transitive <unistd.h>), so
  # they stay behind `when not defined(nimNativeIo)`.
  when defined(nimNativeIo):
    type Pid* = cint
  else:
    type Pid* {.importc: "pid_t", header: "<sys/types.h>".} = cint

  # Use plain C `char` so that `char**` lines up with libc's expectation
  # (Nimony's `cstring` is `NC8*` / unsigned char*, which triggers
  # `-Wincompatible-pointer-types` on posix_spawn / execvp / execve).
  type CChar* {.importc: "char", nodecl.} = int8
  type CCharArray* = nil ptr UncheckedArray[nil ptr CChar]

  when defined(nimNativeIo):
    proc pipe*(a: ptr cint): cint {.importc: "pipe", sideEffect.}
    proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
    proc fork*(): Pid {.importc: "fork", sideEffect.}
    proc execve*(path: cstring; argv, env: CCharArray): cint {.importc: "execve", sideEffect.}
    # `execvp` is defined below as a native wrapper (it is NOT a syscall — it is
    # libc's PATH-resolving layer over `execve`); see after `posix_environ`.
    # There is no `waitpid` Linux syscall — it is libc sugar for `wait4` with a NULL
    # `rusage`. arkham lowers bare syscall names, so on the native backend we bind to
    # `wait4` directly and pass `rusage = nil` ourselves, keeping the 3-arg `waitpid`
    # signature the rest of the code (and the libc path) expects.
    # No `<sys/wait.h>` header (strip it like mkdir/rmdir/unlink above): on Linux it
    # transitively pulls `<unistd.h>`, whose `execve(const char*, char* const*, ...)`
    # prototype conflicts with our bare `execve` binding. The status decoders and
    # `WNOHANG`/`WCONTINUED` are all self-defined, so the header buys us nothing.
    proc wait4(pid: Pid; status: var cint; options: cint;
               rusage: nil pointer): Pid {.importc: "wait4", sideEffect.}
    proc waitpid*(pid: Pid; status: var cint; options: cint): Pid {.inline.} =
      wait4(pid, status, options, nil)
    proc kill*(pid: Pid; sig: cint): cint {.importc: "kill", sideEffect.}
    proc setpgid*(pid, pgid: Pid): cint {.importc: "setpgid", sideEffect.}
    proc exitnow*(status: cint) {.importc: "_exit", noreturn.}
    proc read*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "read", sideEffect.}
    proc write*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "write", sideEffect.}
    # The libc-free runtime has no `environ`; the generated `main` captures the
    # kernel-provided env block into `nimEnviron` (see hexer genMainProc). Reading
    # it makes `execShellCmd`/`execProcess` pass the real parent environment.
    var posix_environ* {.importc: "nimEnviron".}: ptr UncheckedArray[cstring]

    proc execvp*(file: cstring; argv: CCharArray): cint {.sideEffect.} =
      ## Libc-free `execvp`. There is no `execvp` syscall — it is libc's PATH-
      ## resolving wrapper around `execve`. If `file` contains a '/', exec it
      ## directly; otherwise try `file` in each ':'-separated directory of `$PATH`
      ## (default `/bin:/usr/bin`), reading the environment from `nimEnviron`.
      ## Runs in the forked child of `osproc.startProcess`; on success it never
      ## returns, so the allocations here are reached only on the error path.
      let envp = cast[CCharArray](posix_environ)
      var hasSlash = false
      var n = 0
      while file[n] != '\0':
        if file[n] == '/': hasSlash = true
        inc n
      if hasSlash:
        return execve(file, argv, envp)
      # Locate PATH in the environment block (raw cstring scan, no allocation).
      var path = "/bin:/usr/bin"
      if posix_environ != nil:
        var i = 0
        while posix_environ[i] != nil:
          let e = posix_environ[i]
          if e[0] == 'P' and e[1] == 'A' and e[2] == 'T' and e[3] == 'H' and
             e[4] == '=':
            path = ""
            var k = 5
            while e[k] != '\0':
              path.add e[k]
              inc k
            break
          inc i
      var fileStr = ""
      var m = 0
      while file[m] != '\0':
        fileStr.add file[m]
        inc m
      var start = 0
      var i = 0
      while i <= path.len:
        if i == path.len or path[i] == ':':
          let dir = if i > start: path[start ..< i] else: "."
          var candidate = dir & "/" & fileStr
          discard execve(candidate.toCString, argv, envp)   # returns only on failure
          start = i + 1
        inc i
      result = -1'i32
  else:
    proc pipe*(a: ptr cint): cint {.
      importc, header: "<unistd.h>", sideEffect.}
    proc dup2*(oldfd, newfd: cint): cint {.
      importc, header: "<unistd.h>", sideEffect.}
    proc fork*(): Pid {.importc, header: "<unistd.h>", sideEffect.}
    proc execvp*(file: cstring; argv: CCharArray): cint {.
      importc, header: "<unistd.h>", sideEffect.}
    proc execve*(path: cstring; argv, env: CCharArray): cint {.
      importc, header: "<unistd.h>", sideEffect.}
    proc waitpid*(pid: Pid; status: var cint; options: cint): Pid {.
      importc, header: "<sys/wait.h>", sideEffect.}
    proc kill*(pid: Pid; sig: cint): cint {.
      importc, header: "<signal.h>", sideEffect.}
    proc setpgid*(pid, pgid: Pid): cint {.
      importc, header: "<unistd.h>", sideEffect.}
    proc exitnow*(status: cint) {.
      importc: "_exit", header: "<unistd.h>", noreturn.}
    proc read*(fildes: cint; buf: pointer; nbyte: int): int {.
      importc, header: "<unistd.h>", sideEffect.}
    proc write*(fildes: cint; buf: pointer; nbyte: int): int {.
      importc, header: "<unistd.h>", sideEffect.}

    # --- posix_spawn / signal-set family (libc-only; native osproc uses fork+exec) ---
    when defined(osx):
      # On macOS these are typedef'd to scalar types (`__uint32_t` for
      # `sigset_t`, `void *` for the spawn handles); declaring them as
      # opaque `object` makes nimony emit `(T){}` compound literals which
      # clang rejects for scalar types. The pointer typedefs are nullable
      # at the C level, so map to `nil pointer` (which has a `default()`
      # overload).
      type
        Sigset* {.importc: "sigset_t", header: "<signal.h>".} = uint32
        Tposix_spawnattr* {.importc: "posix_spawnattr_t",
            header: "<spawn.h>".} = nil pointer
        Tposix_spawn_file_actions* {.importc: "posix_spawn_file_actions_t",
            header: "<spawn.h>".} = nil pointer
    else:
      type
        Sigset* {.importc: "sigset_t", header: "<signal.h>", final, pure.} = object
        Tposix_spawnattr* {.importc: "posix_spawnattr_t",
            header: "<spawn.h>", final, pure.} = object
        Tposix_spawn_file_actions* {.importc: "posix_spawn_file_actions_t",
            header: "<spawn.h>", final, pure.} = object

    # posix_spawn
    proc posix_spawn*(pid: var Pid; path: cstring;
                      file_actions: var Tposix_spawn_file_actions;
                      attrp: var Tposix_spawnattr;
                      argv, envp: CCharArray): cint {.
      importc, header: "<spawn.h>", sideEffect.}
    proc posix_spawnp*(pid: var Pid; file: cstring;
                       file_actions: var Tposix_spawn_file_actions;
                       attrp: var Tposix_spawnattr;
                       argv, envp: CCharArray): cint {.
      importc, header: "<spawn.h>", sideEffect.}
    proc posix_spawn_file_actions_init*(
        fops: var Tposix_spawn_file_actions): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawn_file_actions_destroy*(
        fops: var Tposix_spawn_file_actions): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawn_file_actions_addclose*(
        fops: var Tposix_spawn_file_actions; fildes: cint): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawn_file_actions_adddup2*(
        fops: var Tposix_spawn_file_actions; fildes, newfildes: cint): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawn_file_actions_addchdir_np*(
        fops: var Tposix_spawn_file_actions; path: cstring): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_init*(attr: var Tposix_spawnattr): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_destroy*(attr: var Tposix_spawnattr): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_setflags*(attr: var Tposix_spawnattr;
                                   flags: cshort): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_setpgroup*(attr: var Tposix_spawnattr;
                                    pgroup: Pid): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_setsigmask*(attr: var Tposix_spawnattr;
                                     mask: var Sigset): cint {.
      importc, header: "<spawn.h>".}
    proc posix_spawnattr_setsigdefault*(attr: var Tposix_spawnattr;
                                        mask: var Sigset): cint {.
      importc, header: "<spawn.h>".}
    proc sigemptyset*(mask: var Sigset): cint {.
      importc, header: "<signal.h>".}
    proc sigfillset*(mask: var Sigset): cint {.
      importc, header: "<signal.h>".}
    proc sigaddset*(mask: var Sigset; sig: cint): cint {.
      importc, header: "<signal.h>".}

    # environ
    var posix_environ* {.importc: "environ", header: "<unistd.h>".}:
      ptr UncheckedArray[cstring]

  # errno
  when defined(nimNativeIo):
    proc strerror*(errnum: cint): cstring {.importc: "strerror", sideEffect.}
  else:
    proc strerror*(errnum: cint): cstring {.
      importc, header: "<string.h>", sideEffect.}

  when defined(nimNativeIo):
    proc nanosleep*(req: var Timespec; rem: var Timespec): cint {.importc: "nanosleep", sideEffect.}
  else:
    proc nanosleep*(req: var Timespec; rem: var Timespec): cint {.
      importc, header: "<time.h>", sideEffect.}

## This is a raw POSIX interface module. It does not not provide any
## convenience: cstrings are used instead of proper Nim strings and
## return codes indicate errors. If you want exceptions
## and a proper Nim-like interface, use the OS module or write a wrapper.
##
## Every binding is header-free: types and constants are ABI transcriptions
## (see posix_other.nim) and procs are bare `importc` of real libc/kernel
## symbols. `-d:useLibc` changes which allocator and stdio implementation the
## stdlib uses, never which declarations exist.

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(posix):
  # Included first so `Time`/`Timespec` (and the posix consts) are defined
  # before `Stat` references `Timespec` — nimony does not resolve that forward
  # reference across the conditional `type` sections below.
  include posix_other

  type
    InAddrScalar* = uint32
    Sighandler* = proc (a: cint) {.noconv.}
    FileHandle* = cint
    SocketHandle* = cint
    Pid* = cint      ## pid_t

  when defined(linux) and defined(amd64):
    # Hardcoded Linux/amd64 ABI. `struct stat` is 144 bytes; only the fields
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
  elif defined(linux) and defined(arm64):
    # Hardcoded Linux/arm64 ABI: the kernel's asm-generic `struct stat`
    # (include/uapi/asm-generic/stat.h), 128 bytes, also used by riscv64 and
    # loongarch64. Note the field ORDER differs from amd64 (mode before nlink,
    # both 32-bit) and the trailing padding is two u32s.
    type
      Mode* = uint32   ## mode_t
      Off* = int64     ## off_t
      Dev* = uint      ## dev_t
      Ino* = uint      ## ino_t

      Stat* {.pure.} = object ## Linux/arm64 `struct stat` (asm-generic)
        st_dev*: Dev              # offset 0
        st_ino*: Ino              # 8
        st_mode*: Mode            # 16
        st_nlink: uint32          # 20
        st_uid: uint32            # 24
        st_gid: uint32            # 28
        st_rdev: Dev              # 32
        pad1: uint64              # 40
        st_size*: Off             # 48
        st_blksize: int32         # 56
        pad2: int32               # 60
        st_blocks: int64          # 64
        st_atim: Timespec         # 72
        st_mtim*: Timespec        # 88
        st_ctim: Timespec         # 104
        unused4: uint32           # 120
        unused5: uint32           # 124
  elif defined(linux) and defined(i386):
    # Hardcoded Linux/i386 ABI: glibc's `struct stat64` (96 bytes), so `Off`
    # stays 64-bit like every other target. The stat procs below bind the
    # `stat64`/`lstat64`/`fstat64` symbols on this architecture to match.
    # i386 aligns 64-bit integers to 4 bytes, which is why st_size can sit at
    # offset 44; nimony's C backend inherits that rule from the C compiler.
    type
      Mode* = uint32   ## mode_t
      Off* = int64     ## off_t
      Dev* = uint64    ## dev_t (64-bit even on i386 in the LFS layout)
      Ino* = uint64    ## ino64_t

      Stat* {.pure.} = object ## Linux/i386 `struct stat64`
        st_dev*: Dev              # offset 0
        pad0: uint32              # 8
        st_ino32: uint32          # 12 (truncated inode; real one at the tail)
        st_mode*: Mode            # 16
        st_nlink: uint32          # 20
        st_uid: uint32            # 24
        st_gid: uint32            # 28
        st_rdev: Dev              # 32
        pad3: uint32              # 40
        st_size*: Off             # 44
        st_blksize: int32         # 52
        st_blocks: int64          # 56
        st_atim: Timespec         # 64
        st_mtim*: Timespec        # 72
        st_ctim: Timespec         # 80
        st_ino*: Ino              # 88
  elif defined(osx):
    # Hardcoded macOS (`__DARWIN_64_BIT_INO_T`) `struct stat`. The layout is
    # the same on arm64 and x86_64 (both use the 64-bit-inode ABI);
    # `struct stat` is 144 bytes. Only the fields other modules read are
    # exposed by name — the rest are correctly-sized private padding so
    # `fstat` writes land at the right offsets.
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

  const StatHasNanoseconds* = true ## \
    ## Boolean flag that indicates if the system supports nanosecond time
    ## resolution in the fields of `Stat`. All supported targets do.

  template st_mtime*(s: Stat): int64 = int64(s.st_mtim.tv_sec)
    ## Time of last data modification (seconds since epoch). All hardcoded
    ## layouts store the full `st_mtim` Timespec; this accessor mirrors the
    ## POSIX `st_mtime` field/macro.

  # GCC treats POSIX functions like `execve` as builtins with a known
  # prototype (`int execve(const char*, char* const*, char* const*)`) and emits
  # `-Wbuiltin-declaration-mismatch` against our header-free bindings, whose
  # only difference is pointer qualifiers (`char**` vs `char* const*`) —
  # identical at the ABI level. Nothing in the C standard turns a POSIX
  # declaration into a builtin, so this is GCC overreach; silence it here
  # rather than smuggle `const`-qualified casts through every call site
  # (nim-lang/nimony#2148). Guarded to GCC: clang has no such warning and
  # would instead spam `-Wunknown-warning-option`.
  when defined(gcc):
    {.passC: "-Wno-builtin-declaration-mismatch".}

  proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc: "fcntl", sideEffect.}

  # A single FIXED-ARITY `open` with a defaulted `mode`. A `varargs` form would
  # be monomorphized by sem into separate 2- and 3-argument variants that both
  # `importc "open"` — and `nimony n` lowers each importc syscall to ONE
  # register-signature stub keyed by the C name, so two different arities would
  # collapse to a single stub and the shorter call would leave a declared arg
  # unbound. With a default, `open(fc, flags)` and `open(fc, flags, mode)` are
  # the SAME 3-arg call (the kernel ignores `mode` unless O_CREAT is set).
  #
  # On i386 every symbol that touches a 64-bit `Off` binds the glibc LFS
  # variant (`open64`/`stat64`/...): the plain names there speak the legacy
  # 32-bit-off_t ABI, and our `Off` is int64 on every architecture.
  when defined(linux) and defined(i386):
    proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open64", sideEffect.}
    proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate64".}
    proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat64", sideEffect.}
    proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat64", sideEffect.}
    proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat64".}
  else:
    proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
    proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
    proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
    proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat", sideEffect.}
    proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat".}

  proc close*(a1: cint): cint {.importc: "close".}

  # The libc `S_IS*` are header macros, so they are reimplemented natively
  # from the file-type bits (`st_mode and S_IFMT`). The S_IF* values are
  # standard POSIX and identical on every supported target.
  template fileType(m: Mode): uint32 = uint32(m) and 0o170000'u32 # S_IFMT
  proc S_ISBLK*(m: Mode): bool = fileType(m) == 0o060000'u32  ## block special
  proc S_ISCHR*(m: Mode): bool = fileType(m) == 0o020000'u32  ## char special
  proc S_ISDIR*(m: Mode): bool = fileType(m) == 0o040000'u32  ## directory
  proc S_ISFIFO*(m: Mode): bool = fileType(m) == 0o010000'u32 ## FIFO/pipe
  proc S_ISREG*(m: Mode): bool = fileType(m) == 0o100000'u32  ## regular file
  proc S_ISLNK*(m: Mode): bool = fileType(m) == 0o120000'u32  ## symlink
  proc S_ISSOCK*(m: Mode): bool = fileType(m) == 0o140000'u32 ## socket

  when defined(linux) and defined(i386):
    proc mmap*(a1: nil pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
      importc: "mmap64".}
  else:
    proc mmap*(a1: nil pointer, a2: int, a3, a4, a5: cint, a6: Off): pointer {.
      importc: "mmap".}
  proc munmap*(a1: nil pointer, a2: int): cint {.importc: "munmap".}

  when defined(nimNativeIo):
    var errnoVar: cint = 0
      ## Native errno maintained by this module's freestanding syscall wrappers
      ## (currently the directory ops). Mirrors libc's `errno`. NOTE: on the
      ## C-backend native build the bare-importc syscalls still set libc's
      ## `errno`, not this one, so error codes are only fully accurate on the
      ## raw-syscall (arkham) target — happy paths work in both (see `pcall`'s
      ## caveat).
    proc errno*(): cint {.inline.} = errnoVar
      ## The last error code. Consumers like `std/dirs` report errors via
      ## `posixToErrorCode(errno())` with no libc involved.
  else:
    # libc IO is in use, so libc's own calls (fopen, opendir, ...) report
    # failures through libc's errno. `errno` itself is a header macro; the
    # stable, header-free way to reach it is the address-returning accessor
    # every modern libc exports: `__errno_location` on Linux (glibc and musl),
    # `__error` on Darwin.
    when defined(osx):
      proc errnoLocation(): ptr cint {.importc: "__error", sideEffect.}
    else:
      proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}
    proc errno*(): cint {.inline.} = errnoLocation()[]
      ## The last error code (libc's `errno`).

  template pcall*(x: untyped): clong {.untyped.} =
    ## Normalizes a syscall-style call to the Linux raw convention: returns the
    ## non-negative result on success, or `-errno` on failure. Hides whether the
    ## error is signalled by the raw syscall's negative return (freestanding /
    ## arkham, `-d:nimNativeIo`) or by libc's `-1` + the `errno` global.
    when defined(nimNativeIo):
      clong(x)
    else:
      let r = clong(x)
      if r < 0: clong(-errno()) else: r

  template mmapFailed*(p: pointer): bool =
    ## True if an `mmap` result indicates failure. The kernel signals failure as
    ## an address in `[-4095, -1]`; libc maps that to `MAP_FAILED` (`(void*)-1`,
    ## itself in range), so one range check covers both conventions.
    cast[int](p) >= -4095 and cast[int](p) <= -1

  template mmapErrno*(p: pointer): cint =
    ## `errno` for a failed `mmap` (see `mmapFailed`).
    when defined(nimNativeIo): cint(-cast[int](p))
    else: errno()

  proc clock_gettime*(a1: ClockId, a2: var Timespec): cint {.importc: "clock_gettime", sideEffect.}

  proc getcwd*(a1: cstring, a2: int): cstring {.importc: "getcwd", sideEffect.}
  proc chdir*(path: cstring): cint {.importc: "chdir", sideEffect.}

  proc realpath*(path, resolved: cstring): cstring {.importc: "realpath", sideEffect.}

  proc readlink*(a1, a2: cstring, a3: int): int {.importc: "readlink".}
  proc symlink*(a1, a2: cstring): cint {.importc: "symlink".}

  # Directory operations
  when defined(linux):
    # `opendir`/`readdir`/`closedir` are libc functions (`DIR` is an opaque libc
    # buffer), not syscalls, so on Linux they are reimplemented on top of
    # open(2) + getdents64(2) + close(2) for every configuration. `Dirent`
    # keeps the same two fields (`d_type`, `d_name`) the consumers read, but
    # with a native layout — its bytes are copied out of the raw
    # `struct linux_dirent64` records.
    const
      O_DIRECTORY = (when defined(arm64): cint(0o40000) else: cint(0o200000))
        ## arm64 overrides the asm-generic value (its 0o200000 slot is
        ## O_DIRECT there — using the x86 constant made opendir fail on
        ## arm64); amd64 and i386 share the asm-generic layout.
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
      when defined(nimNativeIo):
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
  else:
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
      proc opendir*(name: cstring): nil ptr DIR {.importc: "opendir$INODE64", sideEffect.}
      proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc: "readdir$INODE64", sideEffect.}
    else:
      proc opendir*(name: cstring): nil ptr DIR {.importc: "opendir", sideEffect.}
      proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc: "readdir", sideEffect.}
    proc closedir*(dirp: nil ptr DIR): cint {.importc: "closedir", sideEffect.}

  proc mkdir*(path: cstring, mode: Mode): cint {.importc: "mkdir", sideEffect.}
  proc rmdir*(path: cstring): cint {.importc: "rmdir", sideEffect.}
  proc unlink*(path: cstring): cint {.importc: "unlink", sideEffect.}

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

  proc sysconf*(a1: cint): int {.importc: "sysconf".}

  # posix_fallocate: a libc function, not a syscall.
  when defined(linux):
    # Implemented over the `fallocate` symbol, which is a glibc/musl export
    # AND a raw syscall name arkham can lower — one binding serves every
    # configuration (glibc's own posix_fallocate is just this plus a
    # gap-plugging fallback for filesystems without fallocate support).
    when defined(i386):
      proc fallocateImpl(fd: cint; mode: cint; offset, len: Off): cint {.
        importc: "fallocate64", sideEffect.}
    else:
      proc fallocateImpl(fd: cint; mode: cint; offset, len: Off): cint {.
        importc: "fallocate", sideEffect.}
    proc posix_fallocate*(a1: cint, a2, a3: Off): cint =
      let r = pcall(fallocateImpl(a1, 0, a2, a3))
      if r < 0: cint(-r) else: cint(0)
  else:
    # 2001 POSIX evidently does not concern Apple: no posix_fallocate on
    # macOS. Keep the traditional fcntl(F_PREALLOCATE) emulation, with the
    # fstore_t layout and F_* values transcribed from the xnu <fcntl.h>.
    type FStore {.pure.} = object ## fstore_t (32 bytes)
      fst_flags: uint32     ## IN: flags word
      fst_posmode: cint     ## IN: indicates offset field
      fst_offset: Off       ## IN: start of the region
      fst_length: Off       ## IN: size of the region
      fst_bytesalloc: Off   ## OUT: number of bytes allocated
    const
      F_PREALLOCATE = cint(42)
      F_PEOFPOSMODE = cint(3)
      F_ALLOCATEALL = uint32(4)
    proc posix_fallocate*(a1: cint, a2, a3: Off): cint =
      var fst = FStore(fst_flags: F_ALLOCATEALL, fst_posmode: F_PEOFPOSMODE,
                       fst_offset: a2, fst_length: a3)
      # Must also call ftruncate to match what POSIX does. Unlike posix_fallocate,
      # this can shrink files.  Could guard w/getFileSize, but caller likely knows
      # present size & has no good reason to call this unless it is growing.
      if fcntl(a1, F_PREALLOCATE, fst.addr) != cint(-1): ftruncate(a1, a2 + a3)
      else: cint(-1)

  # <sys/wait.h> status decoding — header macros, reimplemented natively.
  proc WEXITSTATUS*(s: cint): cint =  (s and 0xff00) shr 8
  proc WTERMSIG*(s: cint): cint = s and 0x7f
  proc WSTOPSIG*(s: cint): cint = WEXITSTATUS(s)
  proc WIFEXITED*(s: cint): bool = WTERMSIG(s) == 0
  proc WIFSIGNALED*(s: cint): bool = (cast[int8]((s and 0x7f) + 1) shr 1) > 0
  proc WIFSTOPPED*(s: cint): bool = (s and 0xff) == 0x7f
  proc WIFCONTINUED*(s: cint): bool = s == WCONTINUED

  # -------- Process / pipe / exec bindings needed by std/osproc --------
  # Plain fork+exec on every configuration; the posix_spawn family is gone
  # (its attribute/file-action types are opaque libc-internal structs — the
  # exact kind of untranscribable ABI this module no longer depends on).

  # Use plain C `char` so that `char**` lines up with libc's expectation
  # (Nimony's `cstring` is `NC8*` / unsigned char*, which triggers
  # `-Wincompatible-pointer-types` on execve).
  type CChar* {.importc: "char", nodecl.} = int8
  type CCharArray* = nil ptr UncheckedArray[nil ptr CChar]

  proc pipe*(a: ptr cint): cint {.importc: "pipe", sideEffect.}
  proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
  proc fork*(): Pid {.importc: "fork", sideEffect.}
  proc execve*(path: cstring; argv, env: CCharArray): cint {.importc: "execve", sideEffect.}
  # There is no `waitpid` Linux syscall — it is libc sugar for `wait4` with a
  # NULL `rusage`. `wait4` is exported by glibc, musl and libSystem, and
  # arkham lowers the bare name to the raw syscall; binding it directly keeps
  # ONE symbol for every configuration, with the 3-arg `waitpid` signature the
  # rest of the code expects provided as an inline wrapper.
  proc wait4(pid: Pid; status: var cint; options: cint;
             rusage: nil pointer): Pid {.importc: "wait4", sideEffect.}
  proc waitpid*(pid: Pid; status: var cint; options: cint): Pid {.inline.} =
    wait4(pid, status, options, nil)
  proc kill*(pid: Pid; sig: cint): cint {.importc: "kill", sideEffect.}
  proc setpgid*(pid, pgid: Pid): cint {.importc: "setpgid", sideEffect.}
  proc exitnow*(status: cint) {.importc: "_exit", noreturn.}
  proc read*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "read", sideEffect.}
  proc write*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "write", sideEffect.}

  # The environment block. The generated `main` captures its third parameter
  # (`char** envp`) into the `nimEnviron` global on every backend (see hexer's
  # genMainProc), so no libc `environ`/`_NSGetEnviron` binding is needed.
  var posix_environ* {.importc: "nimEnviron".}: ptr UncheckedArray[cstring]

  proc strerror*(errnum: cint): cstring {.importc: "strerror", sideEffect.}

  proc nanosleep*(req: var Timespec; rem: var Timespec): cint {.importc: "nanosleep", sideEffect.}

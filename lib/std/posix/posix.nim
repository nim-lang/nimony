## This is a raw POSIX interface module. It does not provide any
## convenience: cstrings are used instead of proper Nim strings and
## return codes indicate errors. If you want exceptions
## and a proper Nim-like interface, use the OS module or write a wrapper.
##
## Every binding is header-free: types and constants are ABI transcriptions
## (see linux/, macos/, and illumos/) and procs bind real libc/kernel symbols.
## `-d:useLibc` changes which allocator and stdio implementation the
## stdlib uses, never which declarations exist.
## ABI declarations are checked by tests/nimony/stdlib/tposixabi.nim.

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(posix):
  # Foundation types must precede platform Stat layouts: nimony does not
  # resolve forward references across these conditional type sections.
  type
    ClockId* = cint  ## clockid_t (int on Linux and Darwin)
    Time* = distinct clong  ## time_t
    Timespec* {.pure.} = object
      tv_sec*: Time  ## Seconds.
      tv_nsec*: clong  ## Nanoseconds.
    IOVec* {.pure} = object ## struct iovec
      iov_base*: pointer ## Base address of a memory region for input or output.
      iov_len*: csize_t ## The size of the memory pointed to by iov_base.
    SockLen* = cuint ## socklen_t
    InAddr* {.pure.} = object ## struct in_addr
      s_addr*: uint32
    InAddrScalar* = uint32
    Sighandler* = proc (a: cint) {.noconv.}
    FileHandle* = cint
    SocketHandle* = cint
    Pid* = cint  ## pid_t

  # Private includes preserve the public namespace and type identity.
  # Order: foundations, OS constants/layouts, shared helpers, OS operations.
  when defined(linux):
    include "linux/consts"
    include "linux/types"
  elif defined(osx):
    include "macos/consts"
    include "macos/types"
  elif defined(illumos):
    when defined(nimNoLibc):
      {.error: "illumos always requires libc; nimNoLibc is not supported".}
    include "illumos/consts"
    include "illumos/types"
  else:
    {.error: "std/posix has no transcribed ABI for this OS; supported: Linux, macOS, illumos/amd64".}

  # Permission bits shared by the supported ABIs.
  const
    S_IRUSR* = cint(0o400)
    S_IWUSR* = cint(0o200)

  const linuxA64Raw* = defined(linux) and defined(arm64) and defined(nimNoLibc)
    ## Linux/AArch64 on the truly freestanding backend (`nimony n`: arkham +
    ## nifasm, no libc linked). The asm-generic syscall table lacks legacy
    ## operations such as open/stat/fork; linux/arm64/raw implements them
    ## using the available *at/*2/clone calls.
    ##
    ## `nimNoLibc`, NOT `nimNativeIo`: the C backend still LINKS libc, so
    ## there open/stat are real symbols. Using nimNativeIo here would emit
    ## newfstatat calls, which glibc does not export under that name.

  const StatHasNanoseconds* = true
    ## All supported targets store nanosecond-resolution timestamps.

  template st_mtime*(s: Stat): int64 = int64(s.st_mtim.tv_sec)
    ## Time of last data modification (seconds since epoch).

  # GCC treats POSIX functions like execve as builtins and diagnoses pointer
  # qualifier differences in our header-free bindings despite identical ABI
  # (nim-lang/nimony#2148). Clang has no such warning, so guard this to GCC.
  when defined(gcc):
    {.passC: "-Wno-builtin-declaration-mismatch".}

  proc fcntl*(a1: cint, a2: cint): cint {.varargs, importc: "fcntl", sideEffect.}
  proc close*(a1: cint): cint {.importc: "close".}

  # Header macros reimplemented from the file-type bits, identical across
  # the supported ABIs.
  template fileType(m: Mode): uint32 = uint32(m) and 0o170000'u32 # S_IFMT
  proc S_ISBLK*(m: Mode): bool = fileType(m) == 0o060000'u32
  proc S_ISCHR*(m: Mode): bool = fileType(m) == 0o020000'u32
  proc S_ISDIR*(m: Mode): bool = fileType(m) == 0o040000'u32
  proc S_ISFIFO*(m: Mode): bool = fileType(m) == 0o010000'u32
  proc S_ISREG*(m: Mode): bool = fileType(m) == 0o100000'u32
  proc S_ISLNK*(m: Mode): bool = fileType(m) == 0o120000'u32
  proc S_ISSOCK*(m: Mode): bool = fileType(m) == 0o140000'u32

  # C size_t must not be declared as int when another binding includes
  # sys/mman.h in the same C unit.
  proc munmap*(a1: nil pointer, a2: csize_t): cint {.importc: "munmap".}

  # Only truly freestanding builds use native errno. The C backend still
  # links libc even with nimNativeIo, and libc writes its own errno slot.
  when defined(nimNoLibc):
    var errnoVar: cint = 0
      ## Maintained by our directory wrappers; raw calls return -errno.
    proc errno*(): cint {.inline.} = errnoVar
  else:
    when defined(osx):
      include "macos/errno"
    elif defined(linux):
      include "linux/errno"
    elif defined(illumos):
      include "illumos/errno"
    proc errno*(): cint {.inline.} = errnoLocation()[]
      ## The last error code (libc's errno).

  template pcall*(x: untyped): clong {.untyped.} =
    ## Normalize a syscall-style call: non-negative result or -errno.
    ## Freestanding calls already return -errno; libc returns -1 and sets
    ## its errno slot. Consumers should use this instead of reading errno.
    when defined(nimNoLibc):
      clong(x)
    else:
      let r = clong(x)
      if r < 0: clong(-errno()) else: r

  template mmapFailed*(p: pointer): bool =
    ## Kernel errors are addresses in [-4095, -1]; libc's MAP_FAILED (-1)
    ## is in the same range.
    cast[int](p) >= -4095 and cast[int](p) <= -1

  template mmapErrno*(p: pointer): cint =
    ## Error code for a failed mmap.
    when defined(nimNoLibc): cint(-cast[int](p))
    else: errno()

  proc clock_gettime*(a1: ClockId, a2: var Timespec): cint {.importc: "clock_gettime", sideEffect.}
  proc getcwd*(a1: cstring, a2: int): cstring {.importc: "getcwd", sideEffect.}
  proc chdir*(path: cstring): cint {.importc: "chdir", sideEffect.}
  proc realpath*(path, resolved: cstring): cstring {.importc: "realpath", sideEffect.}

  # Platform operations depend on the shared pcall/errno helpers above.
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
  when defined(linux):
    include "linux/bindings"
    include "linux/dirs"
  elif defined(osx):
    include "macos/bindings"
    include "macos/dirs"
  elif defined(illumos):
    include "illumos/bindings"
    include "illumos/dirs"

  # Directory entry type constants shared by the supported ABIs.
  const
    DT_UNKNOWN* = 0'u8
    DT_FIFO* = 1'u8
    DT_CHR* = 2'u8
    DT_DIR* = 4'u8
    DT_BLK* = 6'u8
    DT_REG* = 8'u8
    DT_LNK* = 10'u8
    DT_SOCK* = 12'u8
    DT_WHT* = 14'u8

  proc sysconf*(a1: cint): int {.importc: "sysconf".}

  when not defined(illumos):
    # sys/wait.h status macros, reimplemented natively.
    proc WEXITSTATUS*(s: cint): cint = (s and 0xff00) shr 8
    proc WTERMSIG*(s: cint): cint = s and 0x7f
    proc WSTOPSIG*(s: cint): cint = WEXITSTATUS(s)
    proc WIFEXITED*(s: cint): bool = WTERMSIG(s) == 0
    proc WIFSIGNALED*(s: cint): bool = (cast[int8]((s and 0x7f) + 1) shr 1) > 0
    proc WIFSTOPPED*(s: cint): bool = (s and 0xff) == 0x7f
    proc WIFCONTINUED*(s: cint): bool = s == WCONTINUED

  # Use plain C char for execve's char** (cstring uses unsigned char*).
  type CChar* {.importc: "char", nodecl.} = int8
  type CCharArray* = nil ptr UncheckedArray[nil ptr CChar]

  proc execve*(path: cstring; argv, env: CCharArray): cint {.importc: "execve", sideEffect.}
  # waitpid is libc sugar for wait4 with NULL rusage. wait4 is exported by
  # glibc, musl and libSystem, and arkham lowers it to the raw syscall.
  when not defined(illumos):
    proc wait4(pid: Pid; status: var cint; options: cint;
               rusage: nil pointer): Pid {.importc: "wait4", sideEffect.}
    proc waitpid*(pid: Pid; status: var cint; options: cint): Pid {.inline.} =
      wait4(pid, status, options, nil)
  proc kill*(pid: Pid; sig: cint): cint {.importc: "kill", sideEffect.}
  proc setpgid*(pid, pgid: Pid): cint {.importc: "setpgid", sideEffect.}
  proc exitnow*(status: cint) {.importc: "_exit", noreturn.}
  proc read*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "read", sideEffect.}
  proc write*(fildes: cint; buf: pointer; nbyte: int): int {.importc: "write", sideEffect.}

  # genMainProc captures main's envp into nimEnviron on every backend.
  var posix_environ* {.importc: "nimEnviron".}: ptr UncheckedArray[cstring]

  proc strerror*(errnum: cint): cstring {.importc: "strerror", sideEffect.}
  proc nanosleep*(req: var Timespec; rem: var Timespec): cint {.importc: "nanosleep", sideEffect.}

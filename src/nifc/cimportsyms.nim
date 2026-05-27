#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## C import macro symbol resolver.
##
## Some C symbols used via `{.importc, header.}` are actually `#define` macros
## (e.g. `stdout` → `__stdoutp` on macOS).  This module resolves known macros
## so the LLVM backend emits the real symbol name.

import std / [tables]

type
  Platform* = enum
    pfMacOS
    pfLinux
    pfWindows

  ImportMacroKind* = enum
    imkReplace       ## replace with C symbol name → external global
    imkConstant      ## replace with constant value → private constant
    imkCall          ## emit a call instruction

  ImportMacroEntry* = object
    case kind*: ImportMacroKind
    of imkReplace:
      resolvedName*: string
    of imkConstant:
      constVal*: string
    of imkCall:
      callFuncName*: string

template S(v: string): ImportMacroEntry = ImportMacroEntry(kind: imkReplace, resolvedName: v)
template I(v: string): ImportMacroEntry = ImportMacroEntry(kind: imkConstant, constVal: v)
template C(v: string): ImportMacroEntry = ImportMacroEntry(kind: imkCall, callFuncName: v)

# macOS import map
const macOSImportMap* = block:
  var m: Table[string, ImportMacroEntry]
  m["stdin"] = S("__stdinp")
  m["stdout"] = S("__stdoutp")
  m["stderr"] = S("__stderrp")
  m["errno"] = C("__errno_location")
  # errno
  m["EPERM"] = I("1"); m["ENOENT"] = I("2"); m["ESRCH"] = I("3"); m["EINTR"] = I("4")
  m["EIO"] = I("5"); m["ENXIO"] = I("6"); m["E2BIG"] = I("7"); m["ENOEXEC"] = I("8")
  m["EBADF"] = I("9"); m["ECHILD"] = I("10"); m["EAGAIN"] = I("35"); m["EWOULDBLOCK"] = I("35")
  m["ENOMEM"] = I("12"); m["EACCES"] = I("13"); m["EFAULT"] = I("14"); m["EBUSY"] = I("16")
  m["EEXIST"] = I("17"); m["EXDEV"] = I("18"); m["ENODEV"] = I("19"); m["ENOTDIR"] = I("20")
  m["EISDIR"] = I("21"); m["EINVAL"] = I("22"); m["ENFILE"] = I("23"); m["EMFILE"] = I("24")
  m["ENOTTY"] = I("25"); m["EFBIG"] = I("27"); m["ENOSPC"] = I("28"); m["ESPIPE"] = I("29")
  m["EROFS"] = I("30"); m["EMLINK"] = I("31"); m["EPIPE"] = I("32"); m["EDOM"] = I("33")
  m["ERANGE"] = I("34"); m["ENOTBLK"] = I("15"); m["ETXTBSY"] = I("26")
  m["EDEADLK"] = I("11"); m["ENAMETOOLONG"] = I("63"); m["ENOLCK"] = I("46")
  m["ENOSYS"] = I("78"); m["ENOTEMPTY"] = I("66"); m["ELOOP"] = I("62")
  m["EADDRINUSE"] = I("48"); m["EADDRNOTAVAIL"] = I("49"); m["ECONNABORTED"] = I("53")
  m["ECONNREFUSED"] = I("61"); m["ECONNRESET"] = I("54"); m["EHOSTUNREACH"] = I("65")
  m["EISCONN"] = I("56"); m["ENETDOWN"] = I("50"); m["ENETRESET"] = I("52")
  m["ENETUNREACH"] = I("51"); m["ETIMEDOUT"] = I("60"); m["EINPROGRESS"] = I("36")
  m["ENOTSOCK"] = I("38"); m["EDESTADDRREQ"] = I("39"); m["EMSGSIZE"] = I("40")
  m["EPROTOTYPE"] = I("41"); m["ENOPROTOOPT"] = I("42"); m["EPROTONOSUPPORT"] = I("43")
  m["EAFNOSUPPORT"] = I("47"); m["ENOTSUP"] = I("45"); m["EOPNOTSUPP"] = I("102")
  m["ENOTCONN"] = I("57"); m["EDQUOT"] = I("69"); m["ESTALE"] = I("70")
  m["ENOMSG"] = I("42"); m["EIDRM"] = I("43"); m["ECANCELED"] = I("89")
  m["EILSEQ"] = I("92"); m["EOVERFLOW"] = I("84"); m["EBADMSG"] = I("94")
  m["EPROTO"] = I("100"); m["ENODATA"] = I("96"); m["ENOSR"] = I("98")
  m["ENOSTR"] = I("53"); m["ETIME"] = I("101")
  # fcntl/open
  m["O_RDONLY"] = I("0"); m["O_WRONLY"] = I("1"); m["O_RDWR"] = I("2")
  m["O_ACCMODE"] = I("3"); m["O_CREAT"] = I("0x200"); m["O_EXCL"] = I("0x800")
  m["O_TRUNC"] = I("0x400"); m["O_APPEND"] = I("0x8"); m["O_NONBLOCK"] = I("0x4")
  m["O_SYNC"] = I("0x8000"); m["O_DSYNC"] = I("0x400000"); m["O_DIRECTORY"] = I("0x100000")
  m["O_CLOEXEC"] = I("0x1000000"); m["O_NOCTTY"] = I("0x20000"); m["O_DIRECT"] = I("0x400000")
  m["O_NOFOLLOW"] = I("0x100")
  # socket
  m["AF_INET"] = I("2"); m["AF_INET6"] = I("30"); m["AF_UNIX"] = I("1"); m["AF_UNSPEC"] = I("0")
  m["SOCK_STREAM"] = I("1"); m["SOCK_DGRAM"] = I("2"); m["SOCK_RAW"] = I("3"); m["SOCK_SEQPACKET"] = I("5")
  m["SOL_SOCKET"] = I("0xFFFF")
  m["SO_REUSEADDR"] = I("4"); m["SO_KEEPALIVE"] = I("8"); m["SO_BROADCAST"] = I("0x20")
  m["SO_LINGER"] = I("0x80"); m["SO_ERROR"] = I("0x1007"); m["SO_TYPE"] = I("0x1008")
  m["SO_ACCEPTCONN"] = I("0x1002"); m["SO_RCVBUF"] = I("0x1003"); m["SO_SNDBUF"] = I("0x1001")
  m["TCP_NODELAY"] = I("1"); m["SOMAXCONN"] = I("128")
  m["IPPROTO_TCP"] = I("6"); m["IPPROTO_UDP"] = I("17"); m["IPPROTO_IP"] = I("0")
  m["IPPROTO_ICMP"] = I("1"); m["IPPROTO_ICMPV6"] = I("58"); m["IPPROTO_RAW"] = I("255")
  m["SHUT_RD"] = I("0"); m["SHUT_WR"] = I("1"); m["SHUT_RDWR"] = I("2")
  m["INET_ADDRSTRLEN"] = I("16"); m["INET6_ADDRSTRLEN"] = I("46")
  # signal
  m["SIGHUP"] = I("1"); m["SIGINT"] = I("2"); m["SIGQUIT"] = I("3"); m["SIGILL"] = I("4")
  m["SIGTRAP"] = I("5"); m["SIGABRT"] = I("6"); m["SIGEMT"] = I("7"); m["SIGFPE"] = I("8")
  m["SIGKILL"] = I("9"); m["SIGBUS"] = I("10"); m["SIGSEGV"] = I("11"); m["SIGSYS"] = I("12")
  m["SIGPIPE"] = I("13"); m["SIGALRM"] = I("14"); m["SIGTERM"] = I("15"); m["SIGURG"] = I("16")
  m["SIGSTOP"] = I("17"); m["SIGTSTP"] = I("18"); m["SIGCONT"] = I("19"); m["SIGCHLD"] = I("20")
  m["SIGTTIN"] = I("21"); m["SIGTTOU"] = I("22"); m["SIGIO"] = I("23")
  m["SIGXCPU"] = I("24"); m["SIGXFSZ"] = I("25"); m["SIGPROF"] = I("29")
  m["SIGUSR1"] = I("30"); m["SIGUSR2"] = I("31"); m["SIGVTALRM"] = I("28")
  m["MINSIGSTKSZ"] = I("32768")
  # seek
  m["SEEK_SET"] = I("0"); m["SEEK_CUR"] = I("1"); m["SEEK_END"] = I("2")
  # stdio
  m["_IOFBF"] = I("0"); m["_IOLBF"] = I("1"); m["_IONBF"] = I("2")
  m["EOF"] = I("-1"); m["BUFSIZ"] = I("1024")
  m["F_OK"] = I("0"); m["R_OK"] = I("4"); m["W_OK"] = I("2"); m["X_OK"] = I("1")
  # poll
  m["POLLIN"] = I("1"); m["POLLOUT"] = I("4"); m["POLLERR"] = I("8"); m["POLLHUP"] = I("16")
  m["POLLNVAL"] = I("32"); m["POLLPRI"] = I("2"); m["POLLRDNORM"] = I("64")
  m["POLLWRNORM"] = I("4"); m["POLLRDBAND"] = I("128"); m["POLLWRBAND"] = I("4")
  # mmap
  m["PROT_READ"] = I("1"); m["PROT_WRITE"] = I("2"); m["PROT_EXEC"] = I("4"); m["PROT_NONE"] = I("0")
  m["MAP_SHARED"] = I("1"); m["MAP_PRIVATE"] = I("2"); m["MAP_ANONYMOUS"] = I("0x1000")
  m["MAP_FIXED"] = I("16"); m["MAP_FAILED"] = S("MAP_FAILED")
  # ipc
  m["IPC_CREAT"] = I("0x200"); m["IPC_EXCL"] = I("0x400"); m["IPC_NOWAIT"] = I("0x800")
  m["IPC_RMID"] = I("0"); m["IPC_PRIVATE"] = I("0")
  # misc
  m["FD_SETSIZE"] = I("1024"); m["FD_CLOEXEC"] = I("1"); m["CLOCKS_PER_SEC"] = I("1000000")
  m

# Linux import map
const linuxImportMap* = block:
  var m: Table[string, ImportMacroEntry]
  m["stdin"] = S("stdin")
  m["stdout"] = S("stdout")
  m["stderr"] = S("stderr")
  m["errno"] = C("__errno_location")
  # errno
  m["EPERM"] = I("1"); m["ENOENT"] = I("2"); m["ESRCH"] = I("3"); m["EINTR"] = I("4")
  m["EIO"] = I("5"); m["ENXIO"] = I("6"); m["E2BIG"] = I("7"); m["ENOEXEC"] = I("8")
  m["EBADF"] = I("9"); m["ECHILD"] = I("10"); m["EAGAIN"] = I("11"); m["EWOULDBLOCK"] = I("11")
  m["ENOMEM"] = I("12"); m["EACCES"] = I("13"); m["EFAULT"] = I("14"); m["EBUSY"] = I("16")
  m["EEXIST"] = I("17"); m["EXDEV"] = I("18"); m["ENODEV"] = I("19"); m["ENOTDIR"] = I("20")
  m["EISDIR"] = I("21"); m["EINVAL"] = I("22"); m["ENFILE"] = I("23"); m["EMFILE"] = I("24")
  m["ENOTTY"] = I("25"); m["EFBIG"] = I("27"); m["ENOSPC"] = I("28"); m["ESPIPE"] = I("29")
  m["EROFS"] = I("30"); m["EMLINK"] = I("31"); m["EPIPE"] = I("32"); m["EDOM"] = I("33")
  m["ERANGE"] = I("34"); m["ENOTBLK"] = I("15"); m["ETXTBSY"] = I("26")
  m["EDEADLK"] = I("35"); m["ENAMETOOLONG"] = I("36"); m["ENOLCK"] = I("37")
  m["ENOSYS"] = I("38"); m["ENOTEMPTY"] = I("39"); m["ELOOP"] = I("40")
  m["EADDRINUSE"] = I("98"); m["EADDRNOTAVAIL"] = I("99"); m["ECONNABORTED"] = I("103")
  m["ECONNREFUSED"] = I("111"); m["ECONNRESET"] = I("104"); m["EHOSTUNREACH"] = I("113")
  m["EISCONN"] = I("106"); m["ENETDOWN"] = I("100"); m["ENETRESET"] = I("102")
  m["ENETUNREACH"] = I("101"); m["ETIMEDOUT"] = I("110"); m["EINPROGRESS"] = I("115")
  m["ENOTSOCK"] = I("88"); m["EDESTADDRREQ"] = I("89"); m["EMSGSIZE"] = I("90")
  m["EPROTOTYPE"] = I("91"); m["ENOPROTOOPT"] = I("92"); m["EPROTONOSUPPORT"] = I("93")
  m["EAFNOSUPPORT"] = I("97"); m["ENOTSUP"] = I("95"); m["EOPNOTSUPP"] = I("95")
  m["ENOTCONN"] = I("107"); m["EDQUOT"] = I("122"); m["ESTALE"] = I("116")
  m["ENOMSG"] = I("42"); m["EIDRM"] = I("43"); m["ECANCELED"] = I("125")
  m["EILSEQ"] = I("84"); m["EOVERFLOW"] = I("75"); m["EBADMSG"] = I("74")
  m["EPROTO"] = I("71"); m["ENODATA"] = I("45"); m["ENOSR"] = I("47")
  m["ENOSTR"] = I("44"); m["ETIME"] = I("46")
  # fcntl/open
  m["O_RDONLY"] = I("0"); m["O_WRONLY"] = I("1"); m["O_RDWR"] = I("2"); m["O_ACCMODE"] = I("3")
  m["O_CREAT"] = I("64"); m["O_EXCL"] = I("128"); m["O_TRUNC"] = I("512"); m["O_APPEND"] = I("1024")
  m["O_NONBLOCK"] = I("2048"); m["O_SYNC"] = I("0x101000"); m["O_DSYNC"] = I("4096")
  m["O_DIRECTORY"] = I("65536"); m["O_CLOEXEC"] = I("0x800000"); m["O_NOCTTY"] = I("256")
  m["O_DIRECT"] = I("16384"); m["O_NOFOLLOW"] = I("0x80000"); m["O_TMPFILE"] = I("0x410000")
  # socket
  m["AF_INET"] = I("2"); m["AF_INET6"] = I("10"); m["AF_UNIX"] = I("1"); m["AF_UNSPEC"] = I("0")
  m["SOCK_STREAM"] = I("1"); m["SOCK_DGRAM"] = I("2"); m["SOCK_RAW"] = I("3"); m["SOCK_SEQPACKET"] = I("5")
  m["SOL_SOCKET"] = I("1")
  m["SO_REUSEADDR"] = I("2"); m["SO_KEEPALIVE"] = I("9"); m["SO_BROADCAST"] = I("6")
  m["SO_LINGER"] = I("13"); m["SO_ERROR"] = I("4"); m["SO_TYPE"] = I("3"); m["SO_ACCEPTCONN"] = I("30")
  m["SO_RCVBUF"] = I("8"); m["SO_SNDBUF"] = I("7"); m["TCP_NODELAY"] = I("1"); m["SOMAXCONN"] = I("128")
  m["IPPROTO_TCP"] = I("6"); m["IPPROTO_UDP"] = I("17"); m["IPPROTO_IP"] = I("0")
  m["IPPROTO_ICMP"] = I("1"); m["IPPROTO_ICMPV6"] = I("58"); m["IPPROTO_RAW"] = I("255")
  m["SHUT_RD"] = I("0"); m["SHUT_WR"] = I("1"); m["SHUT_RDWR"] = I("2")
  m["INET_ADDRSTRLEN"] = I("16"); m["INET6_ADDRSTRLEN"] = I("46")
  # signal
  m["SIGHUP"] = I("1"); m["SIGINT"] = I("2"); m["SIGQUIT"] = I("3"); m["SIGILL"] = I("4")
  m["SIGTRAP"] = I("5"); m["SIGABRT"] = I("6"); m["SIGFPE"] = I("8"); m["SIGKILL"] = I("9")
  m["SIGBUS"] = I("7"); m["SIGSEGV"] = I("11"); m["SIGSYS"] = I("31"); m["SIGPIPE"] = I("13")
  m["SIGALRM"] = I("14"); m["SIGTERM"] = I("15"); m["SIGURG"] = I("23"); m["SIGSTOP"] = I("19")
  m["SIGTSTP"] = I("20"); m["SIGCONT"] = I("18"); m["SIGCHLD"] = I("17")
  m["SIGTTIN"] = I("21"); m["SIGTTOU"] = I("22"); m["SIGIO"] = I("29")
  m["SIGXCPU"] = I("24"); m["SIGXFSZ"] = I("25"); m["SIGPROF"] = I("27")
  m["SIGUSR1"] = I("10"); m["SIGUSR2"] = I("12"); m["SIGVTALRM"] = I("26")
  m["SIGSTKSZ"] = I("8192"); m["MINSIGSTKSZ"] = I("2048")
  # seek
  m["SEEK_SET"] = I("0"); m["SEEK_CUR"] = I("1"); m["SEEK_END"] = I("2")
  # stdio
  m["_IOFBF"] = I("0"); m["_IOLBF"] = I("1"); m["_IONBF"] = I("2")
  m["EOF"] = I("-1"); m["BUFSIZ"] = I("8192")
  m["F_OK"] = I("0"); m["R_OK"] = I("4"); m["W_OK"] = I("2"); m["X_OK"] = I("1")
  # poll
  m["POLLIN"] = I("1"); m["POLLOUT"] = I("4"); m["POLLERR"] = I("8"); m["POLLHUP"] = I("16")
  m["POLLNVAL"] = I("32"); m["POLLPRI"] = I("2"); m["POLLRDNORM"] = I("64")
  m["POLLWRNORM"] = I("4"); m["POLLRDBAND"] = I("128"); m["POLLWRBAND"] = I("4")
  # mmap
  m["PROT_READ"] = I("1"); m["PROT_WRITE"] = I("2"); m["PROT_EXEC"] = I("4"); m["PROT_NONE"] = I("0")
  m["MAP_SHARED"] = I("1"); m["MAP_PRIVATE"] = I("2"); m["MAP_ANONYMOUS"] = I("32")
  m["MAP_FIXED"] = I("16"); m["MAP_FAILED"] = S("MAP_FAILED")
  # ipc
  m["IPC_CREAT"] = I("512"); m["IPC_EXCL"] = I("1024"); m["IPC_NOWAIT"] = I("2048")
  m["IPC_RMID"] = I("0"); m["IPC_PRIVATE"] = I("0")
  # misc
  m["FD_SETSIZE"] = I("1024"); m["FD_CLOEXEC"] = I("1")
  m["CLOCKS_PER_SEC"] = I("1000000")
  m

# Windows import map
const windowsImportMap* = block:
  var m: Table[string, ImportMacroEntry]
  m["stdin"] = S("stdin")
  m["stdout"] = S("stdout")
  m["stderr"] = S("stderr")
  m["errno"] = C("_errno")
  # errno
  m["EPERM"] = I("1"); m["ENOENT"] = I("2"); m["ESRCH"] = I("3"); m["EINTR"] = I("4")
  m["EIO"] = I("5"); m["ENXIO"] = I("6"); m["E2BIG"] = I("7"); m["ENOEXEC"] = I("8")
  m["EBADF"] = I("9"); m["ECHILD"] = I("10"); m["EAGAIN"] = I("11"); m["EWOULDBLOCK"] = I("11")
  m["ENOMEM"] = I("12"); m["EACCES"] = I("13"); m["EFAULT"] = I("14"); m["EBUSY"] = I("16")
  m["EEXIST"] = I("17"); m["EXDEV"] = I("18"); m["ENODEV"] = I("19"); m["ENOTDIR"] = I("20")
  m["EISDIR"] = I("21"); m["EINVAL"] = I("22"); m["ENFILE"] = I("23"); m["EMFILE"] = I("24")
  m["ENOTTY"] = I("25"); m["EFBIG"] = I("27"); m["ENOSPC"] = I("28"); m["ESPIPE"] = I("29")
  m["EROFS"] = I("30"); m["EMLINK"] = I("31"); m["EPIPE"] = I("32"); m["EDOM"] = I("33")
  m["ERANGE"] = I("34")
  m["EDEADLK"] = I("36"); m["ENAMETOOLONG"] = I("38"); m["ENOLCK"] = I("39")
  m["ENOSYS"] = I("40"); m["ENOTEMPTY"] = I("41"); m["ELOOP"] = I("42")
  m["EADDRINUSE"] = I("100"); m["EADDRNOTAVAIL"] = I("101"); m["ECONNABORTED"] = I("106")
  m["ECONNREFUSED"] = I("107"); m["ECONNRESET"] = I("108"); m["EHOSTUNREACH"] = I("110")
  m["EISCONN"] = I("113"); m["ENETDOWN"] = I("115"); m["ENETRESET"] = I("117")
  m["ENETUNREACH"] = I("118"); m["ETIMEDOUT"] = I("138"); m["EINPROGRESS"] = I("119")
  # fcntl/open
  m["O_RDONLY"] = I("0"); m["O_WRONLY"] = I("1"); m["O_RDWR"] = I("2"); m["O_ACCMODE"] = I("3")
  m["O_CREAT"] = I("0x100"); m["O_EXCL"] = I("0x200"); m["O_TRUNC"] = I("0x200"); m["O_APPEND"] = I("0x400")
  # socket
  m["AF_INET"] = I("2"); m["AF_INET6"] = I("23"); m["AF_UNIX"] = I("1"); m["AF_UNSPEC"] = I("0")
  m["SOCK_STREAM"] = I("1"); m["SOCK_DGRAM"] = I("2"); m["SOCK_RAW"] = I("3")
  m["SO_REUSEADDR"] = I("4"); m["SO_KEEPALIVE"] = I("8"); m["SO_BROADCAST"] = I("0x20")
  m["SO_LINGER"] = I("0x80"); m["TCP_NODELAY"] = I("1")
  m["IPPROTO_TCP"] = I("6"); m["IPPROTO_UDP"] = I("17"); m["IPPROTO_IP"] = I("0")
  m["IPPROTO_ICMP"] = I("1"); m["IPPROTO_ICMPV6"] = I("58"); m["IPPROTO_RAW"] = I("255")
  m["SHUT_RD"] = I("0"); m["SHUT_WR"] = I("1"); m["SHUT_RDWR"] = I("2")
  m["INET_ADDRSTRLEN"] = I("22"); m["INET6_ADDRSTRLEN"] = I("65")
  # seek
  m["SEEK_SET"] = I("0"); m["SEEK_CUR"] = I("1"); m["SEEK_END"] = I("2")
  # stdio
  m["_IOFBF"] = I("0"); m["_IOLBF"] = I("1"); m["_IONBF"] = I("2")
  m["EOF"] = I("-1"); m["BUFSIZ"] = I("512")
  m["F_OK"] = I("0"); m["R_OK"] = I("4"); m["W_OK"] = I("2"); m["X_OK"] = I("1")
  # mmap
  m["PROT_READ"] = I("1"); m["PROT_WRITE"] = I("2"); m["PROT_EXEC"] = I("4"); m["PROT_NONE"] = I("0")
  m["MAP_SHARED"] = I("1"); m["MAP_PRIVATE"] = I("2"); m["MAP_FIXED"] = I("0x10")
  m["MAP_FAILED"] = S("MAP_FAILED")
  # misc
  m["FD_SETSIZE"] = I("64"); m["FD_CLOEXEC"] = I("1")
  m["CLOCKS_PER_SEC"] = I("1000")
  m

proc detectPlatform*(): Platform =
  when defined(macos) or defined(macosx):
    pfMacOS
  elif defined(linux):
    pfLinux
  elif defined(windows):
    pfWindows
  else:
    pfLinux

proc resolveImportMacro*(name: string; platform: Platform = detectPlatform()): ImportMacroEntry =
  const maps = [macOSImportMap, linuxImportMap, windowsImportMap]
  result = maps[platform.ord].getOrDefault(name)
  if result.kind notin {imkReplace, imkConstant, imkCall}:
    result = ImportMacroEntry(kind: imkReplace, resolvedName: name)
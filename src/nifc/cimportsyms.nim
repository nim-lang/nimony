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
  m["MAP_FIXED"] = I("16")
  # ipc
  m["IPC_CREAT"] = I("0x200"); m["IPC_EXCL"] = I("0x400"); m["IPC_NOWAIT"] = I("0x800")
  m["IPC_RMID"] = I("0"); m["IPC_PRIVATE"] = I("0")
  # misc
  m["FD_SETSIZE"] = I("1024"); m["FD_CLOEXEC"] = I("1"); m["CLOCKS_PER_SEC"] = I("1000000")
  # pthread (macOS values)
  m["PTHREAD_MUTEX_RECURSIVE"] = I("2"); m["PTHREAD_MUTEX_DEFAULT"] = I("2")
  m["PTHREAD_MUTEX_NORMAL"] = I("0"); m["PTHREAD_MUTEX_ERRORCHECK"] = I("3")
  m["PTHREAD_CREATE_JOINABLE"] = I("0"); m["PTHREAD_CREATE_DETACHED"] = I("1")
  m["PTHREAD_PRIO_NONE"] = I("0"); m["PTHREAD_PRIO_INHERIT"] = I("1"); m["PTHREAD_PRIO_PROTECT"] = I("2")
  m["PTHREAD_EXPLICIT_SCHED"] = I("1"); m["PTHREAD_INHERIT_SCHED"] = I("0")
  m["PTHREAD_SCOPE_SYSTEM"] = I("0"); m["PTHREAD_SCOPE_PROCESS"] = I("1")
  m["PTHREAD_PROCESS_PRIVATE"] = I("0"); m["PTHREAD_PROCESS_SHARED"] = I("1")
  m["PTHREAD_CANCEL_ENABLE"] = I("0"); m["PTHREAD_CANCEL_DISABLE"] = I("1")
  m["PTHREAD_CANCEL_DEFERRED"] = I("0"); m["PTHREAD_CANCEL_ASYNCHRONOUS"] = I("1")
  m["PTHREAD_BARRIER_SERIAL_THREAD"] = I("-1")
  # signal (macOS values)
  m["SIG_BLOCK"] = I("1"); m["SIG_UNBLOCK"] = I("2"); m["SIG_SETMASK"] = I("3")
  m["SIG_DFL"] = I("0"); m["SIG_IGN"] = I("1"); m["SIGPOLL"] = I("7")
  m["SA_NOCLDSTOP"] = I("8"); m["SA_NOCLDWAIT"] = I("0x20"); m["SA_SIGINFO"] = I("0x40")
  m["SA_ONSTACK"] = I("0x1"); m["SA_RESTART"] = I("0x2")
  m["SA_NODEFER"] = I("0x10"); m["SA_RESETHAND"] = I("0x4")
  m["SIGEV_NONE"] = I("0"); m["SIGEV_SIGNAL"] = I("1"); m["SIGEV_THREAD"] = I("2")
  # fcntl (macOS values)
  m["F_DUPFD"] = I("0"); m["F_GETFD"] = I("1"); m["F_SETFD"] = I("2")
  m["F_GETFL"] = I("3"); m["F_SETFL"] = I("4"); m["F_GETLK"] = I("7")
  m["F_SETLK"] = I("8"); m["F_SETLKW"] = I("9"); m["F_GETOWN"] = I("5"); m["F_SETOWN"] = I("6")
  m["F_RDLCK"] = I("1"); m["F_WRLCK"] = I("2"); m["F_UNLCK"] = I("3")
  # socket (macOS values)
  m["SO_DEBUG"] = I("0x1"); m["SO_DONTROUTE"] = I("0x10"); m["SO_OOBINLINE"] = I("0x100")
  m["SO_RCVLOWAT"] = I("0x1000"); m["SO_SNDLOWAT"] = I("0x1001"); m["SO_BINDTODEVICE"] = I("25")
  m["SO_RCVTIMEO"] = I("0x1006"); m["SO_SNDTIMEO"] = I("0x1005")
  m["MSG_CTRUNC"] = I("0x20"); m["MSG_DONTROUTE"] = I("0x4"); m["MSG_EOR"] = I("0x8")
  m["MSG_OOB"] = I("0x1"); m["MSG_PEEK"] = I("0x2"); m["MSG_TRUNC"] = I("0x10"); m["MSG_WAITALL"] = I("0x40")
  m["SCM_RIGHTS"] = I("1")
  m["IPPROTO_IPV6"] = I("41")
  m["IPV6_JOIN_GROUP"] = I("12"); m["IPV6_LEAVE_GROUP"] = I("13"); m["IPV6_MULTICAST_HOPS"] = I("10")
  m["IPV6_MULTICAST_IF"] = I("9"); m["IPV6_MULTICAST_LOOP"] = I("11"); m["IPV6_V6ONLY"] = I("27")
  m["IPV6_UNICAST_HOPS"] = I("4")
  m["INADDR_ANY"] = I("0"); m["INADDR_BROADCAST"] = I("0xffffffff"); m["INADDR_LOOPBACK"] = I("0x7f000001")
  m["IPPORT_RESERVED"] = I("1024"); m["IF_NAMESIZE"] = I("16")
  # stat mode (POSIX standard)
  m["S_IFMT"] = I("0xf000"); m["S_IFBLK"] = I("0x6000"); m["S_IFCHR"] = I("0x2000")
  m["S_IFDIR"] = I("0x4000"); m["S_IFIFO"] = I("0x1000"); m["S_IFLNK"] = I("0xa000")
  m["S_IFREG"] = I("0x8000"); m["S_IFSOCK"] = I("0xc000")
  m["S_ISUID"] = I("0x800"); m["S_ISGID"] = I("0x400"); m["S_ISVTX"] = I("0x200")
  m["S_IRWXU"] = I("0x1c0"); m["S_IRUSR"] = I("0x100"); m["S_IWUSR"] = I("0x80"); m["S_IXUSR"] = I("0x40")
  m["S_IRWXG"] = I("0x38"); m["S_IRGRP"] = I("0x20"); m["S_IWGRP"] = I("0x10"); m["S_IXGRP"] = I("0x8")
  m["S_IRWXO"] = I("0x7"); m["S_IROTH"] = I("0x4"); m["S_IWOTH"] = I("0x2"); m["S_IXOTH"] = I("0x1")
  # wait (macOS values)
  m["WCONTINUED"] = I("0x10"); m["WEXITED"] = I("0x4"); m["WNOHANG"] = I("1")
  m["WNOWAIT"] = I("0x20"); m["WSTOPPED"] = I("0x8"); m["WUNTRACED"] = I("4")
  m["P_ALL"] = I("0"); m["P_PID"] = I("1"); m["P_PGID"] = I("2")
  # sched/clock
  m["SCHED_FIFO"] = I("1"); m["SCHED_OTHER"] = I("0"); m["SCHED_RR"] = I("2")
  m["CLOCK_MONOTONIC"] = I("1"); m["CLOCK_REALTIME"] = I("0")
  m["CLOCK_PROCESS_CPUTIME_ID"] = I("2"); m["CLOCK_THREAD_CPUTIME_ID"] = I("3")
  m["TIMER_ABSTIME"] = I("1")
  # dlopen
  m["RTLD_LAZY"] = I("0x1"); m["RTLD_NOW"] = I("0x2"); m["RTLD_GLOBAL"] = I("0x100"); m["RTLD_LOCAL"] = I("0")
  # glob/fnmatch/ftw
  m["GLOB_APPEND"] = I("2"); m["GLOB_DOOFFS"] = I("8"); m["GLOB_ERR"] = I("1"); m["GLOB_MARK"] = I("4")
  m["GLOB_NOCHECK"] = I("16"); m["GLOB_NOESCAPE"] = I("32"); m["GLOB_NOSORT"] = I("64")
  m["GLOB_NOSPACE"] = I("128"); m["GLOB_NOSYS"] = I("256")
  m["FNM_NOESCAPE"] = I("1"); m["FNM_PATHNAME"] = I("2"); m["FNM_PERIOD"] = I("4"); m["FNM_NOMATCH"] = I("1")
  m["FTW_F"] = I("0"); m["FTW_D"] = I("2"); m["FTW_DNR"] = I("1"); m["FTW_DP"] = I("16")
  m["FTW_NS"] = I("8"); m["FTW_SL"] = I("128"); m["FTW_SLN"] = I("256")
  m["FTW_PHYS"] = I("64"); m["FTW_MOUNT"] = I("32"); m["FTW_CHDIR"] = I("4")
  # locale (macOS values)
  m["LC_ALL"] = I("0"); m["LC_COLLATE"] = I("2"); m["LC_CTYPE"] = I("1"); m["LC_MESSAGES"] = I("4")
  m["LC_MONETARY"] = I("3"); m["LC_NUMERIC"] = I("5"); m["LC_TIME"] = I("6")
  # aio
  m["AIO_ALLDONE"] = I("1"); m["AIO_CANCELED"] = I("2"); m["AIO_NOTCANCELED"] = I("4")
  m["LIO_NOP"] = I("0"); m["LIO_NOWAIT"] = I("1"); m["LIO_READ"] = I("2"); m["LIO_WAIT"] = I("3")
  m["LIO_WRITE"] = I("4")
  # spawn
  m["POSIX_SPAWN_RESETIDS"] = I("1"); m["POSIX_SPAWN_SETPGROUP"] = I("2")
  m["POSIX_SPAWN_SETSCHEDPARAM"] = I("4"); m["POSIX_SPAWN_SETSCHEDULER"] = I("8")
  m["POSIX_SPAWN_SETSIGDEF"] = I("16"); m["POSIX_SPAWN_SETSIGMASK"] = I("32")
  # rlimit
  m["RLIMIT_NOFILE"] = I("8"); m["RLIMIT_STACK"] = I("3")
  # eai (macOS values)
  m["EAI_AGAIN"] = I("2"); m["EAI_BADFLAGS"] = I("1"); m["EAI_FAIL"] = I("3"); m["EAI_FAMILY"] = I("5")
  m["EAI_MEMORY"] = I("6"); m["EAI_NONAME"] = I("8"); m["EAI_OVERFLOW"] = I("14")
  m["EAI_SERVICE"] = I("9"); m["EAI_SOCKTYPE"] = I("10"); m["EAI_SYSTEM"] = I("11")
  # ai (macOS values)
  m["AI_ADDRCONFIG"] = I("0x400"); m["AI_ALL"] = I("0x100"); m["AI_CANONNAME"] = I("0x2")
  m["AI_NUMERICHOST"] = I("0x4"); m["AI_NUMERICSERV"] = I("0x1000"); m["AI_PASSIVE"] = I("0x1")
  m["AI_V4MAPPED"] = I("0x800")
  # ni (macOS values)
  m["NI_DGRAM"] = I("0x10"); m["NI_NAMEREQD"] = I("0x4"); m["NI_NOFQDN"] = I("0x1")
  m["NI_NUMERICHOST"] = I("0x2"); m["NI_NUMERICSERV"] = I("0x8")
  # mmap/memory (macOS values)
  m["MAP_NORESERVE"] = I("0x40"); m["MCL_CURRENT"] = I("1"); m["MCL_FUTURE"] = I("2")
  m["MS_ASYNC"] = I("8"); m["MS_SYNC"] = I("0x10"); m["MS_INVALIDATE"] = I("2")
  m["POSIX_MADV_NORMAL"] = I("0"); m["POSIX_MADV_RANDOM"] = I("1"); m["POSIX_MADV_SEQUENTIAL"] = I("2")
  m["POSIX_MADV_WILLNEED"] = I("3"); m["POSIX_MADV_DONTNEED"] = I("4")
  # errno additions (macOS values)
  m["EALREADY"] = I("37"); m["EMULTIHOP"] = I("94"); m["ENOBUFS"] = I("55"); m["ENOLINK"] = I("97")
  # resolver
  m["HOST_NOT_FOUND"] = I("1"); m["TRY_AGAIN"] = I("2"); m["NO_DATA"] = I("4"); m["NO_RECOVERY"] = I("3")
  # nl_types
  m["NL_CAT_LOCALE"] = I("1"); m["NL_SETD"] = I("1")
  m["CODESET"] = I("0"); m["NOEXPR"] = I("0x50001"); m["YESEXPR"] = I("0x50000")
  m["RADIXCHAR"] = I("0"); m["THOUSEP"] = I("1"); m["CRNCYSTR"] = I("0x4000f")
  m["ALT_DIGITS"] = I("131119")
  m

# Linux import map
const linuxImportMap* = block:
  var m: Table[string, ImportMacroEntry]
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
  m["MAP_FIXED"] = I("16")
  # ipc
  m["IPC_CREAT"] = I("512"); m["IPC_EXCL"] = I("1024"); m["IPC_NOWAIT"] = I("2048")
  m["IPC_RMID"] = I("0"); m["IPC_PRIVATE"] = I("0")
  # misc
  m["FD_SETSIZE"] = I("1024"); m["FD_CLOEXEC"] = I("1")
  m["CLOCKS_PER_SEC"] = I("1000000")
  # pthread
  m["PTHREAD_MUTEX_RECURSIVE"] = I("1"); m["PTHREAD_MUTEX_DEFAULT"] = I("0")
  m["PTHREAD_MUTEX_NORMAL"] = I("0"); m["PTHREAD_MUTEX_ERRORCHECK"] = I("2")
  m["PTHREAD_CREATE_JOINABLE"] = I("0"); m["PTHREAD_CREATE_DETACHED"] = I("1")
  m["PTHREAD_PRIO_NONE"] = I("0"); m["PTHREAD_PRIO_INHERIT"] = I("1"); m["PTHREAD_PRIO_PROTECT"] = I("2")
  m["PTHREAD_EXPLICIT_SCHED"] = I("1"); m["PTHREAD_INHERIT_SCHED"] = I("0")
  m["PTHREAD_SCOPE_SYSTEM"] = I("0"); m["PTHREAD_SCOPE_PROCESS"] = I("1")
  m["PTHREAD_PROCESS_PRIVATE"] = I("0"); m["PTHREAD_PROCESS_SHARED"] = I("1")
  m["PTHREAD_CANCEL_ENABLE"] = I("0"); m["PTHREAD_CANCEL_DISABLE"] = I("1")
  m["PTHREAD_CANCEL_DEFERRED"] = I("0"); m["PTHREAD_CANCEL_ASYNCHRONOUS"] = I("1")
  m["PTHREAD_BARRIER_SERIAL_THREAD"] = I("-1")
  # signal
  m["SIG_BLOCK"] = I("0"); m["SIG_UNBLOCK"] = I("1"); m["SIG_SETMASK"] = I("2")
  m["SIG_DFL"] = I("0"); m["SIG_IGN"] = I("1"); m["SIGPOLL"] = I("29")
  m["SA_NOCLDSTOP"] = I("1"); m["SA_NOCLDWAIT"] = I("2"); m["SA_SIGINFO"] = I("4")
  m["SA_ONSTACK"] = I("0x08000000"); m["SA_RESTART"] = I("0x10000000")
  m["SA_NODEFER"] = I("0x40000000"); m["SA_RESETHAND"] = I("0x80000000")
  m["SIGEV_NONE"] = I("1"); m["SIGEV_SIGNAL"] = I("0"); m["SIGEV_THREAD"] = I("2")
  # fcntl
  m["F_DUPFD"] = I("0"); m["F_GETFD"] = I("1"); m["F_SETFD"] = I("2")
  m["F_GETFL"] = I("3"); m["F_SETFL"] = I("4"); m["F_GETLK"] = I("5")
  m["F_SETLK"] = I("6"); m["F_SETLKW"] = I("7"); m["F_GETOWN"] = I("9"); m["F_SETOWN"] = I("8")
  m["F_RDLCK"] = I("0"); m["F_WRLCK"] = I("1"); m["F_UNLCK"] = I("2")
  # socket
  m["SO_DEBUG"] = I("1"); m["SO_DONTROUTE"] = I("5"); m["SO_OOBINLINE"] = I("10")
  m["SO_RCVLOWAT"] = I("18"); m["SO_SNDLOWAT"] = I("19"); m["SO_BINDTODEVICE"] = I("25")
  m["SO_RCVTIMEO"] = I("20"); m["SO_SNDTIMEO"] = I("21")
  m["MSG_CTRUNC"] = I("8"); m["MSG_DONTROUTE"] = I("4"); m["MSG_EOR"] = I("0x80")
  m["MSG_OOB"] = I("1"); m["MSG_PEEK"] = I("2"); m["MSG_TRUNC"] = I("0x20"); m["MSG_WAITALL"] = I("0x100")
  m["SCM_RIGHTS"] = I("1")
  m["IPPROTO_IPV6"] = I("41")
  m["IPV6_JOIN_GROUP"] = I("20"); m["IPV6_LEAVE_GROUP"] = I("21"); m["IPV6_MULTICAST_HOPS"] = I("18")
  m["IPV6_MULTICAST_IF"] = I("17"); m["IPV6_MULTICAST_LOOP"] = I("19"); m["IPV6_V6ONLY"] = I("26")
  m["IPV6_UNICAST_HOPS"] = I("16")
  m["INADDR_ANY"] = I("0"); m["INADDR_BROADCAST"] = I("0xffffffff"); m["INADDR_LOOPBACK"] = I("0x7f000001")
  m["IPPORT_RESERVED"] = I("1024"); m["IF_NAMESIZE"] = I("16")
  # stat mode
  m["S_IFMT"] = I("0xf000"); m["S_IFBLK"] = I("0x6000"); m["S_IFCHR"] = I("0x2000")
  m["S_IFDIR"] = I("0x4000"); m["S_IFIFO"] = I("0x1000"); m["S_IFLNK"] = I("0xa000")
  m["S_IFREG"] = I("0x8000"); m["S_IFSOCK"] = I("0xc000")
  m["S_ISUID"] = I("0x800"); m["S_ISGID"] = I("0x400"); m["S_ISVTX"] = I("0x200")
  m["S_IRWXU"] = I("0x1c0"); m["S_IRUSR"] = I("0x100"); m["S_IWUSR"] = I("0x80"); m["S_IXUSR"] = I("0x40")
  m["S_IRWXG"] = I("0x38"); m["S_IRGRP"] = I("0x20"); m["S_IWGRP"] = I("0x10"); m["S_IXGRP"] = I("0x8")
  m["S_IRWXO"] = I("0x7"); m["S_IROTH"] = I("0x4"); m["S_IWOTH"] = I("0x2"); m["S_IXOTH"] = I("0x1")
  # wait
  m["WCONTINUED"] = I("8"); m["WEXITED"] = I("4"); m["WNOHANG"] = I("1")
  m["WNOWAIT"] = I("0x01000000"); m["WSTOPPED"] = I("2"); m["WUNTRACED"] = I("4")
  m["P_ALL"] = I("0"); m["P_PID"] = I("1"); m["P_PGID"] = I("2")
  # sched/clock
  m["SCHED_FIFO"] = I("1"); m["SCHED_OTHER"] = I("0"); m["SCHED_RR"] = I("2")
  m["CLOCK_MONOTONIC"] = I("1"); m["CLOCK_REALTIME"] = I("0")
  m["CLOCK_PROCESS_CPUTIME_ID"] = I("2"); m["CLOCK_THREAD_CPUTIME_ID"] = I("3")
  m["TIMER_ABSTIME"] = I("1")
  # dlopen
  m["RTLD_LAZY"] = I("1"); m["RTLD_NOW"] = I("2"); m["RTLD_GLOBAL"] = I("0x100"); m["RTLD_LOCAL"] = I("0")
  # glob/fnmatch/ftw
  m["GLOB_APPEND"] = I("2"); m["GLOB_DOOFFS"] = I("8"); m["GLOB_ERR"] = I("1"); m["GLOB_MARK"] = I("4")
  m["GLOB_NOCHECK"] = I("16"); m["GLOB_NOESCAPE"] = I("32"); m["GLOB_NOSORT"] = I("64")
  m["GLOB_NOSPACE"] = I("128"); m["GLOB_NOSYS"] = I("256")
  m["FNM_NOESCAPE"] = I("1"); m["FNM_PATHNAME"] = I("2"); m["FNM_PERIOD"] = I("4"); m["FNM_NOMATCH"] = I("1")
  m["FTW_F"] = I("0"); m["FTW_D"] = I("2"); m["FTW_DNR"] = I("1"); m["FTW_DP"] = I("16")
  m["FTW_NS"] = I("8"); m["FTW_SL"] = I("128"); m["FTW_SLN"] = I("256")
  m["FTW_PHYS"] = I("64"); m["FTW_MOUNT"] = I("32"); m["FTW_CHDIR"] = I("4")
  # locale
  m["LC_ALL"] = I("6"); m["LC_COLLATE"] = I("3"); m["LC_CTYPE"] = I("0"); m["LC_MESSAGES"] = I("5")
  m["LC_MONETARY"] = I("4"); m["LC_NUMERIC"] = I("1"); m["LC_TIME"] = I("2")
  # aio
  m["AIO_ALLDONE"] = I("1"); m["AIO_CANCELED"] = I("2"); m["AIO_NOTCANCELED"] = I("4")
  m["LIO_NOP"] = I("0"); m["LIO_NOWAIT"] = I("1"); m["LIO_READ"] = I("2"); m["LIO_WAIT"] = I("3")
  m["LIO_WRITE"] = I("4")
  # spawn
  m["POSIX_SPAWN_RESETIDS"] = I("1"); m["POSIX_SPAWN_SETPGROUP"] = I("2")
  m["POSIX_SPAWN_SETSCHEDPARAM"] = I("4"); m["POSIX_SPAWN_SETSCHEDULER"] = I("8")
  m["POSIX_SPAWN_SETSIGDEF"] = I("16"); m["POSIX_SPAWN_SETSIGMASK"] = I("32")
  # rlimit
  m["RLIMIT_NOFILE"] = I("7"); m["RLIMIT_STACK"] = I("3")
  # eai
  m["EAI_AGAIN"] = I("-3"); m["EAI_BADFLAGS"] = I("-1"); m["EAI_FAIL"] = I("-4"); m["EAI_FAMILY"] = I("-6")
  m["EAI_MEMORY"] = I("-10"); m["EAI_NONAME"] = I("-2"); m["EAI_OVERFLOW"] = I("-12")
  m["EAI_SERVICE"] = I("-8"); m["EAI_SOCKTYPE"] = I("-7"); m["EAI_SYSTEM"] = I("-11")
  # ai
  m["AI_ADDRCONFIG"] = I("0x20"); m["AI_ALL"] = I("0x10"); m["AI_CANONNAME"] = I("2")
  m["AI_NUMERICHOST"] = I("4"); m["AI_NUMERICSERV"] = I("0x400"); m["AI_PASSIVE"] = I("1")
  m["AI_V4MAPPED"] = I("8")
  # ni
  m["NI_DGRAM"] = I("0x10"); m["NI_NAMEREQD"] = I("8"); m["NI_NOFQDN"] = I("4")
  m["NI_NUMERICHOST"] = I("1"); m["NI_NUMERICSERV"] = I("2")
  # mmap/memory
  m["MAP_NORESERVE"] = I("0x4000"); m["MCL_CURRENT"] = I("1"); m["MCL_FUTURE"] = I("2")
  m["MS_ASYNC"] = I("1"); m["MS_SYNC"] = I("4"); m["MS_INVALIDATE"] = I("2")
  m["POSIX_MADV_NORMAL"] = I("0"); m["POSIX_MADV_RANDOM"] = I("1"); m["POSIX_MADV_SEQUENTIAL"] = I("2")
  m["POSIX_MADV_WILLNEED"] = I("3"); m["POSIX_MADV_DONTNEED"] = I("4")
  # errno additions
  m["EALREADY"] = I("114"); m["EMULTIHOP"] = I("72"); m["ENOBUFS"] = I("105"); m["ENOLINK"] = I("67")
  # resolver
  m["HOST_NOT_FOUND"] = I("1"); m["TRY_AGAIN"] = I("2"); m["NO_DATA"] = I("4"); m["NO_RECOVERY"] = I("3")
  # nl_types
  m["NL_CAT_LOCALE"] = I("1"); m["NL_SETD"] = I("1")
  m["CODESET"] = I("14"); m["NOEXPR"] = I("0x50001"); m["YESEXPR"] = I("0x50000")
  m["RADIXCHAR"] = I("0x10000"); m["THOUSEP"] = I("0x10001"); m["CRNCYSTR"] = I("0x4000f")
  m["ALT_DIGITS"] = I("131119")
  m

# Windows import map
const windowsImportMap* = block:
  var m: Table[string, ImportMacroEntry]
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
  let m = maps[platform.ord]
  if m.contains(name):
    result = m[name]
  else:
    result = ImportMacroEntry(kind: imkReplace, resolvedName: name)
# Verifies the hardcoded POSIX ABI declarations in std/posix against the
# platform's real C headers. At run time this test generates a C file full of
# `_Static_assert`s вЂ” one per transcribed constant, struct size and field
# offset, with the expected value read from the LIVE Nim declarations вЂ” and
# compiles it with the system C compiler. A wrong transcription therefore
# fails this test loudly on the platform that can see it, instead of silently
# corrupting data at run time. This is the safety net that makes header-free
# bindings maintainable at all.

import std/[assertions, syncio, os, dirs]

when defined(posix):
  import std/posix/posix
  when defined(linux):
    from std/posix/epoll import EpollData, EpollEvent

  var src = ""

  proc ck(expr: string; v: int64) =
    src.add "_Static_assert((long long)("
    src.add expr
    src.add ") == "
    src.add $v
    src.add "LL, \""
    src.add expr
    src.add "\");\n"

  # Field offsets are computed from a pointer overlaid on a zeroed scratch
  # buffer (`default(T)` is unusable here: several structs carry non-nil
  # `pointer` fields, which nimony refuses to default).
  var scratch: array[1024, byte]

  proc off[T](base: ptr T; field: pointer): int64 =
    int64(cast[int](field) - cast[int](base))

  proc main =
    src.add "#define _GNU_SOURCE\n"
    src.add "#include <stddef.h>\n"
    src.add "#include <errno.h>\n"
    src.add "#include <fcntl.h>\n"
    src.add "#include <time.h>\n"
    src.add "#include <signal.h>\n"
    src.add "#include <stdio.h>\n"
    src.add "#include <unistd.h>\n"
    src.add "#include <dirent.h>\n"
    src.add "#include <dlfcn.h>\n"
    src.add "#include <pthread.h>\n"
    src.add "#include <sys/mman.h>\n"
    src.add "#include <sys/socket.h>\n"
    src.add "#include <sys/stat.h>\n"
    src.add "#include <sys/types.h>\n"
    src.add "#include <sys/wait.h>\n"
    src.add "#include <netinet/in.h>\n"
    when defined(linux):
      src.add "#include <sys/epoll.h>\n"

    # ---- errno ----
    ck("E2BIG", int64(E2BIG)); ck("EACCES", int64(EACCES))
    ck("EADDRINUSE", int64(EADDRINUSE)); ck("EADDRNOTAVAIL", int64(EADDRNOTAVAIL))
    ck("EAFNOSUPPORT", int64(EAFNOSUPPORT)); ck("EAGAIN", int64(EAGAIN))
    ck("EALREADY", int64(EALREADY)); ck("EBADF", int64(EBADF))
    ck("EBADMSG", int64(EBADMSG)); ck("EBUSY", int64(EBUSY))
    ck("ECANCELED", int64(ECANCELED)); ck("ECHILD", int64(ECHILD))
    ck("ECONNABORTED", int64(ECONNABORTED)); ck("ECONNREFUSED", int64(ECONNREFUSED))
    ck("ECONNRESET", int64(ECONNRESET)); ck("EDEADLK", int64(EDEADLK))
    ck("EDESTADDRREQ", int64(EDESTADDRREQ)); ck("EDOM", int64(EDOM))
    ck("EEXIST", int64(EEXIST)); ck("EFAULT", int64(EFAULT))
    ck("EFBIG", int64(EFBIG)); ck("EHOSTUNREACH", int64(EHOSTUNREACH))
    ck("EIDRM", int64(EIDRM)); ck("EILSEQ", int64(EILSEQ))
    ck("EINPROGRESS", int64(EINPROGRESS)); ck("EINTR", int64(EINTR))
    ck("EINVAL", int64(EINVAL)); ck("EIO", int64(EIO))
    ck("EISCONN", int64(EISCONN)); ck("EISDIR", int64(EISDIR))
    ck("ELOOP", int64(ELOOP)); ck("EMFILE", int64(EMFILE))
    ck("EMLINK", int64(EMLINK)); ck("EMSGSIZE", int64(EMSGSIZE))
    ck("ENAMETOOLONG", int64(ENAMETOOLONG)); ck("ENETDOWN", int64(ENETDOWN))
    ck("ENETRESET", int64(ENETRESET)); ck("ENETUNREACH", int64(ENETUNREACH))
    ck("ENFILE", int64(ENFILE)); ck("ENOBUFS", int64(ENOBUFS))
    ck("ENODATA", int64(ENODATA)); ck("ENODEV", int64(ENODEV))
    ck("ENOENT", int64(ENOENT)); ck("ENOEXEC", int64(ENOEXEC))
    ck("ENOLCK", int64(ENOLCK)); ck("ENOMEM", int64(ENOMEM))
    ck("ENOMSG", int64(ENOMSG)); ck("ENOPROTOOPT", int64(ENOPROTOOPT))
    ck("ENOSPC", int64(ENOSPC)); ck("ENOSR", int64(ENOSR))
    ck("ENOSTR", int64(ENOSTR)); ck("ENOSYS", int64(ENOSYS))
    ck("ENOTCONN", int64(ENOTCONN)); ck("ENOTDIR", int64(ENOTDIR))
    ck("ENOTEMPTY", int64(ENOTEMPTY)); ck("ENOTSOCK", int64(ENOTSOCK))
    ck("ENOTSUP", int64(ENOTSUP)); ck("ENOTTY", int64(ENOTTY))
    ck("ENXIO", int64(ENXIO)); ck("EOPNOTSUPP", int64(EOPNOTSUPP))
    ck("EOVERFLOW", int64(EOVERFLOW)); ck("EPERM", int64(EPERM))
    ck("EPIPE", int64(EPIPE)); ck("EPROTO", int64(EPROTO))
    ck("EPROTONOSUPPORT", int64(EPROTONOSUPPORT)); ck("EPROTOTYPE", int64(EPROTOTYPE))
    ck("ERANGE", int64(ERANGE)); ck("EROFS", int64(EROFS))
    ck("ESPIPE", int64(ESPIPE)); ck("ESRCH", int64(ESRCH))
    ck("ETIME", int64(ETIME)); ck("ETIMEDOUT", int64(ETIMEDOUT))
    ck("ETXTBSY", int64(ETXTBSY)); ck("EWOULDBLOCK", int64(EWOULDBLOCK))
    ck("EXDEV", int64(EXDEV))

    # ---- fcntl / open flags ----
    ck("O_RDONLY", int64(O_RDONLY)); ck("O_WRONLY", int64(O_WRONLY))
    ck("O_RDWR", int64(O_RDWR)); ck("O_CREAT", int64(O_CREAT))
    ck("O_TRUNC", int64(O_TRUNC)); ck("O_APPEND", int64(O_APPEND))
    ck("O_NONBLOCK", int64(O_NONBLOCK)); ck("O_CLOEXEC", int64(O_CLOEXEC))
    ck("F_GETFL", int64(F_GETFL)); ck("F_SETFL", int64(F_SETFL))

    # ---- mmap ----
    ck("PROT_READ", int64(PROT_READ)); ck("PROT_WRITE", int64(PROT_WRITE))
    ck("MAP_SHARED", int64(MAP_SHARED)); ck("MAP_PRIVATE", int64(MAP_PRIVATE))
    ck("MAP_ANONYMOUS", int64(MAP_ANONYMOUS))
    when defined(linux):
      ck("MAP_POPULATE", int64(MAP_POPULATE))

    # ---- sockets ----
    ck("AF_UNSPEC", int64(AF_UNSPEC)); ck("AF_UNIX", int64(AF_UNIX))
    ck("AF_INET", int64(AF_INET)); ck("AF_INET6", int64(AF_INET6))
    ck("SOCK_STREAM", int64(SOCK_STREAM)); ck("SOCK_DGRAM", int64(SOCK_DGRAM))
    ck("SOCK_RAW", int64(SOCK_RAW)); ck("SOCK_SEQPACKET", int64(SOCK_SEQPACKET))
    ck("SOL_SOCKET", int64(SOL_SOCKET)); ck("SO_REUSEADDR", int64(SO_REUSEADDR))
    ck("IPPROTO_IP", int64(IPPROTO_IP)); ck("IPPROTO_IPV6", int64(IPPROTO_IPV6))
    ck("IPPROTO_ICMP", int64(IPPROTO_ICMP)); ck("IPPROTO_ICMPV6", int64(IPPROTO_ICMPV6))
    ck("IPPROTO_RAW", int64(IPPROTO_RAW)); ck("IPPROTO_TCP", int64(IPPROTO_TCP))
    ck("IPPROTO_UDP", int64(IPPROTO_UDP)); ck("INADDR_ANY", int64(INADDR_ANY))

    # ---- clocks / signals / wait / seek / stdio / dlfcn / pthread / sysconf ----
    ck("CLOCK_REALTIME", int64(CLOCK_REALTIME)); ck("CLOCK_MONOTONIC", int64(CLOCK_MONOTONIC))
    ck("SIGABRT", int64(SIGABRT)); ck("SIGCONT", int64(SIGCONT))
    ck("SIGKILL", int64(SIGKILL)); ck("SIGSTOP", int64(SIGSTOP))
    ck("SIGTERM", int64(SIGTERM))
    ck("WNOHANG", int64(WNOHANG)); ck("WCONTINUED", int64(WCONTINUED))
    ck("SEEK_SET", int64(SEEK_SET)); ck("SEEK_CUR", int64(SEEK_CUR))
    ck("SEEK_END", int64(SEEK_END))
    ck("_IOFBF", int64(IOFBF))
    ck("RTLD_NOW", int64(RTLD_NOW)); ck("RTLD_GLOBAL", int64(RTLD_GLOBAL))
    ck("PTHREAD_MUTEX_RECURSIVE", int64(PTHREAD_MUTEX_RECURSIVE))
    ck("_SC_NPROCESSORS_ONLN", int64(SC_NPROCESSORS_ONLN))
    ck("S_IRUSR", int64(S_IRUSR)); ck("S_IWUSR", int64(S_IWUSR))

    # ---- dirent d_type ----
    ck("DT_UNKNOWN", int64(DT_UNKNOWN)); ck("DT_FIFO", int64(DT_FIFO))
    ck("DT_CHR", int64(DT_CHR)); ck("DT_DIR", int64(DT_DIR))
    ck("DT_BLK", int64(DT_BLK)); ck("DT_REG", int64(DT_REG))
    ck("DT_LNK", int64(DT_LNK)); ck("DT_SOCK", int64(DT_SOCK))

    # ---- scalar typedef widths ----
    ck("sizeof(mode_t)", int64(sizeof(Mode)))
    ck("sizeof(off_t)", int64(sizeof(Off)))
    ck("sizeof(dev_t)", int64(sizeof(Dev)))
    ck("sizeof(ino_t)", int64(sizeof(Ino)))
    ck("sizeof(time_t)", int64(sizeof(Time)))
    ck("sizeof(pid_t)", int64(sizeof(Pid)))
    ck("sizeof(socklen_t)", int64(sizeof(SockLen)))
    ck("sizeof(sa_family_t)", int64(sizeof(TSa_Family)))

    # ---- struct timespec ----
    let ts = cast[ptr Timespec](addr scratch[0])
    ck("sizeof(struct timespec)", int64(sizeof(Timespec)))
    ck("offsetof(struct timespec, tv_sec)", off(ts, addr ts.tv_sec))
    ck("offsetof(struct timespec, tv_nsec)", off(ts, addr ts.tv_nsec))

    # ---- struct iovec ----
    let iov = cast[ptr IOVec](addr scratch[0])
    ck("sizeof(struct iovec)", int64(sizeof(IOVec)))
    ck("offsetof(struct iovec, iov_base)", off(iov, addr iov.iov_base))
    ck("offsetof(struct iovec, iov_len)", off(iov, addr iov.iov_len))

    # ---- struct stat (exported fields anchor the layout; total size pins
    #      the private padding) ----
    let st = cast[ptr Stat](addr scratch[0])
    ck("sizeof(struct stat)", int64(sizeof(Stat)))
    ck("offsetof(struct stat, st_dev)", off(st, addr st.st_dev))
    ck("offsetof(struct stat, st_ino)", off(st, addr st.st_ino))
    ck("offsetof(struct stat, st_mode)", off(st, addr st.st_mode))
    ck("offsetof(struct stat, st_size)", off(st, addr st.st_size))
    when defined(osx):
      ck("offsetof(struct stat, st_mtimespec)", off(st, addr st.st_mtim))
    else:
      ck("offsetof(struct stat, st_mtim)", off(st, addr st.st_mtim))

    # ---- struct sockaddr / struct msghdr ----
    let sa = cast[ptr SockAddr](addr scratch[0])
    ck("offsetof(struct sockaddr, sa_family)", off(sa, addr sa.sa_family))
    ck("offsetof(struct sockaddr, sa_data)", off(sa, addr sa.sa_data))
    let mh = cast[ptr Tmsghdr](addr scratch[0])
    ck("sizeof(struct msghdr)", int64(sizeof(Tmsghdr)))
    ck("offsetof(struct msghdr, msg_name)", off(mh, addr mh.msg_name))
    ck("offsetof(struct msghdr, msg_namelen)", off(mh, addr mh.msg_namelen))
    ck("offsetof(struct msghdr, msg_iov)", off(mh, addr mh.msg_iov))
    ck("offsetof(struct msghdr, msg_iovlen)", off(mh, addr mh.msg_iovlen))
    ck("sizeof(((struct msghdr*)0)->msg_iovlen)", int64(sizeof(mh.msg_iovlen)))
    ck("offsetof(struct msghdr, msg_control)", off(mh, addr mh.msg_control))
    ck("offsetof(struct msghdr, msg_controllen)", off(mh, addr mh.msg_controllen))
    ck("offsetof(struct msghdr, msg_flags)", off(mh, addr mh.msg_flags))

    # ---- macOS struct dirent (our Dirent overlays libSystem's records) ----
    when defined(osx):
      let de = cast[ptr Dirent](addr scratch[0])
      ck("offsetof(struct dirent, d_type)", off(de, addr de.d_type))
      ck("offsetof(struct dirent, d_name)", off(de, addr de.d_name))
      ck("sizeof(((struct dirent*)0)->d_name)", int64(sizeof(de.d_name)))

    # ---- Linux struct epoll_event (packed on amd64 only) ----
    when defined(linux):
      let ev = cast[ptr EpollEvent](addr scratch[0])
      ck("sizeof(struct epoll_event)", int64(sizeof(EpollEvent)))
      ck("offsetof(struct epoll_event, events)", off(ev, addr ev.events))
      ck("offsetof(struct epoll_event, data)", off(ev, addr ev.data))
      ck("sizeof(epoll_data_t)", int64(sizeof(EpollData)))

    # ---- pthread blob sizes (the opaque arrays transcribed in
    #      private/syslocks.nim and rawthreads.nim must be at least as large
    #      and as aligned as the real structs; the bounds here mirror those
    #      declarations, so a platform whose real struct outgrows the blob
    #      fails right here) ----
    const mutexBlob =
      when defined(osx): 64
      elif defined(arm64): 48  # aarch64 glibc; x86_64 is 40
      elif defined(amd64): 40
      else: 24
    ck("sizeof(pthread_mutex_t) <= " & $mutexBlob, 1)
    ck("sizeof(pthread_cond_t) <= 48", 1)
    ck("sizeof(pthread_mutexattr_t) <= 16", 1)
    ck("sizeof(pthread_attr_t) <= 64", 1)
    ck("sizeof(pthread_t)", int64(sizeof(pointer)))

    src.add "int main(void) { return 0; }\n"

    let cfile = getTempDir() & "nimony_posixabi_probe.c"
    try:
      writeFile(cfile, src)
    except:
      quit "cannot write " & cfile
    let code = execShellCmd("cc -fsyntax-only " & cfile)
    if code != 0:
      # The compiler already printed which _Static_assert failed; keep the
      # generated file around for inspection.
      quit "posix ABI probe failed; see " & cfile
    discard tryRemoveFile(path(cfile))

  main()

echo "ok"

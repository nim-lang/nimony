# Private implementation included by std/posix/posix; not a standalone module.

# Header-free ABI constants; checked by tests/nimony/stdlib/tposixabi.nim.
# Linux values are shared by the supported amd64/arm64/i386 ABIs and by
# glibc and musl. Architectures with divergent errno/socket tables are
# rejected by types.nim, not silently assigned these values.
# Only declare symbols consumed in this repository; add their header probes
# to tposixabi.nim when extending this table.

# <errno.h>
const
  EPERM* = cint(1)
  ENOENT* = cint(2)
  ESRCH* = cint(3)
  EINTR* = cint(4)
  EIO* = cint(5)
  ENXIO* = cint(6)
  E2BIG* = cint(7)
  ENOEXEC* = cint(8)
  EBADF* = cint(9)
  ECHILD* = cint(10)
  EAGAIN* = cint(11)
  ENOMEM* = cint(12)
  EACCES* = cint(13)
  EFAULT* = cint(14)
  EBUSY* = cint(16)
  EEXIST* = cint(17)
  EXDEV* = cint(18)
  ENODEV* = cint(19)
  ENOTDIR* = cint(20)
  EISDIR* = cint(21)
  EINVAL* = cint(22)
  ENFILE* = cint(23)
  EMFILE* = cint(24)
  ENOTTY* = cint(25)
  ETXTBSY* = cint(26)
  EFBIG* = cint(27)
  ENOSPC* = cint(28)
  ESPIPE* = cint(29)
  EROFS* = cint(30)
  EMLINK* = cint(31)
  EPIPE* = cint(32)
  EDOM* = cint(33)
  ERANGE* = cint(34)
  EDEADLK* = cint(35)
  ENAMETOOLONG* = cint(36)
  ENOLCK* = cint(37)
  ENOSYS* = cint(38)
  ENOTEMPTY* = cint(39)
  ELOOP* = cint(40)
  EWOULDBLOCK* = EAGAIN
  ENOMSG* = cint(42)
  EIDRM* = cint(43)
  ENOSTR* = cint(60)
  ENODATA* = cint(61)
  ETIME* = cint(62)
  ENOSR* = cint(63)
  EPROTO* = cint(71)
  EBADMSG* = cint(74)
  EOVERFLOW* = cint(75)
  EILSEQ* = cint(84)
  ENOTSOCK* = cint(88)
  EDESTADDRREQ* = cint(89)
  EMSGSIZE* = cint(90)
  EPROTOTYPE* = cint(91)
  ENOPROTOOPT* = cint(92)
  EPROTONOSUPPORT* = cint(93)
  EOPNOTSUPP* = cint(95)
  ENOTSUP* = EOPNOTSUPP
  EAFNOSUPPORT* = cint(97)
  EADDRINUSE* = cint(98)
  EADDRNOTAVAIL* = cint(99)
  ENETDOWN* = cint(100)
  ENETUNREACH* = cint(101)
  ENETRESET* = cint(102)
  ECONNABORTED* = cint(103)
  ECONNRESET* = cint(104)
  ENOBUFS* = cint(105)
  EISCONN* = cint(106)
  ENOTCONN* = cint(107)
  ETIMEDOUT* = cint(110)
  ECONNREFUSED* = cint(111)
  EHOSTUNREACH* = cint(113)
  EALREADY* = cint(114)
  EINPROGRESS* = cint(115)
  ECANCELED* = cint(125)

# <fcntl.h> (asm-generic; the four flags arm64 overrides — O_DIRECTORY,
# O_NOFOLLOW, O_DIRECT, O_LARGEFILE — are not declared here; the only
# consumed one, O_DIRECTORY, is defined in each architecture directory)
const
  O_RDONLY* = cint(0)
  O_WRONLY* = cint(1)
  O_RDWR* = cint(2)
  O_CREAT* = cint(0o100)
  O_TRUNC* = cint(0o1000)
  O_APPEND* = cint(0o2000)
  O_NONBLOCK* = cint(0o4000)
  O_CLOEXEC* = cint(0o2000000)
  F_GETFL* = cint(3)
  F_SETFL* = cint(4)

# <dlfcn.h>
const
  RTLD_NOW* = cint(2)
  RTLD_GLOBAL* = cint(0x100)

# <pthread.h>
const
  PTHREAD_MUTEX_RECURSIVE* = cint(1)

# <signal.h>
const
  SIGABRT* = cint(6)
  SIGKILL* = cint(9)
  SIGTERM* = cint(15)
  SIGCONT* = cint(18)
  SIGSTOP* = cint(19)

# <sys/mman.h>
const
  PROT_READ* = cint(1)
  PROT_WRITE* = cint(2)
  MAP_SHARED* = cint(1)
  MAP_PRIVATE* = cint(2)
  MAP_ANONYMOUS* = cint(0x20)
  MAP_POPULATE* = cint(0x8000)

# <sys/socket.h> / <netinet/in.h>
const
  AF_UNSPEC* = cint(0)
  AF_UNIX* = cint(1)
  AF_INET* = cint(2)
  AF_INET6* = cint(10)
  SOCK_STREAM* = cint(1)
  SOCK_DGRAM* = cint(2)
  SOCK_RAW* = cint(3)
  SOCK_SEQPACKET* = cint(5)
  SOL_SOCKET* = cint(1)
  SO_REUSEADDR* = cint(2)
  IPPROTO_IP* = cint(0)
  IPPROTO_ICMP* = cint(1)
  IPPROTO_TCP* = cint(6)
  IPPROTO_UDP* = cint(17)
  IPPROTO_IPV6* = cint(41)
  IPPROTO_ICMPV6* = cint(58)
  IPPROTO_RAW* = cint(255)
  INADDR_ANY* = 0'u32

# <sys/wait.h>
const
  WNOHANG* = cint(1)
  WCONTINUED* = cint(8)

# <time.h>
const
  CLOCK_REALTIME* = cint(0)
  CLOCK_MONOTONIC* = cint(1)
  TIMER_ABSTIME* = cint(1)

# <unistd.h>
const
  SEEK_SET* = cint(0)
  SEEK_CUR* = cint(1)
  SEEK_END* = cint(2)
  SC_NPROCESSORS_ONLN* = cint(84)  ## _SC_NPROCESSORS_ONLN (glibc; musl uses
                                   ## the same value)

# <stdio.h>
const
  IOFBF* = cint(0)  ## _IOFBF

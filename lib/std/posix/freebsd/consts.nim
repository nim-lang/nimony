# Private implementation included by std/posix/posix; not a standalone module.

# FreeBSD (values from the FreeBSD 15 headers; identical on amd64 and
# arm64). posix_other.nim rejects every other OS before this file is
# reached. FreeBSD has no ENODATA, ENOSR, ENOSTR or ETIME (the STREAMS and
# xattr errnos), and EOPNOTSUPP is an alias of ENOTSUP.

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
  EDEADLK* = cint(11)
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
  EAGAIN* = cint(35)
  EWOULDBLOCK* = EAGAIN
  EINPROGRESS* = cint(36)
  EALREADY* = cint(37)
  ENOTSOCK* = cint(38)
  EDESTADDRREQ* = cint(39)
  EMSGSIZE* = cint(40)
  EPROTOTYPE* = cint(41)
  ENOPROTOOPT* = cint(42)
  EPROTONOSUPPORT* = cint(43)
  ENOTSUP* = cint(45)
  EAFNOSUPPORT* = cint(47)
  EADDRINUSE* = cint(48)
  EADDRNOTAVAIL* = cint(49)
  ENETDOWN* = cint(50)
  ENETUNREACH* = cint(51)
  ENETRESET* = cint(52)
  ECONNABORTED* = cint(53)
  ECONNRESET* = cint(54)
  ENOBUFS* = cint(55)
  EISCONN* = cint(56)
  ENOTCONN* = cint(57)
  ETIMEDOUT* = cint(60)
  ECONNREFUSED* = cint(61)
  ELOOP* = cint(62)
  ENAMETOOLONG* = cint(63)
  EHOSTUNREACH* = cint(65)
  ENOTEMPTY* = cint(66)
  ENOLCK* = cint(77)
  ENOSYS* = cint(78)
  EOVERFLOW* = cint(84)
  ECANCELED* = cint(85)
  EIDRM* = cint(82)
  ENOMSG* = cint(83)
  EILSEQ* = cint(86)
  EBADMSG* = cint(89)
  EPROTO* = cint(92)
  EOPNOTSUPP* = ENOTSUP

# <fcntl.h>
const
  O_RDONLY* = cint(0)
  O_WRONLY* = cint(1)
  O_RDWR* = cint(2)
  O_NONBLOCK* = cint(0x4)
  O_APPEND* = cint(0x8)
  O_CREAT* = cint(0x200)
  O_TRUNC* = cint(0x400)
  O_CLOEXEC* = cint(0x100000)
  F_GETFL* = cint(3)
  F_SETFL* = cint(4)

# <dlfcn.h>
const
  RTLD_NOW* = cint(0x2)
  RTLD_GLOBAL* = cint(0x100)

# <pthread.h>
const
  PTHREAD_MUTEX_RECURSIVE* = cint(2)

# <signal.h>
const
  SIGABRT* = cint(6)
  SIGKILL* = cint(9)
  SIGTERM* = cint(15)
  SIGSTOP* = cint(17)
  SIGCONT* = cint(19)

# <sys/mman.h>
const
  PROT_READ* = cint(1)
  PROT_WRITE* = cint(2)
  MAP_SHARED* = cint(1)
  MAP_PRIVATE* = cint(2)
  MAP_ANONYMOUS* = cint(0x1000)  ## MAP_ANON
  MAP_POPULATE* = cint(0)  ## Linux-only prefault hint; harmless no-op flag
                           ## value elsewhere

# <sys/socket.h> / <netinet/in.h>
const
  AF_UNSPEC* = cint(0)
  AF_UNIX* = cint(1)
  AF_INET* = cint(2)
  AF_INET6* = cint(28)
  SOCK_STREAM* = cint(1)
  SOCK_DGRAM* = cint(2)
  SOCK_RAW* = cint(3)
  SOCK_SEQPACKET* = cint(5)
  SOL_SOCKET* = cint(0xffff)
  SO_REUSEADDR* = cint(0x4)
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
  WCONTINUED* = cint(4)

# <time.h>
const
  CLOCK_REALTIME* = cint(0)
  CLOCK_MONOTONIC* = cint(4)
  TIMER_ABSTIME* = cint(1)

# <unistd.h>
const
  SEEK_SET* = cint(0)
  SEEK_CUR* = cint(1)
  SEEK_END* = cint(2)
  SC_NPROCESSORS_ONLN* = cint(58)  ## _SC_NPROCESSORS_ONLN

# <stdio.h>
const
  IOFBF* = cint(0)  ## _IOFBF

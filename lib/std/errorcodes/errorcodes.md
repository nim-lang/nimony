| Nim enum                        |  Posix  |   HTTP  | Windows API |  description |
|---------------------------------|---------|---------|-------------|--------------|
| Success                         |   0'i32 |      200, 201, 202 | 0'i32 | Operation completed successfully. |
| OverflowError                   |     EOVERFLOW  |      | 534'i32 | Integer overflow or underflow error. |
| Failure                         |      |      500 | 1124'i32 | General failure, unknown error, etc. |
| BugError                        |      |       | | Programming bug detected. |
| IndexError                      |      |       | 1413'i32 | Array index out of bounds. |
| RangeError                      |     ERANGE, EDOM  |  416 | 1745'i32, 1781'i32, 5913'i32, 6604'i32, 8322'i32, 14090'i32, 14091'i32, 15038'i32 | Range check error. |
| OverlapError                    |    |   |  | Source and destination memory overlaps. |
| SyntaxError                     |        |    422 | 123'i32 | A general parsing error. |
| OutOfMemError                   |     ENOMEM  |   | 8'i32, 13859'i32 | Not enough memory left. |
| DiskFullError                   |     ENOSPC  |  507 | 39'i32, 1632'i32, 4314'i32 | No space left on device. |
| StackOverflow                   |             |      | 1001'i32 | No stack space left. |
| IOError                         |     EIO     |   | 1117'i32 | I/O error. |
| ValueError                      |     EINVAL, EBADMSG, EILSEQ, ENOMSG, EDESTADDRREQ  |  | 1639'i32, 10022'i32 | Invalid argument. Or: Bad/missing message. |
| KeyError                        |         |  | 1010'i32, 1018'i32, 1169'i32, 1409'i32, 9017'i32, 14007'i32, 14075'i32 | Invalid key. |
| EndOfStreamError                |         |   | 38'i32, 1100'i32, 1129'i32 | End of stream/file reached. |
| SkipError                       |         |  | 625'i32 | Skip to next item. |
| FullError                       |     E2BIG, ENOBUFS  |  | 61'i32, 277'i32, 794'i32, 1502'i32, 4322'i32, 10055'i32, 15083'i32 | No space left in the buffer. Or: Argument list too long. |
| EmptyError                      |     ENODATA |  | 4306'i32 | No message is available. |
| BusyError                       |     EBUSY, ETXTBSY | 429 | 54'i32, 1723'i32, 5909'i32, 7024'i32, 8015'i32, 8206'i32, 8438'i32 | Device is busy. Too many requests. |
| DeadResource                    |     ECHILD, EOWNERDEAD |  |  | Dead thread/owner/child. |
| ResourceExhaustedError          |     ENOLCK   |     | 155'i32, 89'i32 | Thread/process/etc creation failed. |
| DescriptorExhaustedError        |     ENFILE   | | 4'i32, 36'i32 | Too many files open in system. |
| PermissionDenied                |     EACCES, EPERM  |  403, 401, 407, 405, 406 | 5'i32, 10013'i32 | Permission denied. |
| RetryError                      |     EAGAIN, EWOULDBLOCK |  | 1237'i32 | Resource unavailable, try again. |
| TimeoutError                    |     ETIMEDOUT, ETIME | 408, 504 | 258'i32, 1053'i32, 1121'i32, 1460'i32, 7040'i32, 10060'i32 |  Connection timed out. |
| InterruptedError                |     EINTR |  | 10004'i32 | Interrupted function. |
| DeadlockError                   |     EDEADLK | 508 | 1131'i32 | Resource deadlock would occur. |
| LockedError                     |   |  | 33'i32, 108'i32, 212'i32, 717'i32, 1055'i32, 1440'i32, 5960'i32 | Resource is locked. |
| FormatMismatch                  |   |  |  | Source and destination have incompatible formats. |
| AlreadyConnected                |     EADDRINUSE, EISCONN | | 10048'i32, 10056'i32 | Address in use. Socket is connected. |
| AddressNotAvailable             |     EADDRNOTAVAIL | | 55'i32, 64'i32 | Address not available. |
| AddressFamilyUnsupported        |     EAFNOSUPPORT  | | 1763'i32 | Address family not supported. |
| BadOperation                    |     EOPNOTSUPP, ENOTSUP, ENOSYS, EPROTONOSUPPORT, ENOTTY, ESPIPE, EISDIR, ENOTEMPTY  | 400, 415 | 50'i32, 120'i32, 334'i32, 335'i32, 336'i32, 632'i32, 1764'i32, 6834'i32 | Operation not supported. Bad Request. |
| AbortedOperation                |     ECANCELED, ECONNABORTED, ENETRESET |   | 10053'i32, 995'i32, 1235'i32, 1818'i32 |  Operation canceled. Connection aborted. |
| UnimplementedOperation          |      | 501 |  | Operation is not implemented. |
| AlreadyInProgress               |     EALREADY, EINPROGRESS | | 1791'i32, 4056'i32, 10036'i32, 10037'i32 | Operation already in progress. |
| NameTooLong                     |     ENAMETOOLONG | 414, 431 | 111'i32, 206'i32, 8348'i32, 10063'i32, 15121'i32 | Path/Filename/URL too long. |
| NameExists                      |     EEXIST |  | 183'i32, 1250'i32 | Name file/directory already exists. |
| NameNotFound                    |     ENOENT, EIDRM, ENODEV, ENOTDIR | 404 | ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND | No such file or directory or device. |
| ContentTooLong                  |     EFBIG, EMSGSIZE | 413 | 1743'i32, 8349'i32 | File/content too large. |
| BadDescriptor                   |     EPIPE, EBADF, EMFILE, ENOSTR, ENOTSOCK, ENOSR, ENXIO, ESRCH | | 6'i32 | Bad file descriptor/pipe/process/etc. |
| BadExecutable                   |     ENOEXEC |  | 11'i32 | Executable file format error. |
| BadLink                         |     ELOOP, EMLINK, EXDEV | 421 | 1142'i32 | Too many levels of symbolic links. Too many links. Cross-device link. HTTP: Misdirected request. |
| BadProtocol                     |     EPROTOTYPE, ENOPROTOOPT | 505 | 10041'i32, 10042'i32, 10043'i32, 10044'i32 | Protocol wrong type for socket. Protocol not available. HTTP version not supported. |
| ProtocolError                   |     EPROTO |  |  | Protocol error. |
| ReadonlyProtection              |     EROFS  |   | 19'i32   | Cannot write to readonly data. |
| SegFault                        |     EFAULT |  | 487'i32 | Bad address. Segmentation fault. Nil pointer derefence. |
| DiskCorruption                  |            |  | 276'i32, 323'i32, 634'i32, 1392'i32, 1393'i32, 1500'i32, 6843'i32, 9572'i32 | Corrupted disk/file/table. |
| Disconnected                    |     ENETDOWN, ENETUNREACH, ECONNRESET, ENOTCONN |  | 10050'i32, 10051'i32, 10052'i32, 10053'i32, 10054'i32 | Network is down. Network unreachable. Connection reset. The socket is not connected. |
| RefusedConnection               |     ECONNREFUSED |  | 10061'i32 | Connection refused. |
| UnreachableHost                 |     EHOSTUNREACH | 502 | 10065'i32 | Host is unreachable. Bad Gateway. |
| UnrecoverableState              |     ENOTRECOVERABLE |  | | State not recoverable. |
| AuthenticationRequired          |      |  511 |  | Network authentication required. |
| RedirectError                   |      | 308, 307  | | Redirect to other URL/path. |
| Reserved1                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved2                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved3                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved4                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved5                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved6                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved7                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved8                       |      |          | | Reserved for future extensions. This field will then be renamed! |
| Reserved9                       |      |          | | Reserved for future extensions. This field will then be renamed! |

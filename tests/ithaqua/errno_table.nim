# std/oserrors on a freestanding target (std/os and std/dirs do not build there): the POSIX errno <-> ErrorCode
# tables are pure integer mappings, so they are a legitimate differential fixture
# for `nimony w`. Under `nimony w` (standalone) errorcodes_posix.nim has no libc
# to read <errno.h> from; it carries the Linux table (the native oracle is Linux).
# Nothing here reads errno or calls the OS.
import std/[syncio, oserrors]
import std/errorcodes/errorcodes_posix

proc show(e: int32) =
  let c = posixToErrorCode(e)
  echo e, " -> ", $c, " (", ord(c), ") -> ", errorCodeToPosix(c)

for e in [0'i32, 1, 2, 4, 5, 9, 11, 12, 13, 17, 22, 28, 32, 36, 110, 99999]:
  show e

# the reverse direction, including the "no errno" default of 1
for c in [ErrorCode.Success, ErrorCode.PermissionDenied, ErrorCode.NameExists,
          ErrorCode.DiskFullError, ErrorCode.TimeoutError, ErrorCode.Failure]:
  echo $c, " -> ", errorCodeToPosix(c)

# osLastError is deliberately not printed (it depends on the target); the proc
# must merely compile and link.
let code = OSErrorCode(2'i32)
echo int32(code)

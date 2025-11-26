import syncio

## The `std/oserrors` module implements OS error reporting.

type
  OSErrorCode* = distinct int32 ## Specifies an OS Error Code.

when defined(windows):
  import windows/winlean
else:
  var errno {.importc: "errno", header: "<errno.h>".}: cint


when defined(windows):
  import "../../vendor/errorcodes/src" / errorcodes_windows
else:
  import "../../vendor/errorcodes/src" / errorcodes_posix

proc raiseOSError*(errorCode: OSErrorCode, additionalInfo = "") {.noinline, raises.} =
  ## Raises an `OSError exception <system.html#OSError>`_.
  ##
  ## Read the description of the `newOSError proc`_ to learn
  ## how the exception object is created.
  when defined(windows):
    raise windowsToErrorCode(errorCode.int32)
  else:
    raise posixToErrorCode(errorCode.int32)

#{.push stackTrace:off.}
proc osLastError*(): OSErrorCode {.sideEffect.} =
  ## Retrieves the last operating system error code.
  ##
  ## This procedure is useful in the event when an OS call fails. In that case
  ## this procedure will return the error code describing the reason why the
  ## OS call failed. The `OSErrorMsg` procedure can then be used to convert
  ## this code into a string.
  ##
  ## .. warning:: The behaviour of this procedure varies between Windows and POSIX systems.
  ##   On Windows some OS calls can reset the error code to `0` causing this
  ##   procedure to return `0`. It is therefore advised to call this procedure
  ##   immediately after an OS call fails. On POSIX systems this is not a problem.
  ##
  ## See also:
  ## * `osErrorMsg proc`_
  ## * `raiseOSError proc`_
  when defined(windows):
    result = cast[OSErrorCode](getLastError())
  else:
    result = OSErrorCode(errno)
#{.pop.}

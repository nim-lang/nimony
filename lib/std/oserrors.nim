import syncio

## The `std/oserrors` module implements OS error reporting.

type
  OSErrorCode* = distinct int32 ## Specifies an OS Error Code.

when defined(windows):
  import windows/winlean
elif defined(posix):
  from posix/posix import errno


when defined(windows):
  import errorcodes / errorcodes_windows
else:
  import errorcodes / errorcodes_posix

func raiseOSError*(errorCode: OSErrorCode, additionalInfo = "") {.noinline, raises, noreturn.} =
  ## Raises an `OSError exception <system.html#OSError>`_.
  ##
  ## Read the description of the `newOSError proc`_ to learn
  ## how the exception object is created.
  ##
  ## A zero `errorCode` maps to `Success`, and raising that is a no-op — this
  ## proc would *return*, in breach of `.noreturn`, into a caller that has
  ## already established the operation failed. It then carries on with whatever
  ## the failed call left behind. That is not hypothetical: `errno` reads zero
  ## whenever the error slot was never written for this call (a raw-syscall
  ## wrapper, or libc's errno read through the wrong accessor), and the result
  ## was a `-1` file descriptor travelling into `mmap` and a `MAP_FAILED` ring
  ## being written through. The caller knows something went wrong; report that
  ## rather than nothing.
  {.cast(noSideEffect).}:
    when defined(windows):
      var e = windowsToErrorCode(errorCode.int32)
    else:
      var e = posixToErrorCode(errorCode.int32)
    if e == Success: e = Failure
    raise e

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
  elif defined(posix):
    result = OSErrorCode(errno())
  else:
    # Freestanding targets (wasm32 standalone): no OS, no errno. Zero,
    # honestly, per the wasm bring-up convention.
    result = OSErrorCode(0)
#{.pop.}


import std/oserrors

import oscommons
when supportedSystem:
  export symlinkExists

import ../[syncio, assertions, widestrs]

when weirdTarget:
  discard
elif defined(windows):
  import ../windows/winlean
elif defined(posix):
  import ../posix/posix


when weirdTarget:
  {.pragma: noWeirdTarget, error: "this proc is not available on the NimScript/js target".}
else:
  {.pragma: noWeirdTarget.}


when defined(nimscript):
  # for procs already defined in scriptconfig.nim
  template noNimJs(body): untyped = discard
elif defined(js):
  {.pragma: noNimJs, error: "this proc is not available on the js target".}
else:
  {.pragma: noNimJs.}

## .. importdoc:: os.nim

proc createSymlink*(src, dest: string) {.noWeirdTarget, raises.} =
  ## Create a symbolic link at `dest` which points to the item specified
  ## by `src`. On most operating systems, will fail if a link already exists.
  ##
  ## .. warning:: Some OS's (such as Microsoft Windows) restrict the creation
  ##   of symlinks to root users (administrators) or users with developer mode enabled.
  ##
  ## See also:
  ## * `createHardlink proc`_
  ## * `expandSymlink proc`_


  var src = src
  var dest = dest

  when defined(windows):
    const SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2'u32
    # allows anyone with developer mode on to create a link
    let flag = dirExists(src).uint32 or SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE

    var wSrc = newWideCString(src).toWideCString()
    var wDst = newWideCString(dest).toWideCString()
    if createSymbolicLinkW(wDst, wSrc, flag) == 0 or getLastError() != 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")
  else:
    if symlink(src.toCString(), dest.toCString()) != 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")

proc expandSymlink*(symlinkPath: string): string {.noWeirdTarget, raises.} =
  ## Returns a string representing the path to which the symbolic link points.
  ##
  ## On Windows this is a noop, `symlinkPath` is simply returned.
  ##
  ## See also:
  ## * `createSymlink proc`_
  when defined(windows) or defined(nintendoswitch):
    result = symlinkPath
  else:
    var bufLen = 1024
    while true:
      result = newString(bufLen)
      var symlinkPath = symlinkPath
      let len = readlink(symlinkPath.toCString(), result.toCString(), bufLen)
      if len < 0:
        raiseOSError(osLastError(), symlinkPath)
      if len < bufLen:
        result.setLen(len)
        break
      bufLen = bufLen shl 1

## This module implements directory operations for creating, removing,
## and iterating over directories.
##
## **See also:**
## * `paths module <paths.html>`_ for path handling
## * `syncio module <syncio.html>`_ for file I/O

import std/[oserrors]
import std/paths  # separate import for better symbol resolution
import private/oscommons

export PathComponent
export paths.Path, paths.initPath, paths.`/`, paths.`$`

when defined(windows):
  import windows/winlean
  from widestrs import newWideCString, toWideCString

else:
  import posix/posix

when defined(windows):
  import "../../vendor/errorcodes/src" / errorcodes_windows
else:
  import "../../vendor/errorcodes/src" / errorcodes_posix

  var errno {.importc: "errno", header: "<errno.h>".}: cint

proc tryCreateFinalDir*(dir: Path): ErrorCode =
  ## Tries to create the final directory in a path.
  ## In other words, it tries to create a single new directory, not a nested one.
  ## It returns the OS's error code making it easy to distinguish between
  ## "could not create" and "already exists".
  var dirStr = $dir
  when defined(windows):
    if createDirectoryW(newWideCString(dirStr).rawData) == 0'i32:
      result = Success
    else:
      result = windowsToErrorCode getLastError()
  else:
    if mkdir(dirStr.toCString, 0o777) == 0'i32:
      result = Success
    else:
      result = posixToErrorCode(errno)

proc createDir*(dir: Path) {.raises.} =
  ## Creates a new directory `dir`. If the directory already exists, no error is raised.
  ## This can be used to create a nested directory structure directly.
  for d in parentDirs(dir, fromRoot=false, inclusive=true):
    let res = tryCreateFinalDir(d)
    if res == Success or res == NameExists:
      discard "fine"
    else:
      raise res

#[
proc removeDir*(dir: Path) {.raises.} =
  ## Removes the directory `dir`.
  var dirStr = $dir
  when defined(windows):
    if removeDirectoryW(newWideCString(dirStr).rawData) == 0'i32:
      raiseOSError(osLastError())
  else:
    if rmdir(dirStr.toCString) != 0'i32:
      raiseOSError(osLastError())

proc removeFile*(file: Path) {.raises.} =
  ## Removes the file `file`.
  var fileStr = $file
  when defined(windows):
    if deleteFileW(newWideCString(fileStr)).isFail:
      raiseOSError(osLastError())
  else:
    if unlink(fileStr.toCString) != 0'i32:
      raiseOSError(osLastError())

iterator walkDir*(dir: Path,
                  relative = false,
                  checkDir = false): tuple[kind: PathComponent, path: Path] =
  ## Walks over all entries in the directory `dir`.
  ##
  ## Yields tuples of `(kind, path)` where `kind` is one of:
  ## * `pcFile` - regular file
  ## * `pcDir` - directory
  ## * `pcLinkToFile` - symbolic link to a file
  ## * `pcLinkToDir` - symbolic link to a directory
  ##
  ## If `relative` is true, yields relative paths (just the filename/dirname),
  ## otherwise yields full paths.
  ##
  ## If `checkDir` is true, raises an error if `dir` doesn't exist or isn't a directory.
  ##
  ## Special directories "." and ".." are skipped.
  ##
  ## See also:
  ## * `walkDirRec iterator`_
  ## * `dirExists proc <oscommons.html#dirExists,string>`_
  when defined(windows):
    var findData: WIN32_FIND_DATA
    var dirStr = $dir
    let searchPath = dirStr & (when defined(windows): '\\' else: '/') & "*"
    let handle = findFirstFile(searchPath, findData)
    if handle == INVALID_HANDLE_VALUE:
      if checkDir:
        raiseOSError(osLastError())
    else:
      defer:
        discard findClose(handle)

      while true:
        if not skipFindData(findData):
          # Use same pattern as oscommons for getting filename
          var filename = ""
          var i = 0
          while findData.cFileName[i].int16 != 0'i16:
            filename.add char(findData.cFileName[i].int and 0xFF)
            inc i

          let fullPath = if relative: initPath(filename) else: dir / initPath(filename)

          let isDir = (findData.dwFileAttributes.uint32 and FILE_ATTRIBUTE_DIRECTORY) != 0'u32
          let isLink = (findData.dwFileAttributes.uint32 and FILE_ATTRIBUTE_REPARSE_POINT) != 0'u32

          var kind: PathComponent
          if isLink:
            kind = if isDir: pcLinkToDir else: pcLinkToFile
          else:
            kind = if isDir: pcDir else: pcFile

          yield (kind, fullPath)

        if findNextFileW(handle, findData) == 0'i32:
          break

  else: # POSIX
    var dirStr = $dir
    let d = opendir(dirStr.toCString)
    if d == nil:
      if checkDir:
        raiseOSError(osLastError())
    else:
      defer:
        discard closedir(d)

      while true:
        let entry = readdir(d)
        if entry == nil:
          break

        let name = $cast[cstring](addr entry.d_name[0])
        # Skip "." and ".."
        if name == "." or name == "..":
          continue

        let fullPath = if relative:
          initPath(name)
        else:
          dir / initPath(name)

        let fullPathStr = $fullPath
        # Determine the kind
        if symlinkExists(fullPathStr):
          let (pc, _) = getSymlinkFileKind(fullPathStr)
          yield (pc, fullPath)
        elif dirExists(fullPathStr):
          yield (pcDir, fullPath)
        elif fileExists(fullPathStr):
          yield (pcFile, fullPath)
        else:
          # Unknown type, treat as file
          yield (pcFile, fullPath)

proc getCurrentDir*(): Path {.raises.} =
  ## Returns the current working directory as a `Path`.
  ##
  ## Raises an error if unable to retrieve the current directory.
  ##
  ## See also:
  ## * `setCurrentDir proc`_
  when defined(windows):
    const bufSize = 1024'i32
    var buffer = newWideCString("", bufSize)
    let res = getCurrentDirectoryW(bufSize, buffer.rawData)
    if res == 0'i32:
      raiseOSError(osLastError())
    result = paths.initPath($buffer)
  else:
    const bufSize = 1024
    var buffer = newString(bufSize)
    if getcwd(addr buffer[0], bufSize) == nil:
      raiseOSError(osLastError())
    result = paths.initPath($cast[cstring](addr buffer[0]))

proc setCurrentDir*(dir: Path) {.raises.} =
  ## Sets the current working directory to `dir`.
  ##
  ## Raises errors if the directory doesn't exist or lacks permissions.
  ##
  ## See also:
  ## * `getCurrentDir proc`_
  var dirStr = $dir
  when defined(windows):
    if setCurrentDirectoryW(newWideCString(dirStr).rawData) == 0'i32:
      raiseOSError(osLastError())
  else:
    if chdir(dirStr.toCString) != 0'i32:
      raiseOSError(osLastError())

]#

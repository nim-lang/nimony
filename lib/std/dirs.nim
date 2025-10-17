## This module implements operations for creating, removing,
## and iterating over directories.
##
## **See also:**
## * `paths module <paths.html>`_ for path handling
## * `syncio module <syncio.html>`_ for file I/O

import std/[oserrors]
import std/paths  # separate import for better symbol resolution
import private/oscommons

export PathComponent
export paths.Path, paths.path, paths.`/`, paths.`$`

when defined(windows):
  import windows/winlean
  from widestrs import newWideCString, toWideCString, `$`

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
    if createDirectoryW(newWideCString(dirStr).rawData) != 0'i32:
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

proc tryRemoveFinalDir*(dir: Path): ErrorCode =
  ## Tries to remove the final directory in a path.
  ## In other words, it tries to remove a single directory, not a nested one.
  ## It returns the OS's error code making it easy to distinguish between
  ## "could not remove" and "does not exist".
  var dirStr = $dir
  when defined(windows):
    if removeDirectoryW(newWideCString(dirStr).rawData) != 0'i32:
      result = Success
    else:
      result = windowsToErrorCode getLastError()
  else:
    if rmdir(dirStr.toCString) == 0'i32:
      result = Success
    else:
      result = posixToErrorCode(errno)

proc removeDir*(dir: Path) {.raises.} =
  ## Removes the directory `dir`. If the directory does not exist, no error is raised.
  let res = tryRemoveFinalDir(dir)
  if res == Success or res == NameNotFound:
    discard "fine"
  else:
    raise res

proc tryRemoveFile*(file: Path): ErrorCode =
  var fileStr = $file
  when defined(windows):
    if deleteFileW(newWideCString(fileStr).rawData).isSuccess:
      result = Success
    else:
      result = windowsToErrorCode getLastError()
  else:
    if unlink(fileStr.toCString) == 0'i32:
      result = Success
    else:
      result = posixToErrorCode(errno)

proc removeFile*(file: Path) {.raises.} =
  ## Removes the file `file`.
  ## If the file does not exist, no error is raised.
  let res = tryRemoveFile(file)
  if res == Success or res == NameNotFound:
    discard "fine"
  else:
    raise res

type
  DirEntry* = object
    kind*: PathComponent
    path*: Path

  DirWalker* = object
    when defined(windows):
      wimpl: WIN32_FIND_DATA
      handle: Handle
    else:
      pimpl: ptr DIR
    dir: Path
    status: ErrorCode

proc tryOpenDir*(dir: sink Path): DirWalker =
  ## Tries to open the directory `dir` for iteration over its entries.
  result = DirWalker(dir: dir, status: EmptyError)
  when defined(windows):
    var dirStr = $result.dir & "\\*"
    result.handle = findFirstFileW(newWideCString(dirStr).rawData, result.wimpl)
    if result.handle == INVALID_HANDLE_VALUE:
      result.status = windowsToErrorCode getLastError()
    else:
      result.status = Success
  else:
    var dirStr = $result.dir
    result.pimpl = opendir(dirStr.toCString)
    if result.pimpl == nil:
      result.status = posixToErrorCode(errno)
    else:
      result.status = Success

proc fillDirEntry(w: var DirWalker; e: var DirEntry) =
  when defined(windows):
    let isDir = (w.wimpl.dwFileAttributes.uint32 and FILE_ATTRIBUTE_DIRECTORY) != 0'u32
    let isLink = (w.wimpl.dwFileAttributes.uint32 and FILE_ATTRIBUTE_REPARSE_POINT) != 0'u32

    var kind: PathComponent
    if isLink:
      kind = if isDir: pcLinkToDir else: pcLinkToFile
    else:
      kind = if isDir: pcDir else: pcFile
    e.kind = kind
    let f = cast[ptr UncheckedArray[WinChar]](addr w.wimpl.cFileName)
    e.path = paths.path($f)

when defined(posix):
  proc pathComponentFromEntry(w: var DirWalker; name: string; dType: uint8): PathComponent =
    if dType == DT_UNKNOWN or dType == DT_LNK:
      var fullPath = concat($w.dir, "/", name)
      var s = default Stat
      if lstat(fullPath.toCString, s) == 0:
        if S_ISLNK(s.st_mode):
          # For symlinks, we need to check if they point to a directory
          var targetStat = default Stat
          if stat(fullPath.toCString, targetStat) == 0 and S_ISDIR(targetStat.st_mode):
            result = pcLinkToDir
          else:
            result = pcLinkToFile
        elif S_ISDIR(s.st_mode):
          result = pcDir
        else:
          result = pcFile
    elif dType == DT_DIR:
      result = pcDir
    else:
      result = pcFile

proc tryNextDir*(w: var DirWalker; e: var DirEntry): bool =
  when defined(windows):
    while w.status == Success:
      if findNextFileW(w.handle, w.wimpl) != 0'i32:
        if skipFindData(w.wimpl):
          discard "skip entry"
        else:
          break
      else:
        # do not overwrite first error
        if w.status == Success:
          w.status = windowsToErrorCode getLastError()
        break
    result = w.status == Success
    if result:
      fillDirEntry(w, e)
  else:
    while w.status == Success:
      let entry = readdir(w.pimpl)
      if entry == nil:
        if w.status == Success:
          w.status = posixToErrorCode(errno)
        result = false
        break
      else:
        result = true
        let cstr = cast[cstring](addr entry.d_name[0])
        let name = fromCString cstr
        # Skip "." and ".."
        if name == "." or name == "..":
          discard "skip"
        else:
          e.kind = pathComponentFromEntry(w, name, entry.d_type)
          e.path = path(name)
          break

proc tryCloseDir*(w: var DirWalker): ErrorCode =
  when defined(windows):
    if findClose(w.handle).isSuccess:
      result = Success
    else:
      result = windowsToErrorCode getLastError()
  else:
    if closedir(w.pimpl) == 0'i32:
      result = Success
    else:
      result = posixToErrorCode(errno)

iterator walkDir*(dir: Path,
                  relative = false,
                  checkDir = false): tuple[kind: PathComponent, path: Path] {.raises.} =
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
  var w = tryOpenDir(dir)
  if checkDir and w.status != Success:
    raise w.status
  try:
    var e = default DirEntry
    while w.status == Success:
      if tryNextDir(w, e):
        let rel = if relative: e.path else: dir / e.path
        yield (e.kind, rel)
      else:
        break
  finally:
    let res = tryCloseDir(w)
    if checkDir and res != Success: raise res

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
    result = paths.path($buffer)
  else:
    const bufSize = 1024
    var buffer = newString(bufSize)
    if getcwd(buffer.toCString(), bufSize) == nil:
      raiseOSError(osLastError())
    var i = 0
    while i < buffer.len and buffer[i] != '\0': inc i
    shrink(buffer, i)
    result = paths.path(buffer)

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

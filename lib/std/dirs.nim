## This module implements operations for creating, removing,
## and iterating over directories.
##
## **See also:**
## * `paths module <paths.html>`_ for path handling
## * `syncio module <syncio.html>`_ for file I/O

import std/[oserrors]
import std/paths  # separate import for better symbol resolution
import private/oscommons
import std/files

export PathComponent
export paths.Path, paths.path, paths.`/`, paths.`$`
export files.removeFile, files.tryRemoveFile

when defined(windows):
  import windows/winlean
  from widestrs import newWideCString, toWideCString, `$`

else:
  import posix/posix

when defined(windows):
  import errorcodes / errorcodes_windows
else:
  import errorcodes / errorcodes_posix
  # `errno` comes from posix/posix: the libc accessor under useLibc, or the
  # native global maintained by the freestanding wrappers.
{.feature: "lenientnils".}

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
    # `pcall`, not `errno()`: a freestanding build has no libc errno, and a
    # raw syscall reports its error in the result.
    let r = pcall(mkdir(dirStr.toCString, 0o777))
    result = if r == 0: Success else: posixToErrorCode(cint(-r))

proc createDir*(dir: Path) {.raises.} =
  ## Creates a new directory `dir`. If the directory already exists, no error is raised.
  ## This can be used to create a nested directory structure directly.

  # issue #2159
  # creating directory `c:\` results in error on Windows
  var omitNext = isAbsolute(dir)
  # fromRoot=true: parents must be created root-first; the default leaf-first
  # order makes the first mkdir fail with ENOENT for any nested path.
  for d in parentDirs(dir, fromRoot=true, inclusive=true):
    if omitNext:
      omitNext = false
    else:
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
    let r = pcall(rmdir(dirStr.toCString))
    result = if r == 0: Success else: posixToErrorCode(cint(-r))

type
  DirEntry* = object
    kind*: PathComponent
    path*: Path

  DirWalker* = object
    when defined(windows):
      wimpl: WIN32_FIND_DATA
      handle: Handle
    else:
      stream: DirStream
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
    let r = openDirStream(result.stream, dirStr.toCString)
    result.status = if r < 0: posixToErrorCode(cint(-r)) else: Success

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
  proc pathComponentFromEntry(w: var DirWalker; name: string; dType: uint8;
                              special: var bool): PathComponent =
    ## `special` is set for a non-regular file that is not a directory or a
    ## symlink: a FIFO, socket or device, which is reported as `pcFile`.
    special = false
    if dType == DT_UNKNOWN or dType == DT_LNK:
      var fullPath = $w.dir & "/" & name
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
          special = not S_ISREG(s.st_mode)
      else:
        # XXX Better error handling
        result = pcFile
    elif dType == DT_DIR:
      result = pcDir
    else:
      result = pcFile
      special = dType != DT_REG

proc tryNextDir*(w: var DirWalker; e: var DirEntry; skipSpecial = false): bool =
  ## Advances `w` to its next entry and stores it in `e`. Returns false at
  ## the end of the directory or on an error, which `w` then keeps.
  ##
  ## If `skipSpecial` is true, special "file" objects (FIFOs, sockets,
  ## device files) are skipped on POSIX; otherwise they are reported as
  ## `pcFile`. Windows has none.
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
    result = false
    while w.status == Success:
      var cname: cstring = ""
      var dType = DT_UNKNOWN
      let r = nextDirEntry(w.stream, cname, dType)
      if r <= 0:
        # 0 is the end of the directory; it leaves `w.status` alone
        if r < 0:
          w.status = posixToErrorCode(cint(-r))
        result = false
        break
      else:
        result = true
        let name = fromCString cname
        # Skip "." and ".."
        if name == "." or name == "..":
          discard "skip"
        else:
          var special = false
          e.kind = pathComponentFromEntry(w, name, dType, special)
          if special and skipSpecial:
            discard "skip"
          else:
            e.path = path(name)
            break

proc tryCloseDir*(w: var DirWalker): ErrorCode =
  ## Closes `w`. A walker whose `tryOpenDir` failed holds no handle, so there
  ## is nothing to close.
  when defined(windows):
    if w.handle == INVALID_HANDLE_VALUE:
      result = Success
    elif findClose(w.handle).isSuccess:
      result = Success
    else:
      result = windowsToErrorCode getLastError()
  else:
    let r = closeDirStream(w.stream)
    result = if r < 0: posixToErrorCode(cint(-r)) else: Success

iterator walkDir*(dir: Path,
                  relative = false,
                  checkDir = false,
                  skipSpecial = false): tuple[kind: PathComponent, path: Path] {.raises, sideEffect.} =
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
  ## Special directories "." and ".." are skipped. Special "file" objects
  ## (FIFOs, sockets, device files) are yielded as `pcFile`, unless
  ## `skipSpecial` is true: then they are skipped on POSIX.
  var w = tryOpenDir(dir)
  if checkDir and w.status != Success:
    raise w.status
  try:
    var e = default DirEntry
    while w.status == Success:
      if tryNextDir(w, e, skipSpecial):
        let rel = if relative: e.path else: dir / e.path
        yield (e.kind, rel)
      else:
        break
  finally:
    let res = tryCloseDir(w)
    if checkDir and res != Success: raise res

iterator walkDirRec*(dir: Path,
                     yieldFilter = {pcFile}, followFilter = {pcDir},
                     relative = false, checkDir = false,
                     skipSpecial = false): Path {.raises, sideEffect.} =
  ## Recursively walks over the directory `dir` and yields for each file
  ## or directory in `dir`.
  ##
  ## Options `relative`, `checkDir` and `skipSpecial` are explained in the
  ## `walkDir iterator`_ description; `checkDir` only applies to `dir`
  ## itself, so a subdirectory that cannot be opened is silently skipped.
  ##
  ## .. warning:: Modifying the directory structure while the iterator
  ##   is traversing may result in undefined behavior!
  ##
  ## Walking is recursive. `followFilter` controls the behaviour of the iterator:
  ##
  ## =====================   =============================================
  ## yieldFilter             meaning
  ## =====================   =============================================
  ## ``pcFile``              yield real files (default)
  ## ``pcLinkToFile``        yield symbolic links to files
  ## ``pcDir``               yield real directories
  ## ``pcLinkToDir``         yield symbolic links to directories
  ## =====================   =============================================
  ##
  ## =====================   =============================================
  ## followFilter            meaning
  ## =====================   =============================================
  ## ``pcDir``               follow real directories (default)
  ## ``pcLinkToDir``         follow symbolic links to directories
  ## =====================   =============================================
  ##
  ## See also:
  ## * `walkDir iterator`_
  var stack: seq[Path] = @[path("")]
  var checkDir = checkDir
  while stack.len > 0:
    let d = stack.pop()
    let sub = if ($d).len == 0: dir else: dir / d
    for k, p in walkDir(sub, relative = true, checkDir = checkDir,
                        skipSpecial = skipSpecial):
      let rel = if ($d).len == 0: p else: d / p
      if k in {pcDir, pcLinkToDir} and k in followFilter:
        stack.add rel
      if k in yieldFilter:
        yield (if relative: rel else: dir / rel)
    # Only `dir` itself is checked: an unreadable subdirectory would
    # otherwise abort the whole walk with no way to continue it.
    checkDir = false

proc removeDir*(dir: Path; checkDir = false) {.raises.} =
  ## Removes the directory `dir` including all subdirectories and files
  ## in `dir` (recursively).
  ##
  ## Symlinks inside `dir` are removed as links; their targets are never
  ## followed. This does not fail if the directory never existed in the first
  ## place, unless `checkDir` is true.
  ##
  ## Use `tryRemoveFinalDir` to remove a single directory only if it is empty.
  ##
  ## See also:
  ## * `tryRemoveFinalDir proc`_
  ## * `removeFile proc`_
  ## * `createDir proc`_
  ## * `moveDir proc`_
  for kind, p in walkDir(dir, checkDir = checkDir):
    case kind
    of pcFile, pcLinkToFile:
      removeFile(p)
    of pcLinkToDir:
      when defined(windows):
        # A directory symlink or junction is a directory entry on Windows:
        # `DeleteFileW` refuses it, `RemoveDirectoryW` removes the link and
        # leaves the target alone.
        let res = tryRemoveFinalDir(p)
        if res != Success and res != NameNotFound: raise res
      else:
        removeFile(p)
    of pcDir:
      # For subdirectories there is no benefit in `checkDir = false`.
      removeDir(p, true)
  let res = tryRemoveFinalDir(dir)
  if res == Success or (res == NameNotFound and not checkDir):
    discard "fine"
  else:
    raise res

proc copyDir*(source, dest: Path; skipSpecial = false) {.raises.} =
  ## Copies a directory from `source` to `dest`.
  ##
  ## On non-Windows OSes, symlinks are copied as symlinks. On Windows, symlinks
  ## are skipped.
  ##
  ## If `skipSpecial` is true, then (besides all directories) only *regular*
  ## files (**without** special "file" objects like FIFOs, device files,
  ## etc) will be copied on POSIX.
  ##
  ## On Windows this proc copies the attributes from `source` into `dest`.
  ## On other platforms created files and directories get the default
  ## permissions of a newly created file/directory for the user. Use
  ## `copyDirWithPermissions proc`_ to preserve them recursively.
  ##
  ## See also:
  ## * `copyDirWithPermissions proc`_
  ## * `copyFile proc <files.html#copyFile,Path,Path>`_
  ## * `removeDir proc`_
  ## * `createDir proc`_
  ## * `moveDir proc`_
  createDir(dest)
  for kind, p in walkDir(source, relative = true, skipSpecial = skipSpecial):
    if kind == pcDir:
      copyDir(source / p, dest / p, skipSpecial)
    else:
      copyFile(source / p, dest / p, cfSymlinkAsIs)

proc copyDirWithPermissions*(source, dest: Path;
                             ignorePermissionErrors = true;
                             skipSpecial = false) {.raises.} =
  ## Copies a directory from `source` to `dest` preserving file permissions.
  ##
  ## On non-Windows OSes, symlinks are copied as symlinks. On Windows, symlinks
  ## are skipped. `skipSpecial` is as for `copyDir proc`_.
  ##
  ## On Windows this is just `copyDir proc`_, which already copies the
  ## attributes. On other systems the permissions of each file and directory
  ## are applied after it has been copied, which does not happen atomically.
  ## If `ignorePermissionErrors` is true (default), errors while reading or
  ## setting them are ignored; otherwise they are raised.
  ##
  ## See also:
  ## * `copyDir proc`_
  ## * `copyFileWithPermissions proc <files.html#copyFileWithPermissions,Path,Path>`_
  ## * `removeDir proc`_
  ## * `moveDir proc`_
  createDir(dest)
  when not defined(windows):
    try:
      setFilePermissions(dest, getFilePermissions(source), followSymlinks = false)
    except ErrorCode as e:
      if not ignorePermissionErrors: raise e
  for kind, p in walkDir(source, relative = true, skipSpecial = skipSpecial):
    if kind == pcDir:
      copyDirWithPermissions(source / p, dest / p, ignorePermissionErrors,
                             skipSpecial)
    else:
      copyFileWithPermissions(source / p, dest / p, ignorePermissionErrors,
                              cfSymlinkAsIs)

proc moveDir*(source, dest: Path) {.raises.} =
  ## Moves a directory from `source` to `dest`.
  ##
  ## Symlinks are not followed: if `source` contains symlinks, they themselves
  ## are moved, not their targets. As with `rename`, an existing `dest` must
  ## be an empty directory, which is replaced.
  ##
  ## Within one filesystem the move is a single `rename`. Across filesystems
  ## (`EXDEV` on POSIX, an access error on Windows) it falls back to
  ## `copyDir proc`_ followed by `removeDir proc`_ on `source`, as Nim does.
  ##
  ## See also:
  ## * `moveFile proc <files.html#moveFile,Path,Path>`_
  ## * `copyDir proc`_
  ## * `createDir proc`_
  ## * `removeDir proc`_
  var s = $source
  var d = $dest
  var crossDevice = false
  when defined(windows):
    if moveFileExW(newWideCString(s).rawData, newWideCString(d).rawData,
                   MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING).isFail:
      let err = getLastError()
      if err == 5'i32:  # ERROR_ACCESS_DENIED: a directory across volumes
        crossDevice = true
      else:
        raise windowsToErrorCode err
  else:
    let r = pcall(rename(s.toCString, d.toCString))
    if r == clong(-EXDEV):
      crossDevice = true
    elif r < 0:
      raise posixToErrorCode(cint(-r))
  if crossDevice:
    copyDir(source, dest)
    removeDir(source)

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
    let r = tryGetcwd(buffer.toCString(), bufSize)
    if r < 0:
      raiseOSError(OSErrorCode(-r))
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

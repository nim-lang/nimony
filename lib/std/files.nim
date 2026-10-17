## This module implements file handling on `Path`: removing, copying and
## moving files, and querying or changing their permissions.
##
## **See also:**
## * `paths module <paths.html>`_ for path manipulation
## * `dirs module <dirs.html>`_ for directories
## * `symlinks module <symlinks.html>`_ for symbolic links

import std/paths  # separate import for better symbol resolution
import std/[commonio, symlinks]

export paths.Path, paths.path, paths.`/`, paths.`$`
export FilePermission

when defined(windows):
  import windows/winlean
  from widestrs import newWideCString, toWideCString
  import errorcodes / errorcodes_windows
else:
  import posix/posix
  import errorcodes / errorcodes_posix

type
  CopyFlag* = enum    ## Copy options.
    cfSymlinkAsIs,    ## Copy symlinks as symlinks
    cfSymlinkFollow,  ## Copy the files symlinks point to
    cfSymlinkIgnore   ## Ignore symlinks

when defined(posix):
  const PermBits: array[FilePermission, uint32] = [
    0o100'u32, 0o200, 0o400, 0o010, 0o020, 0o040, 0o001, 0o002, 0o004]

  when defined(osx):
    proc createOpen(path: cstring; flags: cint): cint {.varargs, importc: "open", sideEffect.}
      ## `posix.open` with a variadic C prototype, like libc's. `posix.open` is
      ## fixed-arity, and on Apple arm64 a variadic argument goes on the stack
      ## rather than in a register, so libc would read a garbage `mode`.
  else:
    template createOpen(path: cstring; flags: cint; mode: Mode): cint = open(path, flags, mode)

  template posixError(r: clong): ErrorCode =
    ## The `ErrorCode` for a negative `pcall` result.
    posixToErrorCode(cint(-r))

proc tryRemoveFile*(file: Path): ErrorCode =
  ## Tries to remove the file `file`. A symlink is removed itself, never its
  ## target. On Windows the read-only attribute is ignored, as in Nim.
  var fileStr = $file
  when defined(windows):
    let f = newWideCString(fileStr)
    if deleteFileW(f.toWideCString).isSuccess:
      result = Success
    else:
      let err = getLastError()
      if err == 5'i32 and  # ERROR_ACCESS_DENIED
          setFileAttributesW(f.toWideCString, int32(FILE_ATTRIBUTE_NORMAL)).isSuccess and
          deleteFileW(f.toWideCString).isSuccess:
        result = Success
      else:
        result = windowsToErrorCode err
  else:
    let r = pcall(unlink(fileStr.toCString))
    result = if r == 0: Success else: posixToErrorCode(cint(-r))

proc removeFile*(file: Path) {.raises.} =
  ## Removes the file `file`.
  ## If the file does not exist, no error is raised.
  let res = tryRemoveFile(file)
  if res == Success or res == NameNotFound:
    discard "fine"
  else:
    raise res

proc getFilePermissions*(filename: Path): set[FilePermission] {.raises.} =
  ## Retrieves the permissions of `filename`, following symlinks.
  ##
  ## On Windows only the read-only attribute is checked: every other
  ## permission is reported as present.
  ##
  ## See also:
  ## * `setFilePermissions proc`_
  result = {}
  var f = $filename
  when defined(windows):
    let w = newWideCString(f)
    let a = getFileAttributesW(w.toWideCString)
    if a == -1'i32: raise windowsToErrorCode(getLastError())
    if (a.uint32 and FILE_ATTRIBUTE_READONLY) != 0'u32:
      result = {fpUserExec, fpUserRead, fpGroupExec, fpGroupRead,
                fpOthersExec, fpOthersRead}
    else:
      result = {fpUserExec..fpOthersRead}
  else:
    var st = default(Stat)
    let r = pcall(stat(f.toCString, st))
    if r < 0: raise posixError(r)
    for p in fpUserExec..fpOthersRead:
      if (uint32(st.st_mode) and PermBits[p]) != 0'u32: result.incl p

proc setFilePermissions*(filename: Path; permissions: set[FilePermission];
                         followSymlinks = true) {.raises.} =
  ## Sets the permissions of `filename`.
  ##
  ## If `followSymlinks` is true (default) and `filename` is a symlink, the
  ## permissions of the file it points to are changed. With `followSymlinks`
  ## set to false a symlink is left alone: symlink permissions are not
  ## observed on Linux, and this module binds no `lchmod`.
  ##
  ## On Windows only the read-only attribute is changed, following
  ## `fpUserWrite`.
  ##
  ## See also:
  ## * `getFilePermissions proc`_
  if not followSymlinks and symlinkExists(filename): return
  var f = $filename
  when defined(windows):
    let w = newWideCString(f)
    var a = getFileAttributesW(w.toWideCString)
    if a == -1'i32: raise windowsToErrorCode(getLastError())
    if fpUserWrite in permissions:
      a = a and not int32(FILE_ATTRIBUTE_READONLY)
    else:
      a = a or int32(FILE_ATTRIBUTE_READONLY)
    if isFail(setFileAttributesW(w.toWideCString, a)):
      raise windowsToErrorCode(getLastError())
  else:
    let r = pcall(chmod(f.toCString, Mode(permissionBits(permissions))))
    if r < 0: raise posixError(r)

when defined(posix):
  proc copyContents(src, dst: cint; bufferSize: int): ErrorCode =
    ## Copies everything left to read from `src` to `dst`. A signal may
    ## interrupt either call (EINTR) and `write` may take only part of the
    ## buffer, so both are retried until the data is through.
    result = Success
    var buf = newSeq[byte](max(bufferSize, 1))
    let p = cast[int](addr buf[0])
    while result == Success:
      let n = pcall(read(src, cast[pointer](p), buf.len))
      if n == clong(-EINTR): continue
      if n < 0:
        result = posixError(n)
      elif n == 0:
        break
      else:
        var off = 0
        while off < n:
          let w = pcall(write(dst, cast[pointer](p + off), int(n) - off))
          if w == clong(-EINTR): continue
          if w < 0:
            result = posixError(w)
            break
          off += int(w)

  proc copyRegular(source, dest: Path; bufferSize: int): ErrorCode =
    ## Copies the contents of `source` into `dest`, creating `dest` with
    ## mode `0o666` (less the umask) or truncating it if it exists, which
    ## keeps its permissions.
    var s = $source
    var d = $dest
    var r = pcall(open(s.toCString, O_RDONLY or O_CLOEXEC))
    if r < 0: return posixError(r)
    let src = cint(r)
    var st = default(Stat)
    r = pcall(fstat(src, st))
    if r < 0:
      result = posixError(r)
    elif S_ISDIR(st.st_mode):
      # Refuse before `dest` is created or truncated.
      result = posixToErrorCode(EISDIR)
    else:
      r = pcall(createOpen(d.toCString, O_WRONLY or O_CREAT or O_TRUNC or O_CLOEXEC,
                           Mode(0o666)))
      if r < 0:
        result = posixError(r)
      else:
        let dst = cint(r)
        result = copyContents(src, dst, bufferSize)
        # `close` may report a write the kernel deferred; it is not retried
        # on EINTR because the descriptor is released regardless.
        r = pcall(close(dst))
        if r < 0 and r != clong(-EINTR) and result == Success:
          result = posixError(r)
    discard close(src)

proc copyFile*(source, dest: Path; options = cfSymlinkFollow;
               bufferSize = 16_384) {.raises.} =
  ## Copies a file from `source` to `dest`, where `dest.parentDir` must exist.
  ##
  ## On non-Windows OSes, `options` specifies the way the file is copied; by
  ## default, if `source` is a symlink, the file it points to is copied.
  ## `options` is ignored on Windows: symlinks are skipped.
  ##
  ## If `dest` already exists, its permissions are kept and its content is
  ## overwritten. Otherwise on POSIX `dest` gets the default permissions of a
  ## new file (`0o666` less the umask); use `copyFileWithPermissions proc`_
  ## to carry over those of `source`. On Windows `copyFile` already copies
  ## the source file's attributes.
  ##
  ## `bufferSize` is the size of the chunks the content is copied in.
  ##
  ## See also:
  ## * `CopyFlag enum`_
  ## * `copyFileWithPermissions proc`_
  ## * `copyFileToDir proc`_
  let isSymlink = symlinkExists(source)
  if isSymlink and (options == cfSymlinkIgnore or defined(windows)):
    return
  when defined(windows):
    var s = $source
    var d = $dest
    let ws = newWideCString(s)
    let wd = newWideCString(d)
    if isFail(copyFileW(ws.toWideCString, wd.toWideCString, WINBOOL(0))):
      raise windowsToErrorCode(getLastError())
  else:
    if isSymlink and options == cfSymlinkAsIs:
      createSymlink(expandSymlink(source), dest)
    else:
      let res = copyRegular(source, dest, bufferSize)
      if res != Success: raise res

proc copyFileToDir*(source, dir: Path; options = cfSymlinkFollow;
                    bufferSize = 16_384) {.raises.} =
  ## Copies a file `source` into directory `dir`, which must exist.
  ##
  ## `options` and `bufferSize` are as for `copyFile proc`_. An empty `dir`
  ## raises `ValueError` rather than meaning the current directory.
  ##
  ## See also:
  ## * `copyFile proc`_
  if ($dir).len == 0: raise ValueError
  copyFile(source, dir / lastPathPart(source), options, bufferSize)

proc copyFileWithPermissions*(source, dest: Path;
                              ignorePermissionErrors = true;
                              options = cfSymlinkFollow) {.raises.} =
  ## Copies a file from `source` to `dest` preserving its permissions.
  ##
  ## `options` is as for `copyFile proc`_. On Windows this is just
  ## `copyFile`, which already copies the attributes.
  ##
  ## On POSIX the permissions are applied after the content is copied, which
  ## does not happen atomically. If `ignorePermissionErrors` is true (default),
  ## errors while reading or setting them are ignored; otherwise they are
  ## raised. A symlink copied as is, or skipped, gets no permissions applied:
  ## `chmod` would change the file it points to instead.
  ##
  ## See also:
  ## * `copyFile proc`_
  ## * `getFilePermissions proc`_
  ## * `setFilePermissions proc`_
  copyFile(source, dest, options)
  when not defined(windows):
    if options == cfSymlinkFollow or not symlinkExists(source):
      try:
        setFilePermissions(dest, getFilePermissions(source))
      except ErrorCode as e:
        if not ignorePermissionErrors: raise e

proc moveFile*(source, dest: Path) {.raises.} =
  ## Moves a file from `source` to `dest`, where `dest.parentDir` must exist.
  ##
  ## Symlinks are not followed: if `source` is a symlink, it is itself moved,
  ## not its target. If `dest` already exists, it is overwritten.
  ##
  ## Can be used to rename files. Within a filesystem the move is a single
  ## atomic `rename`. Across filesystems (`EXDEV`) POSIX falls back to
  ## `copyFileWithPermissions proc`_ followed by removing `source`; if that
  ## removal fails, `dest` is removed again and the error is raised. Windows
  ## lets `MoveFileExW` copy across volumes itself.
  ##
  ## See also:
  ## * `copyFile proc`_
  ## * `copyFileWithPermissions proc`_
  ## * `removeFile proc`_
  var s = $source
  var d = $dest
  when defined(windows):
    let ws = newWideCString(s)
    let wd = newWideCString(d)
    if isFail(moveFileExW(ws.toWideCString, wd.toWideCString,
                          MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING)):
      raise windowsToErrorCode(getLastError())
  else:
    let r = pcall(rename(s.toCString, d.toCString))
    if r == clong(-EXDEV):
      copyFileWithPermissions(source, dest, options = cfSymlinkAsIs)
      try:
        removeFile(source)
      except ErrorCode as e:
        discard tryRemoveFile(dest)
        raise e
    elif r < 0:
      raise posixError(r)

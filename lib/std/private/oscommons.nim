

import std/[oserrors]


import std/[syncio, assertions, widestrs]

type
  PathComponent* = enum   ## Enumeration specifying a path component.
    ##
    ## See also:
    ## * `walkDirRec iterator`_
    ## * `FileInfo object`_
    pcFile,               ## path refers to a file
    pcLinkToFile,         ## path refers to a symbolic link to a file
    pcDir,                ## path refers to a directory
    pcLinkToDir           ## path refers to a symbolic link to a directory


## .. importdoc:: osdirs.nim, os.nim

const
  weirdTarget* = defined(nimscript) or defined(js)
  supportedSystem* = weirdTarget or defined(windows) or defined(posix)




when weirdTarget:
  discard
elif defined(windows):
  import ../windows/winlean
elif defined(posix):
  import ../posix/posix
  proc c_rename(oldname, newname: cstring): cint {.
    importc: "rename", header: "<stdio.h>".}



when defined(windows) and not weirdTarget:
  template wrapUnary*(varname, winApiProc, arg: untyped; body: untyped) {.untyped.} =
    var varname: int32 = winApiProc(newWideCString(arg).toWideCString)

  template wrapBinary*(varname, winApiProc, arg, arg2: untyped) {.untyped.} =
    var varname = winApiProc(newWideCString(arg).toWideCString, arg2)
  proc findFirstFile*(a: string, b: var WIN32_FIND_DATA): Handle =
    var a = a
    result = findFirstFileW(newWideCString(a).toWideCString, b)
  template findNextFile*(a, b: untyped): untyped {.untyped.} = findNextFileW(a, b)

  # template getFilename*(f: untyped): untyped =
  #   $cast[WideCString](addr(f.cFileName[0]))

  proc skipFindData*(f: WIN32_FIND_DATA): bool {.inline.} =
    # Note - takes advantage of null delimiter in the cstring
    let dot = ord('.') # TODO: const
    result = f.cFileName[0].int == dot and (f.cFileName[1].int == 0 or
             f.cFileName[1].int == dot and f.cFileName[2].int == 0)

when supportedSystem:
  when defined(posix) and not weirdTarget:
    proc getSymlinkFileKind*(path: string):
        tuple[pc: PathComponent, isSpecial: bool] =
      # Helper function.
      var s: Stat = default(Stat)
      assert(path != "")
      result = (pcLinkToFile, false)

      var path = path
      if stat(path.toCString, s) == 0'i32:
        if S_ISDIR(s.st_mode):
          result = (pcLinkToDir, false)
        elif not S_ISREG(s.st_mode):
          result = (pcLinkToFile, true)

  proc tryMoveFSObject*(source, dest: string, isDir: bool): bool {.raises.} =
    ## Moves a file (or directory if `isDir` is true) from `source` to `dest`.
    ##
    ## Returns false in case of `EXDEV` error or `AccessDeniedError` on Windows (if `isDir` is true).
    ## In case of other errors `OSError` is raised.
    ## Returns true in case of success.
    when defined(windows):
      var source = source
      var dest = dest
      let s = newWideCString(source)
      let d = newWideCString(dest)
      result = (int32 moveFileExW(s.toWideCString, d.toWideCString, MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING)) != 0'i32
    else:
      var source = source
      var dest = dest
      result = c_rename(source.toCString, dest.toCString) == 0'i32

    if not result:
      let err = osLastError()
      let isAccessDeniedError =
        when defined(windows):
          const AccessDeniedError = OSErrorCode(5)
          isDir and err == AccessDeniedError
        else:
          err == EXDEV.OSErrorCode
      if not isAccessDeniedError:
        raiseOSError(err, "(" & $source & ", " & $dest)

  when not defined(windows):
    const maxSymlinkLen* = 1024

  proc fileExists*(filename: string): bool {.tags: [ReadDirEffect], sideEffect.} =
    ## Returns true if `filename` exists and is a regular file or symlink.
    ##
    ## Directories, device files, named pipes and sockets return false.
    ##
    ## See also:
    ## * `dirExists proc`_
    ## * `symlinkExists proc`_
    when defined(windows):
      var filename = filename
      wrapUnary(a, getFileAttributesW, filename):
        if a != -1'i32:
          result = (a and FILE_ATTRIBUTE_DIRECTORY) == 0'i32
    else:
      var res: Stat = default(Stat)
      var filename = filename
      return stat(filename.toCString, res) >= 0'i32 and S_ISREG(res.st_mode)


  proc dirExists*(dir: string): bool {.tags: [ReadDirEffect],
                                       sideEffect.} =
    ## Returns true if the directory `dir` exists. If `dir` is a file, false
    ## is returned. Follows symlinks.
    ##
    ## See also:
    ## * `fileExists proc`_
    ## * `symlinkExists proc`_
    when defined(windows):
      var dir = dir
      wrapUnary(a, getFileAttributesW, dir):
        if a != -1'i32:
          result = (a and FILE_ATTRIBUTE_DIRECTORY) != 0'i32
    else:
      var res: Stat = default(Stat)
      var dir = dir
      result = stat(dir.toCString, res) >= 0'i32 and S_ISDIR(res.st_mode)


  proc symlinkExists*(link: string): bool {.tags: [ReadDirEffect],
                                            sideEffect.} =
    ## Returns true if the symlink `link` exists. Will return true
    ## regardless of whether the link points to a directory or file.
    ##
    ## See also:
    ## * `fileExists proc`_
    ## * `dirExists proc`_
    when defined(windows):
      var link = link
      wrapUnary(a, getFileAttributesW, link):
        if a != -1'i32:
          # xxx see: bug #16784 (bug9); checking `IO_REPARSE_TAG_SYMLINK`
          # may also be needed.
          result = (a and FILE_ATTRIBUTE_REPARSE_POINT) != 0'i32
    else:
      var res: Stat = default(Stat)
      var link = link
      result = lstat(link.toCString, res) >= 0'i32 and S_ISLNK(res.st_mode)

  when defined(windows) and not weirdTarget:
    proc openHandle*(path: string, followSymlink=true, writeAccess=false): Handle =
      var flags = FILE_FLAG_BACKUP_SEMANTICS or FILE_ATTRIBUTE_NORMAL
      if not followSymlink:
        flags = flags or FILE_FLAG_OPEN_REPARSE_POINT
      let access = if writeAccess: GENERIC_WRITE else: 0'u32

      var path = path
      result = createFileW(
        newWideCString(path).toWideCString(), access,
        FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE,
        nil, OPEN_EXISTING, flags, cast[Handle](0)
        )

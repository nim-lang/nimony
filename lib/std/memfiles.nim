## This module provides support for `memory mapped files`:idx:
## (Posix's `mmap`:idx:) on the different operating systems.

import assertions, syncio

when defined(windows):
  import windows / winlean
  import widestrs
elif defined(posix):
  import posix / posix
else:
  {.error: "the memfiles module is not supported on your operating system!".}

import std/oserrors

# TODO: Remove following dummy types when fixed https://github.com/nim-lang/nimony/issues/965
when not defined(windows):
  type
    Handle = bool

type
  MemFile* = object      ## represents a memory mapped file
    mem*: pointer        ## a pointer to the memory mapped file. The pointer
                         ## can be used directly to change the contents of the
                         ## file, if it was opened with write access.
    size*: int           ## size of the memory mapped file

    when defined(windows):
      fHandle*: Handle   ## **Caution**: Windows specific public field to allow
                         ## even more low level trickery.
      mapHandle*: Handle ## **Caution**: Windows specific public field.
      wasOpened*: bool   ## **Caution**: Windows specific public field.
    else:
      handle*: cint      ## **Caution**: Posix specific public field.
      flags: cint        ## **Caution**: Platform specific private field.

when defined(windows):
  type OsFileHandle = Handle
else:
  type OsFileHandle = cint

proc setFileSize(fh: OsFileHandle, newFileSize = -1, oldSize = -1): OSErrorCode =
  ## Set the size of open file pointed to by `fh` to `newFileSize` if != -1,
  ## allocating | freeing space from the file system.  This routine returns the
  ## last OSErrorCode found rather than raising to support old rollback/clean-up
  ## code style. [ Should maybe move to std/osfiles. ]
  result = OSErrorCode(0)
  if newFileSize < 0 or newFileSize == oldSize:
    return result
  when defined(windows):
    var sizeHigh = LONG((newFileSize shr 32))
    let sizeLow = LONG((newFileSize and 0xffffffff))
    let status = setFilePointer(fh, sizeLow, addr(sizeHigh), FILE_BEGIN)
    let lastErr = osLastError()
    if (status == INVALID_SET_FILE_POINTER and lastErr.int32 != NO_ERROR) or
        setEndOfFile(fh).isFail:
      result = lastErr
  else:
    if newFileSize > oldSize: # grow the file
      var e: cint = cint(0) # posix_fallocate truncates up when needed.
      when declared(posix_fallocate):
        while (e = posix_fallocate(fh, 0, newFileSize); e == EINTR):
          discard
      if (e == EINVAL or e == EOPNOTSUPP) and ftruncate(fh, newFileSize) == -1:
        result = osLastError() # fallback arguable; Most portable BUT allows SEGV
      elif e != 0:
        result = osLastError()
    else: # shrink the file
      if ftruncate(fh.cint, newFileSize) == -1:
        result = osLastError()

proc open*(filename: string, mode: FileMode = fmRead,
           mappedSize = -1, offset = 0, newFileSize = -1,
           allowRemap = false, mapFlags = cint(-1)): MemFile {.raises.} =
  ## opens a memory mapped file. If this fails, `OSError` is raised.
  ##
  ## `newFileSize` can only be set if the file does not exist and is opened
  ## with write access (e.g., with fmReadWrite).
  ##
  ##`mappedSize` and `offset`
  ## can be used to map only a slice of the file.
  ##
  ## `offset` must be multiples of the PAGE SIZE of your OS
  ## (usually 4K or 8K but is unique to your OS)
  ##
  ## `allowRemap` only needs to be true if you want to call `mapMem` on
  ## the resulting MemFile; else file handles are not kept open.
  ##
  ## `mapFlags` allows callers to override default choices for memory mapping
  ## flags with a bitwise mask of a variety of likely platform-specific flags
  ## which may be ignored or even cause `open` to fail if misspecified.
  ##
  ## Example:
  ##
  ##   ```nim
  ##   var
  ##     mm, mm_full, mm_half: MemFile
  ##
  ##   mm = memfiles.open("/tmp/test.mmap", mode = fmWrite, newFileSize = 1024)    # Create a new file
  ##   mm.close()
  ##
  ##   # Read the whole file, would fail if newFileSize was set
  ##   mm_full = memfiles.open("/tmp/test.mmap", mode = fmReadWrite, mappedSize = -1)
  ##
  ##   # Read the first 512 bytes
  ##   mm_half = memfiles.open("/tmp/test.mmap", mode = fmReadWrite, mappedSize = 512)
  ##   ```
  result = default(MemFile)
  # The file can be resized only when write mode is used:
  if mode == fmAppend:
    # quit "The append mode is not supported."
    raise BadOperation

  assert newFileSize == -1 or mode != fmRead
  var readonly = mode == fmRead

  template rollback =
    result.mem = nil
    result.size = 0

  when defined(windows):
    let desiredAccess = GENERIC_READ
    let shareMode = FILE_SHARE_READ
    let flags = FILE_FLAG_RANDOM_ACCESS

    template fail(errCode: OSErrorCode, msg: string) =
      rollback()
      if not result.fHandle.isNil: discard closeHandle(result.fHandle)
      if not result.mapHandle.isNil: discard closeHandle(result.mapHandle)
      raiseOSError(errCode, msg)
      # return false
      #raise newException(IOError, msg)

    var filename = filename
    result.fHandle = createFileW(newWideCString(filename).toWideCString,
                                # GENERIC_ALL != (GENERIC_READ or GENERIC_WRITE)
                                if readonly: desiredAccess else: desiredAccess or GENERIC_WRITE,
                                if readonly: shareMode else: shareMode or FILE_SHARE_WRITE,
                                nil,
                                if newFileSize != -1: CREATE_ALWAYS else: OPEN_EXISTING,
                                if readonly: FILE_ATTRIBUTE_READONLY or flags
                                else: FILE_ATTRIBUTE_NORMAL or flags,
                                Handle 0)

    if result.fHandle == INVALID_HANDLE_VALUE:
      fail(osLastError(), "error opening file")

    if (let e = setFileSize(result.fHandle, newFileSize);
        e != 0.OSErrorCode): fail(e, "error setting file size")

    # since the strings are always 'nil', we simply always call
    # CreateFileMappingW which should be slightly faster anyway:
    result.mapHandle = createFileMappingW(
      result.fHandle, nil,
      if readonly: PAGE_READONLY else: PAGE_READWRITE,
      0, 0, nil)

    if result.mapHandle.isNil:
      fail(osLastError(), "error creating mapping")

    result.mem = mapViewOfFileEx(
      result.mapHandle,
      if readonly: FILE_MAP_READ else: FILE_MAP_READ or FILE_MAP_WRITE,
      DWORD(offset shr 32),
      DWORD(offset and 0xffffffff),
      WinSizeT(if mappedSize == -1: 0 else: mappedSize),
      nil)

    if result.mem == nil:
      fail(osLastError(), "error mapping view")

    var hi: DWORD = DWORD(0)
    let low = getFileSize(result.fHandle, addr(hi))
    if low == INVALID_FILE_SIZE:
      fail(osLastError(), "error getting file size")
    else:
      var fileSize = (int64(hi) shl 32) or int64(uint32(low))
      if mappedSize != -1: result.size = min(fileSize, mappedSize).int
      else: result.size = fileSize.int

    result.wasOpened = true
    if not allowRemap and result.fHandle != INVALID_HANDLE_VALUE:
      if closeHandle(result.fHandle).isSuccess:
        result.fHandle = INVALID_HANDLE_VALUE

  else:
    template fail(errCode: OSErrorCode, msg: string) =
      rollback()
      if result.handle != -1: discard close(result.handle)
      raiseOSError(errCode, msg)

    var flags = (if readonly: O_RDONLY else: O_RDWR) or O_CLOEXEC

    if newFileSize != -1:
      flags = flags or O_CREAT or O_TRUNC
      var permissionsMode = Mode(S_IRUSR or S_IWUSR)
      var filename = filename
      result.handle = open(filename.toCString, flags, permissionsMode)
      if result.handle != -1:
        if (let e = setFileSize(result.handle, newFileSize);
            e != 0.OSErrorCode): fail(e, "error setting file size")
    else:
      var filename = filename
      result.handle = open(filename.toCString, flags)

    if result.handle == -1:
      fail(osLastError(), "error opening file")

    if mappedSize != -1: # XXX Logic here differs from `when windows` branch ..
      result.size = mappedSize # .. which always fstats&Uses min(mappedSize, st).
    else: # if newFileSize!=-1: result.size=newFileSize # if trust setFileSize
      var stat: Stat = default(Stat) # ^^.. BUT some FSes (eg. Linux HugeTLBfs) round to 2MiB.
      if fstat(result.handle, stat) != -1:
        result.size = stat.st_size.int # int may be 32-bit-unsafe for 2..<4 GiB
      else:
        fail(osLastError(), "error getting file size")

    result.flags = if mapFlags == cint(-1): MAP_SHARED else: mapFlags
    # Ensure exactly one of MAP_PRIVATE cr MAP_SHARED is set
    if int(result.flags and MAP_PRIVATE) == 0:
      result.flags = result.flags or MAP_SHARED

    let pr = if readonly: PROT_READ else: PROT_READ or PROT_WRITE
    result.mem = mmap(nil, result.size, pr, result.flags, result.handle, offset)
    if result.mem == cast[pointer](MAP_FAILED):
      fail(osLastError(), "file mapping failed")

    if not allowRemap and result.handle != -1:
      if close(result.handle) == 0:
        result.handle = -1

proc close*(f: var MemFile) {.raises.} =
  ## closes the memory mapped file `f`. All changes are written back to the
  ## file system, if `f` was opened with write access.

  var error = false
  var lastErr {.noinit.}: OSErrorCode

  when defined(windows):
    if f.wasOpened:
      error = unmapViewOfFile(f.mem).isFail
      if not error:
        error = closeHandle(f.mapHandle).isFail
        if not error and f.fHandle != INVALID_HANDLE_VALUE:
          discard closeHandle(f.fHandle)
          f.fHandle = INVALID_HANDLE_VALUE
      if error:
        lastErr = osLastError()
  else:
    error = munmap(f.mem, f.size) != 0
    lastErr = osLastError()
    if f.handle != -1:
      error = (close(f.handle) != 0) or error

  f.size = 0
  f.mem = nil

  when defined(windows):
    f.fHandle = Handle 0
    f.mapHandle = Handle 0
    f.wasOpened = false
  else:
    f.handle = -1

  if error: raiseOSError(lastErr)

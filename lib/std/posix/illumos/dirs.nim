# Private include. libc dirent records have variable-length names and no
# d_type. Present the same synthetic entry interface as Linux; std/dirs
# already uses lstat for DT_UNKNOWN. Never overlay this on libc's record.
type
  NativeDirent {.pure.} = object
    ino: uint64
    offset: int64
    reclen: uint16
    name: array[1, char]
  Dirent* {.pure.} = object
    d_type*: uint8
    d_name*: array[256, char]
  DIR* {.pure.} = object
    stream: pointer
    entry: Dirent

proc nativeOpendir(path: cstring): nil pointer {.importc: "opendir", sideEffect.}
proc nativeReaddir(stream: pointer): nil ptr NativeDirent {.importc: "readdir", sideEffect.}
proc nativeClosedir(stream: pointer): cint {.importc: "closedir", sideEffect.}

proc opendir*(path: cstring): nil ptr DIR {.sideEffect.} =
  let stream = nativeOpendir(path)
  if stream == nil: return nil
  result = cast[ptr DIR](alloc0(sizeof(DIR)))
  result.stream = stream

proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.sideEffect.} =
  if dirp == nil:
    errnoLocation()[] = EBADF
    return nil
  errnoLocation()[] = 0
  let native = nativeReaddir(dirp.stream)
  if native == nil: return nil
  # d_name starts at byte 18 on the supported LP64 ABI. Bound the scan by
  # both the native record length and the public buffer, including its NUL.
  let available = int(native.reclen) - 18
  let name = cast[ptr UncheckedArray[char]](addr native.name[0])
  var i = 0
  while i < available and i < dirp.entry.d_name.len:
    dirp.entry.d_name[i] = name[i]
    if name[i] == '\0':
      dirp.entry.d_type = 0 # DT_UNKNOWN (an interface tag, not a native macro)
      return addr dirp.entry
    inc i
  errnoLocation()[] = EOVERFLOW
  result = nil

proc closedir*(dirp: nil ptr DIR): cint {.sideEffect.} =
  if dirp == nil:
    errnoLocation()[] = EBADF
    return -1
  result = nativeClosedir(dirp.stream)
  let saved = errno()
  dealloc(dirp)
  errnoLocation()[] = saved

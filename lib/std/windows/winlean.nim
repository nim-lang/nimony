## This module implements a small wrapper for some needed Win API procedures,
## so that the Nim compiler does not depend on the huge Windows module.

import std/widestrs

# TODO: Uncomment when passC pragma is implemented
#{.passc: "-DWIN32_LEAN_AND_MEAN".}

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(windows):

  # See https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types
  type
    Handle* {.importc: "HANDLE", header: "<WinDef.h>", nodecl.} = distinct int
    LONG* {.importc: "LONG", header: "<WinDef.h>".} = int32
    WINBOOL* = distinct int32
      ## `WINBOOL` uses opposite convention as posix, !=0 meaning success.
    DWORD* {.importc: "DWORD", header: "<WinDef.h>", nodecl.} = int32

  let
    INVALID_HANDLE_VALUE* = cast[Handle](-1)
  # TODO: error: initialization of 'HANDLE' {aka 'void * const'} from 'long long int' makes pointer from integer without a cast [-Wint-conversion]


  const
    FILE_ATTRIBUTE_READONLY* = 0x00000001'i32
    FILE_ATTRIBUTE_HIDDEN* = 0x00000002'i32
    FILE_ATTRIBUTE_SYSTEM* = 0x00000004'i32
    FILE_ATTRIBUTE_DIRECTORY* = 0x00000010'i32
    FILE_ATTRIBUTE_ARCHIVE* = 0x00000020'i32
    FILE_ATTRIBUTE_DEVICE* = 0x00000040'i32
    FILE_ATTRIBUTE_NORMAL* = 0x00000080'i32
    FILE_ATTRIBUTE_TEMPORARY* = 0x00000100'i32
    FILE_ATTRIBUTE_SPARSE_FILE* = 0x00000200'i32
    FILE_ATTRIBUTE_REPARSE_POINT* = 0x00000400'i32
    FILE_ATTRIBUTE_COMPRESSED* = 0x00000800'i32
    FILE_ATTRIBUTE_OFFLINE* = 0x00001000'i32
    FILE_ATTRIBUTE_NOT_CONTENT_INDEXED* = 0x00002000'i32

    FILE_FLAG_FIRST_PIPE_INSTANCE* = 0x00080000'i32
    FILE_FLAG_OPEN_NO_RECALL* = 0x00100000'i32
    FILE_FLAG_OPEN_REPARSE_POINT* = 0x00200000'i32
    FILE_FLAG_POSIX_SEMANTICS* = 0x01000000'i32
    FILE_FLAG_BACKUP_SEMANTICS* = 0x02000000'i32
    FILE_FLAG_DELETE_ON_CLOSE* = 0x04000000'i32
    FILE_FLAG_SEQUENTIAL_SCAN* = 0x08000000'i32
    FILE_FLAG_RANDOM_ACCESS* = 0x10000000'i32
    FILE_FLAG_NO_BUFFERING* = 0x20000000'i32
    FILE_FLAG_OVERLAPPED* = 0x40000000'i32
    FILE_FLAG_WRITE_THROUGH* = 0x80000000'i32

    MAX_PATH* = 260

    MOVEFILE_COPY_ALLOWED* = 0x2'i32
    MOVEFILE_CREATE_HARDLINK* = 0x10'i32
    MOVEFILE_DELAY_UNTIL_REBOOT* = 0x4'i32
    MOVEFILE_FAIL_IF_NOT_TRACKABLE* = 0x20'i32
    MOVEFILE_REPLACE_EXISTING* = 0x1'i32
    MOVEFILE_WRITE_THROUGH* = 0x8'i32


  # for memfiles.nim:

  const
    GENERIC_READ* = 0x80000000'i32
    GENERIC_WRITE* = 0x40000000'i32
    GENERIC_ALL* = 0x10000000'i32
    FILE_SHARE_READ* = 1'i32
    FILE_SHARE_DELETE* = 4'i32
    FILE_SHARE_WRITE* = 2'i32

    CREATE_ALWAYS* = 2'i32
    CREATE_NEW* = 1'i32
    OPEN_EXISTING* = 3'i32
    OPEN_ALWAYS* = 4'i32
    FILE_BEGIN* = 0'i32
    INVALID_SET_FILE_POINTER* = -1'i32
    NO_ERROR* = 0'i32
    PAGE_NOACCESS* = 0x01'i32
    PAGE_EXECUTE* = 0x10'i32
    PAGE_EXECUTE_READ* = 0x20'i32
    PAGE_EXECUTE_READWRITE* = 0x40'i32
    PAGE_READONLY* = 2'i32
    PAGE_READWRITE* = 4'i32
    FILE_MAP_READ* = 4'i32
    FILE_MAP_WRITE* = 2'i32
    INVALID_FILE_SIZE* = -1'i32

    DUPLICATE_SAME_ACCESS* = 2
    FILE_READ_DATA* = 0x00000001 # file & pipe
    FILE_WRITE_DATA* = 0x00000002 # file & pipe


  proc default*(x: typedesc[Handle]): Handle = Handle 0
  proc `==`(x, y: Handle): bool {.borrow.}
  func isNil*(x: Handle): bool = x == Handle 0

  proc `==`(x, y: WINBOOL): bool {.borrow.}

  func isFail*(x: WINBOOL): bool {.inline.} =
    ## Returns true if `x != 0`. Windows uses a different convention than POSIX,
    ## where `x == 0` is commonly used on success.
    x == WINBOOL 0

  func isSuccess*(a: WINBOOL): bool {.inline.} =
    not isFail(a)

  proc closeHandle*(hObject: Handle): WINBOOL {.
      importc: "CloseHandle", stdcall, header: "<Windows.h>".}
  proc getLastError*(): int32 {.
      importc: "GetLastError", stdcall, header: "<Windows.h>", sideEffect.}
  proc createFileW*(lpFileName: WideCString, dwDesiredAccess, dwShareMode: DWORD,
                    lpSecurityAttributes: pointer,
                    dwCreationDisposition, dwFlagsAndAttributes: DWORD,
                    hTemplateFile: Handle): Handle {.
      importc: "CreateFileW", stdcall, header: "<Windows.h>".}
  proc setEndOfFile*(hFile: Handle): WINBOOL {.
      importc: "SetEndOfFile", stdcall, header: "<Windows.h>".}
  proc setFilePointer*(hFile: Handle, lDistanceToMove: LONG,
                       lpDistanceToMoveHigh: ptr LONG,
                       dwMoveMethod: DWORD): DWORD {.
      importc: "SetFilePointer", stdcall, header: "<Windows.h>".}

  proc getFileSize*(hFile: Handle, lpFileSizeHigh: ptr DWORD): DWORD {.
      importc: "GetFileSize",
      stdcall, header: "<Windows.h>".}

  when defined(cpu32):
    type
      WinSizeT* = uint32
  else:
    type
      WinSizeT* = uint64

  proc mapViewOfFileEx*(hFileMappingObject: Handle, dwDesiredAccess: DWORD,
                        dwFileOffsetHigh, dwFileOffsetLow: DWORD,
                        dwNumberOfBytesToMap: WinSizeT,
                        lpBaseAddress: pointer): pointer{.
      importc: "MapViewOfFileEx", stdcall, header: "<Windows.h>".}
  proc createFileMappingW*(hFile: Handle,
                           lpFileMappingAttributes: pointer,
                           flProtect, dwMaximumSizeHigh: DWORD,
                           dwMaximumSizeLow: DWORD,
                           lpName: pointer): Handle {.
      importc: "CreateFileMappingW", stdcall, header: "<Windows.h>".}
  proc unmapViewOfFile*(lpBaseAddress: pointer): WINBOOL {.
      importc: "UnmapViewOfFile", stdcall, header: "<Windows.h>".}


  type
    WinChar* = Utf16Char
    FILETIME* = object ## CANNOT BE int64 BECAUSE OF ALIGNMENT
      dwLowDateTime*: DWORD
      dwHighDateTime*: DWORD

    WIN32_FIND_DATA* {.pure.} = object
      dwFileAttributes*: int32
      ftCreationTime*: FILETIME
      ftLastAccessTime*: FILETIME
      ftLastWriteTime*: FILETIME
      nFileSizeHigh*: int32
      nFileSizeLow*: int32
      dwReserved0: int32
      dwReserved1: int32
      cFileName*: array[0..(MAX_PATH) - 1, WinChar]
      cAlternateFileName*: array[0..13, WinChar]


  proc getFullPathNameW*(lpFileName: WideCString, nBufferLength: int32,
                        lpBuffer: WideCString,
                        lpFilePart: var WideCString): int32 {.
                        stdcall, dynlib: "kernel32",
                        importc: "GetFullPathNameW", sideEffect.}
  proc getFileAttributesW*(lpFileName: WideCString): int32 {.
                          stdcall, dynlib: "kernel32",
                          importc: "GetFileAttributesW", sideEffect.}
  proc setFileAttributesW*(lpFileName: WideCString,
                          dwFileAttributes: int32): WINBOOL {.
      stdcall, dynlib: "kernel32", importc: "SetFileAttributesW", sideEffect.}

  proc findFirstFileW*(lpFileName: WideCString,
                      lpFindFileData: var WIN32_FIND_DATA): Handle {.
      stdcall, dynlib: "kernel32", importc: "FindFirstFileW", sideEffect.}
  proc findNextFileW*(hFindFile: Handle,
                    lpFindFileData: var WIN32_FIND_DATA): int32 {.
      stdcall, dynlib: "kernel32", importc: "FindNextFileW", sideEffect.}

  proc copyFileW*(lpExistingFileName, lpNewFileName: WideCString,
                bFailIfExists: WINBOOL): WINBOOL {.
    importc: "CopyFileW", stdcall, dynlib: "kernel32", sideEffect.}

  proc moveFileW*(lpExistingFileName, lpNewFileName: WideCString): WINBOOL {.
    importc: "MoveFileW", stdcall, dynlib: "kernel32", sideEffect.}
  proc moveFileExW*(lpExistingFileName, lpNewFileName: WideCString,
                    flags: DWORD): WINBOOL {.
    importc: "MoveFileExW", stdcall, dynlib: "kernel32", sideEffect.}
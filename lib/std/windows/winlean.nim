## This module implements a small wrapper for some needed Win API procedures,
## so that the Nim compiler does not depend on the huge Windows module.

import std/widestrs

# TODO: Uncomment when passC pragma is implemented
#{.passc: "-DWIN32_LEAN_AND_MEAN".}

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(windows):

  # See https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types
  type
    Handle* {.importc: "HANDLE", header: "<WinNT.h>".} = int
    LONG* {.importc: "LONG", header: "WinNT.h".} = int32
    WINBOOL* = distinct int32
      ## `WINBOOL` uses opposite convention as posix, !=0 meaning success.
    DWORD* = int32

  const
    INVALID_HANDLE_VALUE* = Handle(-1)

  const
    FILE_ATTRIBUTE_READONLY* = 0x00000001'i32
    FILE_ATTRIBUTE_NORMAL* = 0x00000080'i32

    FILE_FLAG_RANDOM_ACCESS* = 0x10000000'i32

  # for memfiles.nim:

  const
    GENERIC_READ* = 0x80000000'i32
    GENERIC_WRITE* = 0x40000000'i32
    GENERIC_ALL* = 0x10000000'i32
    FILE_SHARE_READ* = 1'i32
    FILE_SHARE_WRITE* = 2'i32

    CREATE_ALWAYS* = 2'i32
    OPEN_EXISTING* = 3'i32
    FILE_BEGIN* = 0'i32
    INVALID_SET_FILE_POINTER* = -1'i32
    NO_ERROR* = 0'i32
    PAGE_EXECUTE* = 0x10'i32
    PAGE_READONLY* = 2'i32
    PAGE_READWRITE* = 4'i32
    FILE_MAP_READ* = 4'i32
    FILE_MAP_WRITE* = 2'i32
    INVALID_FILE_SIZE* = -1'i32

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

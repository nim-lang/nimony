## This module implements a small wrapper for some needed Win API procedures,
## so that the Nim compiler does not depend on the huge Windows module.

import std/widestrs

# TODO: Uncomment when passC pragma is implemented
#{.passc: "-DWIN32_LEAN_AND_MEAN".}

# Workaround https://github.com/nim-lang/nimony/issues/985
when defined(windows):

  # See https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types
  type
    # TODO: typedef PVOID HANDLE;
    Handle* {.importc: "HANDLE", header: "<WinDef.h>", nodecl.} = distinct int
    LONG* {.importc: "LONG", header: "<WinDef.h>".} = int32
    WINBOOL* = distinct int32
      ## `WINBOOL` uses opposite convention as posix, !=0 meaning success.
    DWORD* {.importc: "DWORD", header: "<WinDef.h>", nodecl.} = uint32

  let
    INVALID_HANDLE_VALUE* = cast[Handle](-1)
  # TODO: error: initialization of 'HANDLE' {aka 'void * const'} from 'long long int' makes pointer from integer without a cast [-Wint-conversion]


  const
    FILE_ATTRIBUTE_READONLY* = 0x00000001'u32
    FILE_ATTRIBUTE_HIDDEN* = 0x00000002'u32
    FILE_ATTRIBUTE_SYSTEM* = 0x00000004'u32
    FILE_ATTRIBUTE_DIRECTORY* = 0x00000010'u32
    FILE_ATTRIBUTE_ARCHIVE* = 0x00000020'u32
    FILE_ATTRIBUTE_DEVICE* = 0x00000040'u32
    FILE_ATTRIBUTE_NORMAL* = 0x00000080'u32
    FILE_ATTRIBUTE_TEMPORARY* = 0x00000100'u32
    FILE_ATTRIBUTE_SPARSE_FILE* = 0x00000200'u32
    FILE_ATTRIBUTE_REPARSE_POINT* = 0x00000400'u32
    FILE_ATTRIBUTE_COMPRESSED* = 0x00000800'u32
    FILE_ATTRIBUTE_OFFLINE* = 0x00001000'u32
    FILE_ATTRIBUTE_NOT_CONTENT_INDEXED* = 0x00002000'u32

    FILE_FLAG_FIRST_PIPE_INSTANCE* = 0x00080000'u32
    FILE_FLAG_OPEN_NO_RECALL* = 0x00100000'u32
    FILE_FLAG_OPEN_REPARSE_POINT* = 0x00200000'u32
    FILE_FLAG_POSIX_SEMANTICS* = 0x01000000'u32
    FILE_FLAG_BACKUP_SEMANTICS* = 0x02000000'u32
    FILE_FLAG_DELETE_ON_CLOSE* = 0x04000000'u32
    FILE_FLAG_SEQUENTIAL_SCAN* = 0x08000000'u32
    FILE_FLAG_RANDOM_ACCESS* = 0x10000000'u32
    FILE_FLAG_NO_BUFFERING* = 0x20000000'u32
    FILE_FLAG_OVERLAPPED* = 0x40000000'u32
    FILE_FLAG_WRITE_THROUGH* = 0x80000000'u32

    MAX_PATH* = 260

    MOVEFILE_COPY_ALLOWED* = 0x2'u32
    MOVEFILE_CREATE_HARDLINK* = 0x10'u32
    MOVEFILE_DELAY_UNTIL_REBOOT* = 0x4'u32
    MOVEFILE_FAIL_IF_NOT_TRACKABLE* = 0x20'u32
    MOVEFILE_REPLACE_EXISTING* = 0x1'u32
    MOVEFILE_WRITE_THROUGH* = 0x8'u32


  # for memfiles.nim:

  const
    GENERIC_READ* = 0x80000000'u32
    GENERIC_WRITE* = 0x40000000'u32
    GENERIC_ALL* = 0x10000000'u32
    FILE_SHARE_READ* = 1'u32
    FILE_SHARE_DELETE* = 4'u32
    FILE_SHARE_WRITE* = 2'u32

    CREATE_ALWAYS* = 2'u32
    CREATE_NEW* = 1'u32
    OPEN_EXISTING* = 3'u32
    OPEN_ALWAYS* = 4'u32
    FILE_BEGIN* = 0'u32
    FILE_CURRENT* = 1'u32
    FILE_END* = 2'u32
    NO_ERROR* = 0'i32
    PAGE_NOACCESS* = 0x01'u32
    PAGE_EXECUTE* = 0x10'u32
    PAGE_EXECUTE_READ* = 0x20'u32
    PAGE_EXECUTE_READWRITE* = 0x40'u32
    PAGE_READONLY* = 2'u32
    PAGE_READWRITE* = 4'u32
    FILE_MAP_READ* = 4'u32
    FILE_MAP_WRITE* = 2'u32

    DUPLICATE_SAME_ACCESS* = 2'u32
    FILE_READ_DATA* = 0x00000001'u32 # file & pipe
    FILE_WRITE_DATA* = 0x00000002'u32 # file & pipe

  let INVALID_SET_FILE_POINTER*: DWORD = cast[DWORD](-1)
  let INVALID_FILE_SIZE*: DWORD = cast[DWORD](-1)

  func default*(x: typedesc[Handle]): Handle = Handle 0
  func `==`*(x, y: Handle): bool {.borrow.}
  func isNil*(x: Handle): bool = x == Handle 0

  func `==`(x, y: WINBOOL): bool {.borrow.}

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
                    lpSecurityAttributes: nil pointer,
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
                        lpBaseAddress: nil pointer): nil pointer{.
      importc: "MapViewOfFileEx", stdcall, header: "<Windows.h>".}
  proc createFileMappingW*(hFile: Handle,
                           lpFileMappingAttributes: nil pointer,
                           flProtect, dwMaximumSizeHigh: DWORD,
                           dwMaximumSizeLow: DWORD,
                           lpName: nil pointer): Handle {.
      importc: "CreateFileMappingW", stdcall, header: "<Windows.h>".}
  proc unmapViewOfFile*(lpBaseAddress: nil pointer): WINBOOL {.
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


    BY_HANDLE_FILE_INFORMATION* = object
      dwFileAttributes*: DWORD
      ftCreationTime*: FILETIME
      ftLastAccessTime*: FILETIME
      ftLastWriteTime*: FILETIME
      dwVolumeSerialNumber*: DWORD
      nFileSizeHigh*: DWORD
      nFileSizeLow*: DWORD
      nNumberOfLinks*: DWORD
      nFileIndexHigh*: DWORD
      nFileIndexLow*: DWORD


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
  proc findClose*(hFindFile: Handle): WINBOOL {.
      stdcall, dynlib: "kernel32", importc: "FindClose", sideEffect.}

  proc copyFileW*(lpExistingFileName, lpNewFileName: WideCString,
                bFailIfExists: WINBOOL): WINBOOL {.
    importc: "CopyFileW", stdcall, dynlib: "kernel32", sideEffect.}

  proc moveFileW*(lpExistingFileName, lpNewFileName: WideCString): WINBOOL {.
    importc: "MoveFileW", stdcall, dynlib: "kernel32", sideEffect.}
  proc moveFileExW*(lpExistingFileName, lpNewFileName: WideCString,
                    flags: DWORD): WINBOOL {.
    importc: "MoveFileExW", stdcall, dynlib: "kernel32", sideEffect.}

  proc getCommandLineW*(): WideCString {.importc: "GetCommandLineW",
    stdcall, dynlib: "kernel32", sideEffect.}

  proc sleep*(dwMilliseconds: DWORD) {.importc: "Sleep", stdcall, dynlib: "kernel32", sideEffect.}

  proc getSystemTimeAsFileTime*(lpSystemTimeAsFileTime: var FILETIME) {.
    importc: "GetSystemTimeAsFileTime", dynlib: "kernel32", stdcall, sideEffect.}

  proc getFileInformationByHandle*(hFile: Handle,
    lpFileInformation: ptr BY_HANDLE_FILE_INFORMATION): WINBOOL{.
      stdcall, dynlib: "kernel32", importc: "GetFileInformationByHandle", sideEffect.}

  proc getCurrentDirectoryW*(nBufferLength: int32,
                            lpBuffer: WideCString): int32 {.
    importc: "GetCurrentDirectoryW", dynlib: "kernel32", stdcall, sideEffect.}
  proc setCurrentDirectoryW*(lpPathName: WideCString): int32 {.
    importc: "SetCurrentDirectoryW", dynlib: "kernel32", stdcall, sideEffect.}
  proc createDirectoryW*(pathName: WideCString, security: nil pointer=nil): int32 {.
    importc: "CreateDirectoryW", dynlib: "kernel32", stdcall, sideEffect.}
  proc removeDirectoryW*(lpPathName: WideCString): int32 {.
    importc: "RemoveDirectoryW", dynlib: "kernel32", stdcall, sideEffect.}
  proc deleteFileW*(lpFileName: WideCString): WINBOOL {.
    importc: "DeleteFileW", dynlib: "kernel32", stdcall, sideEffect.}

  proc createSymbolicLinkW*(lpSymlinkFileName, lpTargetFileName: WideCString,
                        flags: DWORD): int32 {.
    importc:"CreateSymbolicLinkW", dynlib: "kernel32", stdcall, sideEffect.}
  proc createHardLinkW*(lpFileName, lpExistingFileName: WideCString,
                        security: pointer=nil): int32 {.
    importc:"CreateHardLinkW", dynlib: "kernel32", stdcall, sideEffect.}

  # -------- Process / pipe / synchronization bindings for std/osproc --------
  const
    INFINITE* = 0xFFFFFFFF'u32
    WAIT_TIMEOUT* = 0x00000102'i32
    WAIT_FAILED* = 0xFFFFFFFF'i32
    WAIT_OBJECT_0* = 0'i32
    STILL_ACTIVE* = 0x00000103'i32

    STARTF_USESTDHANDLES* = 0x00000100'u32
    STARTF_USESHOWWINDOW* = 0x00000001'u32

    NORMAL_PRIORITY_CLASS* = 0x00000020'u32
    CREATE_UNICODE_ENVIRONMENT* = 0x00000400'u32
    CREATE_NO_WINDOW* = 0x08000000'u32

    SYNCHRONIZE* = 0x00100000'u32

    PIPE_ACCESS_INBOUND* = 0x00000001'u32
    PIPE_ACCESS_OUTBOUND* = 0x00000002'u32
    PIPE_ACCESS_DUPLEX* = 0x00000003'u32

    PIPE_NOWAIT* = 0x00000001'u32
    PIPE_WAIT* = 0x00000000'u32

    HANDLE_FLAG_INHERIT* = 0x00000001'u32

    MAXIMUM_WAIT_OBJECTS* = 64

    # Two's-complement bit patterns of the Win32 STD_INPUT_HANDLE (-10),
    # STD_OUTPUT_HANDLE (-11), STD_ERROR_HANDLE (-12) constants cast to DWORD.
    # Spelled out in hex so they can live in a `const` section without
    # requiring a compile-time signed→unsigned cast.
    STD_INPUT_HANDLE*: DWORD = 0xFFFFFFF6'u32
    STD_OUTPUT_HANDLE*: DWORD = 0xFFFFFFF5'u32
    STD_ERROR_HANDLE*: DWORD = 0xFFFFFFF4'u32

  type
    WOHandleArray* = array[0..MAXIMUM_WAIT_OBJECTS - 1, Handle]

    SECURITY_ATTRIBUTES* {.pure.} = object
      nLength*: int32
      lpSecurityDescriptor*: nil pointer
      bInheritHandle*: WINBOOL

    STARTUPINFO* {.pure.} = object
      cb*: int32
      lpReserved*: nil WideCString
      lpDesktop*: nil WideCString
      lpTitle*: nil WideCString
      dwX*: int32
      dwY*: int32
      dwXSize*: int32
      dwYSize*: int32
      dwXCountChars*: int32
      dwYCountChars*: int32
      dwFillAttribute*: int32
      dwFlags*: int32
      wShowWindow*: int16
      cbReserved2*: int16
      lpReserved2*: nil pointer
      hStdInput*: Handle
      hStdOutput*: Handle
      hStdError*: Handle

    PROCESS_INFORMATION* {.pure.} = object
      hProcess*: Handle
      hThread*: Handle
      dwProcessId*: int32
      dwThreadId*: int32

  proc getStdHandle*(nStdHandle: DWORD): Handle {.
    importc: "GetStdHandle", stdcall, dynlib: "kernel32", sideEffect.}

  proc getCurrentProcess*(): Handle {.
    importc: "GetCurrentProcess", stdcall, dynlib: "kernel32", sideEffect.}

  proc duplicateHandle*(hSourceProcessHandle: Handle; hSourceHandle: Handle;
                        hTargetProcessHandle: Handle;
                        lpTargetHandle: var Handle; dwDesiredAccess: DWORD;
                        bInheritHandle: WINBOOL;
                        dwOptions: DWORD): WINBOOL {.
    importc: "DuplicateHandle", stdcall, dynlib: "kernel32", sideEffect.}

  proc setHandleInformation*(hObject: Handle; dwMask: DWORD;
                             dwFlags: DWORD): WINBOOL {.
    importc: "SetHandleInformation", stdcall, dynlib: "kernel32", sideEffect.}

  proc createPipe*(hReadPipe, hWritePipe: var Handle;
                   lpPipeAttributes: var SECURITY_ATTRIBUTES;
                   nSize: DWORD): WINBOOL {.
    importc: "CreatePipe", stdcall, dynlib: "kernel32", sideEffect.}

  proc createNamedPipe*(lpName: WideCString; dwOpenMode, dwPipeMode: DWORD;
                        nMaxInstances, nOutBufferSize, nInBufferSize,
                        nDefaultTimeOut: DWORD;
                        lpSecurityAttributes: ptr SECURITY_ATTRIBUTES): Handle {.
    importc: "CreateNamedPipeW", stdcall, dynlib: "kernel32", sideEffect.}

  proc peekNamedPipe*(hNamedPipe: Handle; lpBuffer: pointer = nil;
                      nBufferSize: DWORD = 0'u32;
                      lpBytesRead: ptr DWORD = nil;
                      lpTotalBytesAvail: ptr DWORD = nil;
                      lpBytesLeftThisMessage: ptr DWORD = nil): WINBOOL {.
    importc: "PeekNamedPipe", stdcall, dynlib: "kernel32", sideEffect.}

  proc readFile*(hFile: Handle; buffer: nil pointer;
                 nNumberOfBytesToRead: int32;
                 lpNumberOfBytesRead: ptr int32;
                 lpOverlapped: nil pointer): WINBOOL {.
    importc: "ReadFile", stdcall, dynlib: "kernel32", sideEffect.}

  proc writeFile*(hFile: Handle; buffer: nil pointer;
                  nNumberOfBytesToWrite: int32;
                  lpNumberOfBytesWritten: ptr int32;
                  lpOverlapped: nil pointer): WINBOOL {.
    importc: "WriteFile", stdcall, dynlib: "kernel32", sideEffect.}

  proc createProcessW*(lpApplicationName: nil WideCString;
                      lpCommandLine: WideCString;
                      lpProcessAttributes: nil pointer;
                      lpThreadAttributes: nil pointer;
                      bInheritHandles: WINBOOL;
                      dwCreationFlags: DWORD;
                      lpEnvironment: nil pointer;
                      lpCurrentDirectory: nil WideCString;
                      lpStartupInfo: var STARTUPINFO;
                      lpProcessInformation: var PROCESS_INFORMATION): WINBOOL {.
    importc: "CreateProcessW", stdcall, dynlib: "kernel32", sideEffect.}

  proc waitForSingleObject*(hHandle: Handle;
                            dwMilliseconds: DWORD): int32 {.
    importc: "WaitForSingleObject", stdcall, dynlib: "kernel32", sideEffect.}
  proc waitForMultipleObjects*(nCount: DWORD; lpHandles: ptr Handle;
                               bWaitAll: WINBOOL;
                               dwMilliseconds: DWORD): int32 {.
    importc: "WaitForMultipleObjects", stdcall, dynlib: "kernel32", sideEffect.}

  proc getExitCodeProcess*(hProcess: Handle;
                           lpExitCode: var int32): WINBOOL {.
    importc: "GetExitCodeProcess", stdcall, dynlib: "kernel32", sideEffect.}
  proc terminateProcess*(hProcess: Handle; uExitCode: DWORD): WINBOOL {.
    importc: "TerminateProcess", stdcall, dynlib: "kernel32", sideEffect.}

  proc suspendThread*(hThread: Handle): DWORD {.
    importc: "SuspendThread", stdcall, dynlib: "kernel32", sideEffect.}
  proc resumeThread*(hThread: Handle): DWORD {.
    importc: "ResumeThread", stdcall, dynlib: "kernel32", sideEffect.}

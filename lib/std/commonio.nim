## The vocabulary shared by the two file APIs, `std/syncio` and `std/asyncio`.
## Both export this module, so a program importing both sees one `FileMode`,
## one `fmRead` and so on, rather than two that are ambiguous.
##
## Also here: what a `FileMode` and a permission set mean to the operating
## system's open call, so the two APIs cannot disagree about it.

type
  FileMode* = enum       ## The file mode when opening a file.
    fmRead,              ## Open the file for read access only.
                         ## If the file does not exist, it will not
                         ## be created.
    fmWrite,             ## Open the file for write access only.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWrite,         ## Open the file for read and write access.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWriteExisting, ## Open the file for read and write access.
                         ## If the file does not exist, it will not be
                         ## created. The existing file will not be cleared.
    fmAppend             ## Open the file for writing only; append data
                         ## at the end. If the file does not exist, it
                         ## will be created.

  FileSeekPos* = enum    ## Position relative to which seek should happen.
                         # The values are ordered so that they match with stdio
                         # SEEK_SET, SEEK_CUR and SEEK_END respectively.
    fspSet               ## Seek to absolute value
    fspCur               ## Seek relative to current position
    fspEnd               ## Seek relative to end

  FilePermission* = enum   ## File access permission, modelled after UNIX.
    fpUserExec,            ## execute access for the file owner
    fpUserWrite,           ## write access for the file owner
    fpUserRead,            ## read access for the file owner
    fpGroupExec,           ## execute access for the group
    fpGroupWrite,          ## write access for the group
    fpGroupRead,           ## read access for the group
    fpOthersExec,          ## execute access for others
    fpOthersWrite,         ## write access for others
    fpOthersRead           ## read access for others

const
  DefaultPermissions* = {fpUserRead, fpUserWrite,
                         fpGroupRead, fpGroupWrite,
                         fpOthersRead, fpOthersWrite}
    ## `0o666`: readable and writable by everyone, before the umask.

proc permissionBits*(p: set[FilePermission]): int32 =
  ## `p` as the POSIX mode bits `open(2)` takes.
  const Bits: array[FilePermission, int32] = [
    0o100'i32, 0o200, 0o400, 0o010, 0o020, 0o040, 0o001, 0o002, 0o004]
  result = 0
  for x in p: result = result or Bits[x]

when defined(windows):
  proc win32OpenArgs*(mode: FileMode): tuple[access, disposition: uint32] =
    ## The `CreateFileW` desired access and creation disposition for `mode`.
    ## FILE_APPEND_DATA makes every write land at end-of-file, the Win32
    ## counterpart of `O_APPEND`.
    const
      GenericRead = 0x80000000'u32
      GenericWrite = 0x40000000'u32
      FileAppendData = 0x00000004'u32
      CreateAlways = 2'u32
      OpenExisting = 3'u32
      OpenAlways = 4'u32
    case mode
    of fmRead: (GenericRead, OpenExisting)
    of fmWrite: (GenericWrite, CreateAlways)
    of fmReadWrite: (GenericRead or GenericWrite, CreateAlways)
    of fmReadWriteExisting: (GenericRead or GenericWrite, OpenExisting)
    of fmAppend: (FileAppendData, OpenAlways)
else:
  when defined(macosx) or defined(macos) or defined(freebsd) or
       defined(openbsd) or defined(netbsd) or defined(dragonfly):
    const
      # BSD/Darwin open(2) flags (differ from Linux; O_RDONLY/WRONLY/RDWR match).
      O_RDONLY = 0x0000'i32
      O_WRONLY = 0x0001'i32
      O_RDWR   = 0x0002'i32
      O_CREAT  = 0x0200'i32
      O_TRUNC  = 0x0400'i32
      O_APPEND = 0x0008'i32
  elif defined(sunos):
    const
      # Solaris/illumos <sys/fcntl.h>: Linux's O_CREAT bit is O_DSYNC here.
      O_RDONLY = 0x0000'i32
      O_WRONLY = 0x0001'i32
      O_RDWR   = 0x0002'i32
      O_CREAT  = 0x0100'i32
      O_TRUNC  = 0x0200'i32
      O_APPEND = 0x0008'i32
  else:
    const
      # Linux open(2) flags (stable across x86_64/arm64).
      O_RDONLY = 0'i32
      O_WRONLY = 1'i32
      O_RDWR   = 2'i32
      O_CREAT  = 0o100'i32
      O_TRUNC  = 0o1000'i32
      O_APPEND = 0o2000'i32

  proc posixOpenFlags*(mode: FileMode): int32 =
    ## The `open(2)` flags for `mode`.
    case mode
    of fmRead: O_RDONLY
    of fmWrite: O_WRONLY or O_CREAT or O_TRUNC
    of fmReadWrite: O_RDWR or O_CREAT or O_TRUNC
    of fmReadWriteExisting: O_RDWR
    of fmAppend: O_WRONLY or O_CREAT or O_APPEND

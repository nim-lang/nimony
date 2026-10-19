# Private implementation included by std/posix/posix; not a standalone module.

proc libcOpendir(name: cstring): nil pointer {.importc: "opendir$INODE64", sideEffect.}
proc libcReaddir(dirp: pointer): nil ptr Dirent {.importc: "readdir$INODE64", sideEffect.}

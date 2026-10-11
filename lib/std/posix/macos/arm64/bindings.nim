# Private implementation included by std/posix/posix; not a standalone module.

proc opendir*(name: cstring): nil ptr DIR {.importc: "opendir", sideEffect.}
proc readdir*(dirp: nil ptr DIR): nil ptr Dirent {.importc: "readdir", sideEffect.}

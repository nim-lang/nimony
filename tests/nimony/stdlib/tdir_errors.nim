# The `try*` directory ops report the OS's error, not `Success`, when they
# fail — also in a freestanding build, which has no libc `errno` to read.
import std/[dirs, os, syncio]

let d = path(getTempDir() / "nimony_tdir_errors")
discard tryRemoveFinalDir(d)
echo tryCreateFinalDir(d) == Success
echo tryCreateFinalDir(d) == NameExists
echo tryRemoveFinalDir(d) == Success
echo tryRemoveFinalDir(d) == NameNotFound
echo tryRemoveFile(d) == NameNotFound

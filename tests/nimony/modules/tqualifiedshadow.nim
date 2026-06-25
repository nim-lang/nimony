## Regression for module-qualified calls when a local routine shadows an
## imported name (boot failure in `semos.fileExists` calling `os.fileExists`).
import std/os
import std/assertions

proc fileExists*(f: string): bool {.inline.} =
  result = os.fileExists(f)

assert fileExists("nonexistent_file_12345.nim") == false

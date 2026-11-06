import std/syncio

type
  PFile {.importc: "FILE*", header: "<stdio.h>".} = ptr object

proc fopen(name, mode: cstring): nil PFile {.importc: "fopen", header: "<stdio.h>".}
proc fclose(f: PFile): cint {.importc: "fclose", header: "<stdio.h>".}
proc fputs(s: cstring; f: PFile): cint {.importc: "fputs", header: "<stdio.h>".}

when defined(windows):
  const nullDevice = cstring"NUL"
else:
  const nullDevice = cstring"/dev/null"

let f = fopen(nullDevice, "w")
if f != nil:
  echo fputs("hello", f) >= 0
  echo fclose(f)
else:
  echo "open failed"

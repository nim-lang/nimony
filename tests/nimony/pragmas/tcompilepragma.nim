# `.compile` in all of Nim's forms, with constant expressions as arguments.

import std/syncio

const depDir = "deps/compilepragma"

when defined(nimonyNoSuchDefine):
  const greeting = "-DGREETING=0"
else:
  const greeting = "-DGREETING=" & "2"

{.compile(depDir & "/a/util.c", greeting).}
{.compile: depDir & "/b/util.c".}
{.compile: (depDir & "/glob/*.c", "$1.o").}

proc utilA(): cint {.importc: "utilA".}
proc utilB(): cint {.importc: "utilB".}
proc globOne(): cint {.importc: "globOne".}
proc globTwo(): cint {.importc: "globTwo".}

echo utilA() + utilB() + globOne() + globTwo()

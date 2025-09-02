#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Can run arbitrary expressions at compile-time by using `selfExec`.

include nifprelude
import nimony_model, decls, programs, xints, semdata, renderer, builtintypes, typeprops

proc executeCall*(c: var SemContext; routine: Routine; dest: var TokenBuf; args: Cursor; info: PackedLineInfo): bool {.nimcall.} =
  result = false


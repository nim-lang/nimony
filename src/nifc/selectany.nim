#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

# included from codegen.nim

##[

Helpers for the `(selectany)` feature. There is no good portable
solution to get this feature for C so we make use of the preprocessor:

select_any.h:

#include "mod_a.h"
#include "mod_b.h"

------------------------------

mod_a.h:
#ifndef genericProc__impl
#  define genericProc__impl
#  define genericProc__a
#endif

------------------------------

mod_b.h:
#ifndef genericProc__impl
#  define genericProc__impl
#  define genericProc__b
#endif

------------------------------

mod_a.c:

#include "select_any.h"
void genericProc() // always declare header
#ifdef genericProc__a
{ ... } // provide an implementation
#else
; // implementation is elsewhere
#endif

------------------------------

mod_b.c:

#include "select_any.h"
void genericProc() // always declare header
#ifdef genericProc__b
{ ... } // provide an implementation
#else
; // implementation is elsewhere
#endif

]##

proc addHeadFile(c: var GeneratedCode; s: string) {.inline.} =
  c.headerFile.add c.tokens.getOrIncl(s)

proc addHeadFile(c: var GeneratedCode; t: PredefinedToken) {.inline.} =
  c.headerFile.add Token(t)

proc genRoutineGuardBegin(c: var GeneratedCode; name: string) =
  let guardName = name & "__" & mangle(c.m.filename.splitModulePath.name)

  inclHeader(c, "\"select_any.h\"")

  c.addHeadFile """#ifndef """
  c.addHeadFile name
  c.addHeadFile "__impl"
  c.addHeadFile NewLine

  c.addHeadFile "  #define "
  c.addHeadFile name
  c.addHeadFile "__impl"
  c.addHeadFile NewLine

  c.addHeadFile "  #define "
  c.addHeadFile guardName
  c.addHeadFile NewLine

  c.addHeadFile "#endif"
  c.addHeadFile NewLine

  c.add NewLine
  c.add "#ifdef "
  c.add guardName
  c.add NewLine


proc genRoutineGuardEnd(c: var GeneratedCode) =
  c.add "#else"
  c.add NewLine
  c.add Semicolon
  c.add "#endif"
  c.add NewLine
  c.add NewLine

#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the ability to access symbols from shared
## libraries. On POSIX this uses the `dlsym` mechanism, on Windows
## `LoadLibrary`.
##
## Examples
## ========
##
## Loading a simple C function
## ---------------------------
##
## The following example demonstrates loading a function called `greet`
## from a library that is determined at runtime based upon a language choice.
## If the library fails to load or the function `greet` is not found,
## it quits with a failure error code.
##
## ```nim
## type
##   GreetFunction = proc (): cstring {.stdcall.}
##
## proc loadGreet(lang: string) =
##   let lib =
##     case lang
##     of "french":
##       loadLib("french.dll")
##     else:
##       loadLib("english.dll")
##   assert lib != nil, "Error loading library"
##
##   let greet = cast[GreetFunction](lib.symAddr("greet"))
##   assert greet != nil, "Error loading 'greet' function from library"
##
##   echo greet()
##
##   unloadLib(lib)
## ```

{.feature: "lenientnils".}

import std/strutils

type
  LibHandle* = pointer ## A handle to a dynamically loaded library.

proc raiseInvalidLibrary*(name: cstring) {.noinline, noreturn, raises.} =
  ## Raises `IOError` when a symbol is missing. Nim's `LibraryError` has no
  ## Nimony equivalent, so this uses the `ErrorCode` model.
  raise IOError

when defined(posix):
  #
  # =========================================================================
  # This is an implementation based on the dlfcn interface.
  # The dlfcn interface is available in Linux, SunOS, Solaris, IRIX, FreeBSD,
  # NetBSD, AIX 4.2, HPUX 11, and probably most other Unix flavors, at least
  # as an emulation layer on top of native functions.
  # =========================================================================
  #
  from std/posix/posix import RTLD_NOW, RTLD_GLOBAL

  proc dlclose(lib: LibHandle): cint {.importc: "dlclose".}
  proc dlopen(path: cstring, mode: cint): LibHandle {.importc: "dlopen".}
  proc dlsym(lib: LibHandle, name: cstring): pointer {.importc: "dlsym".}

elif defined(windows):
  #
  # =======================================================================
  # Native Windows Implementation
  # =======================================================================
  #
  # Bare kernel32 imports (no `header: "<windows.h>"`), like
  # `system/dyncalls`, so the module also works on the native backend.
  proc getProcAddress(lib: LibHandle, name: cstring): pointer {.
      importc: "GetProcAddress", stdcall, dynlib: "kernel32".}
  proc freeLibrary(lib: LibHandle) {.
      importc: "FreeLibrary", stdcall, dynlib: "kernel32".}
  proc winLoadLibrary(path: cstring): LibHandle {.
      importc: "LoadLibraryA", stdcall, dynlib: "kernel32".}

else:
  {.error: "no implementation for dynlib".}

proc loadLib*(path: string, globalSymbols = false): LibHandle =
  ## Loads a library from `path`. Returns nil if the library could not
  ## be loaded.
  when defined(posix):
    var path = path
    let flags =
      if globalSymbols: RTLD_NOW or RTLD_GLOBAL
      else: RTLD_NOW
    result = dlopen(path.toCString, flags)
  elif defined(windows):
    var path = path
    result = winLoadLibrary(path.toCString)

proc loadLib*(): LibHandle =
  ## Gets the handle from the current executable. Returns nil if the
  ## library could not be loaded.
  when defined(posix):
    result = dlopen(nil, RTLD_NOW)
  elif defined(windows):
    result = winLoadLibrary(nil)

proc unloadLib*(lib: LibHandle) =
  ## Unloads the library `lib`.
  when defined(posix):
    discard dlclose(lib)
  elif defined(windows):
    freeLibrary(lib)

proc symAddr*(lib: LibHandle, name: cstring): pointer =
  ## Retrieves the address of a procedure/variable from `lib`. Returns nil
  ## if the symbol could not be found.
  when defined(posix):
    result = dlsym(lib, name)
  elif defined(windows):
    result = getProcAddress(lib, name)

proc checkedSymAddr*(lib: LibHandle, name: cstring): pointer {.raises.} =
  ## Retrieves the address of a procedure/variable from `lib`. Raises
  ## `IOError` if the symbol could not be found.
  result = symAddr(lib, name)
  if result == nil: raiseInvalidLibrary(name)

proc libCandidates*(s: string, dest: var seq[string]) {.raises: [].} =
  ## Given a library name pattern `s`, write possible library names to `dest`.
  let le = strutils.find(s, '(')
  if le >= 0:
    let ri = strutils.find(s, ')', le+1)
    if ri > le:
      let prefix = substr(s, 0, le - 1)
      let suffix = substr(s, ri + 1)
      for middle in split(substr(s, le + 1, ri - 1), '|'):
        libCandidates(prefix & middle & suffix, dest)
      return
  add(dest, s)

proc loadLibPattern*(pattern: string, globalSymbols = false): LibHandle {.raises: [].} =
  ## Loads a library with name matching `pattern`, similar to what the `dynlib`
  ## pragma does. Returns nil if the library could not be loaded.
  ##
  ## .. warning:: this proc uses the GC and so cannot be used to load the GC.
  result = nil
  var candidates = newSeq[string]()
  libCandidates(pattern, candidates)
  for c in candidates:
    result = loadLib(c, globalSymbols)
    if result != nil: break

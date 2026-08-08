#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [tables, sets, hashes, syncio, assertions]

when defined(nimony):
  {.feature: "lenientnils".}


include ".." / lib / nifprelude
include ".." / lib / compat2
import lifter
import ".." / nimony / [nimony_model, typenav, langmodes, sizeof]

export RcField, DataField

type
  EContext* = object
    dir*, main*, ext*: string
    nestedIn*: seq[(StmtKind, SymId)]
    dynlibs*: Table[StrId, seq[(SymId, StrId, SymId)]]
    strLits*: Table[string, SymId]
    newTypes*: Table[string, SymId]
    pending*: TokenBuf
    strLitBuf*: TokenBuf   ## static LongString const decls for SSO long literals
    strLitCounter*: int    ## unique suffix for strLitBuf symbols
    typeCache*: TypeCache
    sizeofCache*: SizeofCache  ## shared size-by-symbol memoization
    bits*: int
    bigEndian*: bool
    nativeBackend*: bool  ## targeting arkham+nifasm (no C): the synthesized
                          ## `main` terminates through `cExit`, and see
                          ## `dynlibIsStaticImport` below.
    isWindows*: bool      ## target OS is Windows: the entry point receives no
                          ## argc/argv/envp, and `dynlib` names an import
                          ## library rather than something to load at runtime.

    breaks*: seq[SymId] # how to translate `break`
    continues*: seq[SymId] # how to translate `continue`
    exceptLabels*: seq[SymId] # how to translate `except`
    instId*: int # per forStmt
    tmpId*: int # per proc
    resultSym*: SymId

    localDeclCounters*: int
    hoistedConsts*: Table[SymId, SymId]  ## proc-level const -> its hoisted,
                                         ## module-suffixed top-level name
    activeChecks*: set[CheckMode]
    liftingCtx*: ref LiftingCtx
    importedModuleSuffixes*: seq[string]
    initBody*: TokenBuf

proc dynlibIsStaticImport*(e: EContext): bool {.inline.} =
  ## How `{.dynlib.}` on an `importc` proc is mapped, which is hexer's call to
  ## make and depends on BOTH the target OS and the backend.
  ##
  ## On Windows a `dynlib` is an import LIBRARY: the symbol is bound through the
  ## image's import table, filled in by the loader before the first instruction
  ## of the process runs. The native backend needs the `(dynlib …)` pragma to
  ## build that table itself, so the annotation is passed on to Leng. Everywhere
  ## else it is dropped and the declaration stays an ordinary prototype:
  ##
  ##   * Windows + C/LLVM — the linker binds it from the import library
  ##     (kernel32 and friends are linked implicitly by every toolchain).
  ##   * Posix + native — a static, libc-free image has no dynamic section to
  ##     import through in the first place.
  ##
  ## The remaining case, Posix + C/LLVM, is the only one that keeps the runtime
  ## loader lowering in `trProc` (`dlopen`/`dlsym` behind a function-pointer
  ## global). Handing Windows to that lowering is not merely redundant, it
  ## cannot work: `system/dyncalls` imports `LoadLibraryA` and `GetProcAddress`
  ## from kernel32 with `dynlib`, and those two ARE the loader — resolving them
  ## through it means the module init calls `nimLoadLibrary("kernel32")`, which
  ## jumps through a function pointer that the same init only assigns further
  ## down. A call to address 0 before the process can print anything.
  e.isWindows

proc usesRuntimeDynlibLoader*(e: EContext): bool {.inline.} =
  ## True when a `dynlib` importc proc is replaced by a function-pointer global
  ## that the module init resolves at run time. The inverse of
  ## `dynlibIsStaticImport` for the C/LLVM backends; the native backend never
  ## emits loader stubs.
  not e.nativeBackend and not e.dynlibIsStaticImport

proc getTmpId*(e: var EContext): int {.inline.} =
  result = e.tmpId
  inc e.tmpId

proc error*(e: var EContext; msg: string; c: Cursor) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(c)
  when defined(debug):
    echo getStackTrace()
  quit 1

proc error*(e: var EContext; msg: string) {.noreturn.} =
  write stdout, "[Error] "
  writeLine stdout, msg
  when defined(debug):
    echo getStackTrace()
  quit 1


# The classic `takeParRi`/`skipParRi`/`loop` helpers are gone: they required
# a physical close token, which ParRi elision removes. Use the bounded-scope
# API (`into`/`takeInto`/`enterScope`+`leaveScope`) instead.

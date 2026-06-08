#
#
#           NIF Pattern Extractor (pattern-by-example)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Compiles a `.nim` file with `nimony` and prints the resulting **NIFC procs**
## from the nimcache, so an idiom written in plain Nim can be inspected as the
## lowered IR the rewrite engine (`vmrewriter`) actually sees — the basis for
## authoring rewrite patterns by example.
##
## Usage:
##
##   patextract [options] <file.nim> [<name-substring>]
##
## Prints every `(proc …)` whose symbol name contains `<name-substring>`
## (all procs if omitted). Keep idiom procs reachable through dead-code
## elimination by marking them `{.exportc.}` (or otherwise using them).
##
## Options:
##   --from:NIFFILE  skip compilation and extract directly from an existing
##                   `.c.nif`/`.oc.nif` (the `<file.nim>` arg is then optional)
##   --nimony:PATH   path to the nimony compiler (default: `bin/nimony` next to
##                   this tool, else `nimony` on PATH)
##   --nimcache:DIR  use DIR for generated files (default: a temp dir)
##   --keep          do not delete the temp nimcache afterwards
##   --shoggoth      also run the optimizer (`--opt:speed`); off by default
##                   because the optimizer is currently unstable on some input
##
## NB: the post-optimizer artifact is `<mod>.oc.nif`; without `--shoggoth` the
## tool reads the pre-optimizer `<mod>.c.nif`. It always prefers an `.oc.nif`
## if one is present.

import std / [os, osproc, strutils, assertions, syncio, parseopt]
import ".." / ".." / "lib" / nifcoreparse  # re-exports nifcore
import ".." / ".." / "gear2" / modnames    # moduleSuffix (canonical modname)
import ".." / ".." / "lib" / argsfinder    # findArgs / processPathsFile

proc findNimony(explicit: string): string =
  if explicit.len > 0: return explicit
  let here = getAppDir()
  for cand in [here / "nimony", here.parentDir.parentDir.parentDir / "bin" / "nimony"]:
    if fileExists(cand.addFileExt(ExeExt)) or fileExists(cand): return cand
  result = "nimony"   # rely on PATH

proc pathsForFile(file: string): seq[string] =
  ## Search paths from a neighbouring `nimony.paths`, mirroring hastur — so the
  ## module name we compute matches what nimony wrote.
  result = @[]
  let baseDir = file.splitFile.dir
  if baseDir.len > 0:
    let pathsFile = findArgs(baseDir, "nimony.paths")
    if pathsFile.len > 0:
      processPathsFile pathsFile, result

proc moduleNif(nimcache, nimFile: string): string =
  ## Canonical NIFC path for `nimFile` under `nimcache`, via the modnames API
  ## (`nimcache/<name>/<name>.{oc,c}.nif`, exactly as nimony/hastur compute it).
  ## Prefers the optimized `.oc.nif` when present.
  let name = moduleSuffix(nimFile, pathsForFile(nimFile))
  let dir = nimcache / name
  let oc = dir / (name & ".oc.nif")
  let c  = dir / (name & ".c.nif")
  if fileExists(oc): oc
  elif fileExists(c): c
  else: ""

proc copyStripLineInfo(dest: var TokenBuf; src: var Cursor) =
  ## Rebuild the subtree at `src` into `dest` without any `LineInfoLit`
  ## suffixes — line info is noise for pattern authoring and we never want it.
  ## `inc`/`into` skip the source's suffix tokens, and we simply never
  ## re-emit them.
  case src.kind
  of TagLit:
    dest.openTag cursorTagId(src)
    src.into:
      while src.hasMore: copyStripLineInfo(dest, src)
    dest.closeTag()
  of IntLit:    dest.addIntLit intVal(src); inc src
  of UIntLit:   dest.addUIntLit uintVal(src); inc src
  of FloatLit:  dest.addFloatLit floatVal(src); inc src
  of CharLit:   dest.addCharLit charLit(src); inc src
  of StrLit:    dest.addStrLit strVal(src); inc src
  of Symbol:    dest.addSymUse symName(src); inc src
  of SymbolDef: dest.addSymDef symName(src); inc src
  of Ident:     dest.addIdent strVal(src); inc src
  of DotToken:  dest.addDotToken(); inc src
  of ExtendedSuffix, LineInfoLit: inc src        # never a value head; drop

proc printProcs(nifFile, nameSubstr: string) =
  var buf = parseFromFile(nifFile)
  var c = buf.beginRead()
  # Top tree is the module `(stmts …)`; iterate its declarations.
  if c.kind == TagLit:
    c.into:
      while c.hasMore:
        if c.kind == TagLit and buf.tags.tagName(c.cursorTagId) == "proc":
          var name = ""
          block:
            var n = c
            inc n                               # first child = the name def
            if n.kind == SymbolDef: name = symName(n, buf.pool)
          if nameSubstr.len == 0 or nameSubstr in name:
            var one = createTokenBuf(c.subtreeWidth, buf.pool, buf.tags)
            var cc = c
            copyStripLineInfo(one, cc)
            stdout.write toString(one)
            stdout.write "\n"
        skip c

proc patMain*(args: seq[string]) =
  ## Entry for the `shoggoth pat …` subcommand. `args` are the CLI tokens after
  ## `pat`.
  # NB: `initOptParser(@[])` would fall back to the *real* command line, so an
  # empty arg list must be handled up front.
  if args.len == 0:
    quit "usage: shoggoth pat [--from:NIF] [--keep] [--shoggoth] <file.nim> [<substr>]"
  var files: seq[string] = @[]
  var nimony = ""
  var nimcache = ""
  var fromFile = ""
  var keep = false
  var runShoggoth = false
  var p = initOptParser(args)
  for kind, key, val in getopt(p):
    case kind
    of cmdArgument: files.add key
    of cmdLongOption, cmdShortOption:
      case key
      of "nimony": nimony = val
      of "nimcache": nimcache = val
      of "from": fromFile = val
      of "keep": keep = true
      of "shoggoth": runShoggoth = true
      else: quit "unknown option: " & key
    of cmdEnd: discard

  # The trailing arg is the optional name substring; with --from there is no
  # .nim file, so the first positional is the substring.
  let nameSubstr =
    if fromFile.len > 0: (if files.len > 0: files[^1] else: "")
    else: (if files.len > 1: files[1] else: "")

  if fromFile.len > 0:
    if not fileExists(fromFile): quit "no such file: " & fromFile
    printProcs(fromFile, nameSubstr)
    return

  if files.len < 1:
    quit "usage: patextract [options] <file.nim> [<name-substring>]"
  let nimFile = files[0]
  if not fileExists(nimFile): quit "no such file: " & nimFile

  let tmp = if nimcache.len > 0: nimcache
            else: getTempDir() / "patextract_" & extractFilename(nimFile).changeFileExt("")
  removeDir(tmp)
  createDir(tmp)
  defer:
    if not keep: removeDir(tmp)

  let exe = findNimony(nimony)
  var args = @["c", "-f", "--nimcache:" & tmp]
  if runShoggoth: args.add "--opt:speed"
  args.add nimFile
  # Tolerate a non-zero exit: the `.c.nif` we want is emitted before the C
  # build / optimizer, so a later-stage failure still leaves it in nimcache.
  let (outp, _) = execCmdEx(quoteShellCommand(@[exe] & args))

  let nif = moduleNif(tmp, nimFile)
  if nif.len == 0:
    stderr.write "patextract: no .c.nif/.oc.nif for '" & nimFile &
      "' (did nimony fail?). Compiler output:\n" & outp & "\n"
    quit 1
  printProcs(nif, nameSubstr)

when isMainModule:
  patMain(commandLineParams())

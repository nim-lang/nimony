#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Nifler2 parses Nim code and writes NIF. Same job as `src/nifler`, without
## linking Nim 2's compiler: the lexer is `nimlexer` and the parser is
## generated from `nimgrammar.nim`.
##
##   bin/nimony c src/nifler2/nifler2.nim
##   nifler2 p file.nim [out.nif]

import std / [syncio, assertions, os, parseopt]
import nimgrammar, niflerout, filters

const Usage = """nifler2 - Nim to NIF
Usage:
  nifler2 [options] p|parse file.nim [out.nif]   parse one file, write NIF
  nifler2 [options] deps file.nim [out.nif]      write only the deps file
  nifler2 t|tree file.nim                        parse one file, print the tree

Options:
  --portablePaths   accepted; paths are always written relative to the
                    current directory, as `nifler --portablePaths` does
  --deps            also write <out>.deps.nif, the module's dependencies
  --force, -f       accepted; the output is always rewritten
"""

proc parse(p: var Parser; inp: string) {.raises.} =
  ## `Parser` owns a `TokenBuf`, which is not copyable, so it is filled in
  ## place rather than returned.
  ## A syntax error ends the process; the lexer's and the filters' errors are
  ## only collected.
  let filename = absolutePath(inp)
  var filterFailed = false
  let src = applyFilters(readFile(inp), filename, filterFailed)
  p = openParser(src, filename, pool, globalTags)
  p.filterFailed = filterFailed
  parseModule p
  if p.failed: reportFailure p

proc report(p: Parser): bool =
  for e in p.lex.errors: echo e
  p.lex.errors.len == 0 and not p.filterFailed

proc main {.raises.} =
  var action = ""
  var args: seq[string] = @[]
  var deps = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if action.len == 0: action = key
      else: args.add key
    of cmdLongOption, cmdShortOption:
      case key
      of "portablePaths", "portablepaths", "force", "f": discard
      of "deps": deps = true
      of "help", "h": quit Usage
      else: quit "nifler2: unsupported option: " & key
    of cmdEnd: discard
  if args.len == 0: quit Usage
  let inp = args[0]
  case action
  of "p", "parse", "deps":
    var p = openParser("", inp, pool, globalTags)
    parse p, inp
    let ok = report(p)
    if not ok: quit 1
    # nifler's naming: `out.nif` as given (an extension added if missing), else
    # the input with `.nif`; the deps file replaces that extension
    let outp = if args.len >= 2: addFileExt(args[1], "nif") else: changeFileExt(inp, ".nif")
    if action != "deps":
      # nimony runs nifler with `--portablePaths`: the file is written relative
      # to the current directory
      writeNifler(p.dest, outp, relativePath(absolutePath(inp), getCurrentDir(), '/'))
    if deps or action == "deps":
      writeDeps(p.dest, changeFileExt(outp, ".deps.nif"))
  of "t", "tree":
    var p = openParser("", inp, pool, globalTags)
    parse p, inp
    echo toString(p.dest)
    if not report(p): quit 1
  else:
    quit Usage

try:
  main()
except:
  quit "nifler2: cannot read the input file"

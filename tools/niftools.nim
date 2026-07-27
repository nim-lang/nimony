#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## niftools — a grab-bag of command-line utilities for NIF / BIF artifacts.
##
## Subcommands:
##   bif2nif <in.bif> [out.nif] [--suffix:<dotted>]
##       Decode a binary `.bif` cache (`nifcore.TokenBuf` on disk, see
##       `src/lib/bif.nim`) back into canonical text NIF — the `(.nif27)` header,
##       the body, and the trailing `(.index …)`. With no `out.nif` the input path
##       is reused with a `.nif` extension.
##   nif2bif <in.nif> [out.bif] [--suffix:<dotted>] [--dense]
##       The inverse: parse a text NIF module into a `TokenBuf` and write it as a
##       binary `.bif` cache (with its symbol index). With no `out.bif` the input
##       path is reused with a `.bif` extension. A `(.unusedname X)` directive
##       (plugin protocol) is lifted into a leading `(unusedname X)` tree —
##       bif has no directive channel; see `bif.UnusedNameTag`.
##
## `--suffix:<dotted>` (e.g. `.mymod`) is the self-module suffix used to decide
## which symbols are "global" (indexed) and to compress their trailing dot,
## exactly as the text writer/reader do. `--dense` parses with per-token line
## info (what the compiler's plugin path uses), so a converted plugin input is
## token-faithful to what `plugins.loadPluginTree` reads from text.
##
## Round-trip: `bif2nif` then `nif2bif` (same `--suffix`) reproduces the input
## `.bif` byte-for-byte — *provided the NIF's filename carries the module name*,
## because a NIF reader re-expands a symbol's compressed trailing dot from the
## file's own name (`nifreader.extractModuleSuffix`), not from `--suffix`. So a
## module `mymod` must live in `mymod.nif` for its compressed symbols to expand
## back to `.mymod`; `--suffix` alone does not override that.
##
## Build:  nim c tools/niftools.nim        (run from the repo root)
## Run:    tools/niftools bif2nif foo.s.bif

import std / [os, syncio, strutils]

import ../src/lib/nifcore
import ../src/lib/bif
import ../src/lib/nifcoreparse

proc fail(msg: string) {.noreturn.} =
  stderr.writeLine "niftools: " & msg
  quit 1

proc parseConvertArgs(cmd, usage, defaultExt: string;
                      args: seq[string]): tuple[inp, outp, suffix: string, dense: bool] =
  ## Shared `<in> [out] [--suffix:<dotted>] [--dense]` parsing for the
  ## file-conversion subcommands. Validates the input exists and defaults
  ## `out` to `in` with `defaultExt`.
  var inPath, outPath, suffix = ""
  var dense = false
  for a in args:
    if a.startsWith("--suffix:"):
      suffix = a[len("--suffix:") .. ^1]
    elif a == "--dense":
      dense = true
    elif a.startsWith("-"):
      fail cmd & ": unknown option: " & a
    elif inPath.len == 0:
      inPath = a
    elif outPath.len == 0:
      outPath = a
    else:
      fail cmd & ": too many arguments"
  if inPath.len == 0:
    fail "usage: niftools " & usage
  if not fileExists(inPath):
    fail cmd & ": file not found: " & inPath
  if outPath.len == 0:
    outPath = inPath.changeFileExt(defaultExt)
  (inPath, outPath, suffix, dense)

proc bif2nifCmd(args: seq[string]) =
  let (inPath, outPath, suffix, _) = parseConvertArgs(
    "bif2nif", "bif2nif <in.bif> [out.nif] [--suffix:<dotted>]", "nif", args)
  var m = bif.load(inPath)
  # A lifted `(unusedname X)` hint tree renders as ordinary NIF text — readable
  # for debugging, and `nif2bif` of that output keeps it a tree, so conversion
  # is stable. Only the directive→tree direction needs special handling.
  writeFile(outPath, toModuleString(m.buf, suffix))
  stderr.writeLine "niftools: wrote " & outPath

proc nif2bifCmd(args: seq[string]) =
  let (inPath, outPath, suffix, dense) = parseConvertArgs(
    "nif2bif", "nif2bif <in.nif> [out.bif] [--suffix:<dotted>] [--dense]", "bif", args)
  # `parseFromFile` derives the module suffix from the filename (`rd.open`), so
  # trailing-dot symbols are re-expanded before we hand the buffer to `store`.
  var hint = ""
  var parsed = parseFromFile(inPath, hint, denseLineInfo = dense)
  if hint.len > 0:
    var buf = createTokenBuf(parsed.len + 4, parsed.pool, parsed.tags)
    buf.buildTree buf.tags.registerTag(UnusedNameTag):
      buf.addSymUse hint
    buf.addBufferSamePool(parsed)
    bif.store(buf, outPath, suffix)
  else:
    bif.store(parsed, outPath, suffix)
  stderr.writeLine "niftools: wrote " & outPath

proc main() =
  let params = commandLineParams()
  if params.len == 0:
    fail "usage: niftools <command> [args]\n  commands: bif2nif, nif2bif"
  case params[0]
  of "bif2nif": bif2nifCmd(params[1 .. ^1])
  of "nif2bif": nif2bifCmd(params[1 .. ^1])
  else: fail "unknown command: " & params[0]

main()

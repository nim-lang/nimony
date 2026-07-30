## Custom runner for the LLVM debug-info golden tests: compile each `.nim`
## with the LLVM backend (`nimony l`) and diff the DWARF-relevant metadata of
## the emitted `.ll` against the checked-in `<test>.ll.expected`.
##
## This directory is `hastur.mode = skip`, so the default `hastur all` sweep
## leaves it out (the LLVM backend cannot build the full stdlib yet); run it
## explicitly with `hastur tests/llvmdebug` (add `--overwrite` to regenerate
## the goldens after an intended debug-info change).
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0:
  toolchainDir = arg("bindir")
  skipBuild = true
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let overwrite = "--overwrite" in commandLineParams()
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

runLLVMDebugTests(dir, overwrite)

## Custom runner for the native-codegen golden tests: compile each `.nim` with
## the C-free native backend (`nimony n --opt:speed`), diff arkham's emitted
## `.asm.nif` against the checked-in `<test>.<target>.asm.nif`, and run the
## linked executable against its `.output`/`.exitcode`. hastur invokes this via
## the tree walk and passes context on argv; we reuse hastur itself as the kit.
##
## The golden is per-target — `x64`, `win_x64`, … — because emitted machine code
## is exactly what the suite pins.
##
## This directory is `hastur.mode = skip`, so the default `hastur all` sweep
## leaves it out (it needs the sibling `../nativenif` arkham/nifasm toolchain);
## run it explicitly with `hastur tests/nativecg` (add `--overwrite` to
## (re)generate the golden after an intended codegen change, or to add the one
## for a target that has none yet).
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0:
  # A prebuilt toolchain was pointed at: use it as-is, don't rebuild from source
  # (mirrors hastur's own `--bindir` implies `--no-build`).
  toolchainDir = arg("bindir")
  skipBuild = true
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let overwrite = "--overwrite" in commandLineParams()
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

runNativeCodegenTests(dir, overwrite)

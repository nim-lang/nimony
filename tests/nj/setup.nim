## Custom runner for the nj (Nimony Jump Elimination) tests: build the `nj`
## tool, then run it over every `.nif` input in this directory and diff the
## result against the sibling `.nj.nif`. hastur invokes this via the tree walk
## (`hastur tests/`) and passes context on argv; we reuse hastur itself as the
## test kit.
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let overwrite = "--overwrite" in commandLineParams()
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

buildNj()
runNifToolTests("nj", dir, ".nif", ".nj.nif", overwrite)

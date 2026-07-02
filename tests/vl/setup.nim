## Custom runner for the vl (Versioned Locations) tests: build the `vl` tool,
## run it over every `.nif` input here and diff against the sibling `.vl.nif`.
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

buildVl()
runNifToolTests("vl", dir, ".nif", ".vl.nif", overwrite)

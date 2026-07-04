## Custom runner for the hexer tests: build `hexer` and `lengc`, index the
## `.nif` inputs, lower them through hexer and run the result via lengc.
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

buildHexer()
buildLengc()
hexertests(overwrite)

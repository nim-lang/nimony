## Custom runner for the validator: build `validator`, then run it over the
## compiler pass sources (grammar/obligation checks) and the deliberately
## broken `tests/check_tags` fixtures. A directory with just this file — the
## suite validates `src/…`, not a folder of inputs.
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

buildValidator()
validatorTests()

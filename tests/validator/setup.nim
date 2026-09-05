## Custom runner for the validator: build it and nimony, then run it over the
## compiler pass sources (which must come back clean) and over the
## deliberately broken `tests/validator_sem` fixtures (which must come back
## with exactly the diagnostics they name). A directory with just this file:
## the suite validates `src/…` and a sibling fixture folder, not a folder of
## inputs of its own.
import std / [os, strutils]
import "../../src/hastur/kit"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let overwrite = "--overwrite" in commandLineParams()

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

# The validator reads what sem produced, so the suite builds nimony too and
# semchecks its inputs itself.
buildValidator()
buildNimony()
validatorTests(overwrite)

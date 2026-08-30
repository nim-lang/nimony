## Custom runner for the validator: build `validator`, then run it over the
## compiler pass sources (grammar/obligation checks) and the deliberately
## broken `tests/check_tags` fixtures — both through the untyped front end —
## and finally over `tests/validator_sem`, which exercises the semchecked one.
## A directory with just this file: the suite validates `src/…` and a sibling
## fixture folder, not a folder of inputs of its own.
import std / [os, strutils]
import "../../src/hastur/kit"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

buildValidator()
validatorTests()
# The semchecked front end needs the compiler to produce the artefacts it
# reads, so this half of the suite depends on nimony as well.
buildNimony()
semValidatorTests("--overwrite" in commandLineParams())

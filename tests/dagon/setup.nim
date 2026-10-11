## Custom runner for the dagon doc-generator tests: build `dagon`, then run
## every `t*.nim` here through `nimony doc` and check the `.assertions` sidecar.
##
## `hastur.mode = skip` keeps this directory out of `hastur all`; `hastur
## tests/dagon` still runs it (a named root overrides the mode). It is green on
## Linux and fails on Windows, unattributed — `nimony doc` is the only test
## runner here that no cross-compile reaches, so the failure has to be read off
## CI. Flip the mode file to `normal` once it is understood and fixed: a doc
## generator that builds the website deserves to be in the sweep.

import std / [os, strutils]
import "../../src/hastur/kit"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let overwrite = "--overwrite" in commandLineParams()
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

buildDagon()
dagontests(dir, overwrite)

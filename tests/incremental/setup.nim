## Custom runner for the incremental-build regression: drives `nimony c
## --report` over `sample.nim` through a fixed sequence of scenarios and
## asserts the per-phase rebuild counts. Needs a built `bin/nimony` (the tree
## walk's `tests/setup.hastur` provides it).
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

incrementalTests()

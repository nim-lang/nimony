## Custom runner for the deterministic self-host bootstrap: bin0 → bin1 → …
## → binN, each stage's nimony recompiling the whole toolchain. A directory
## with just this file. Requires a fully built `bin/` (all self + carry
## tools); the tree walk's `tests/setup.hastur` builds them first.
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

bootCmd("", withValgrind = false)

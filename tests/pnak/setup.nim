## Custom runner for the pnak package-fetcher tests: build `pnak`, then run
## every self-contained `t*.nim` integration test here.
import std / [os, strutils]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

buildPnak()
pnaktests(dir)

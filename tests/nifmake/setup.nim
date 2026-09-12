## Custom runner: an option with an empty value must not swallow the argument
## after it. `nimony` puts `--base:<dir>` in front of the rest of nifmake's
## command line, and the directory is empty when the project is a bare file
## name; a parser that takes the next word as the value loses it.
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let nifmake = (bindir / "nifmake".addFileExt(ExeExt)).quoteShell

let (expected, _) = execCmdEx(nifmake & " version")
let (actual, _) = execCmdEx(nifmake & " --base: version")
if actual == expected:
  echo "nifmake: an empty --base: does not swallow the command after it"
else:
  quit "FAILURE: `nifmake --base: version` did not print what `nifmake version` " &
       "prints; got: " & actual.splitLines[0]

#
#
#           Leng Compiler — JavaScript bundler plugin
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Standalone `{.bundle.}` linker for the JS backend: reads the project link
## manifest NIF (the one nimony's `deps.nim writeLinkManifest` emits) and
## assembles the final single-file JS bundle from the per-module JS artifacts.
##
##   jslink <linkmanifest.nif> <out.js> [runtime.js]
##
## This is the linker half of the plugin story, paired with `lengjs` (the per-
## module `{.build.}` backend tool). The compiler routes each module's Leng IR
## through `lengjs`, records each result in the manifest as a
## `(file … (kind "artifact"))` entry, then a `{.bundle.}` linker overrides the
## final link step and is handed that manifest. `jslink` concatenates the runtime,
## those artifacts (dropping each file's own `"use strict";`), and the program
## entry call into one runnable `.js` — the same assembly the real-program test
## does by hand, but driven by the manifest the compiler actually produces.
##
## Manifest grammar consumed (all string-tagged, non-Leng NIF):
##   (link (apptype S) (output S) (file S (kind S) [(flags S…)])… [(flags S…)])

import std / [os, syncio, strutils]
import ".." / lib / nifreader
import ".." / lib / stringviews   # `$`/`==` for the StringView tag of a ParLe token

type ManifestInfo = object
  apptype: string
  artifacts: seq[string]   ## paths of `kind "artifact"` files, in manifest order

proc readManifest(path: string): ManifestInfo =
  ## Walk the flat manifest token stream, collecting `kind "artifact"` files.
  ## `(file "p" (kind "k"))`: the StringLit right after `(file` is the path and
  ## the one after `(kind` classifies it; a plain C linker keeps only `"obj"`,
  ## the JS bundler keeps only `"artifact"`.
  result = ManifestInfo(apptype: "console")
  var r = nifreader.open(path)   # consumes the `(.nif27)` header
  var tok = default(ExpandedToken)
  var tag = ""
  var expectOutput = false
  var pendingFile = ""
  while true:
    r.next(tok)
    case tok.tk
    of EofToken: break
    of ParLe:
      tag = $tok.data
      if tag == "output": expectOutput = true
    of StringLit:
      let s = r.decodeStr(tok)
      if expectOutput:
        expectOutput = false            # (output "exe") — not needed for a bundle
      elif tag == "apptype":
        result.apptype = s
      elif tag == "file":
        pendingFile = s                 # (file "path" …)
      elif tag == "kind":
        if s == "artifact": result.artifacts.add pendingFile
    else:
      discard
  r.close()

proc main() =
  if paramCount() < 2:
    quit "usage: jslink <linkmanifest.nif> <out.js> [runtime.js]"
  let outp = paramStr(2)
  let runtimePath = if paramCount() >= 3: paramStr(3) else: ""
  let info = readManifest(paramStr(1))

  var bundle = "\"use strict\";\n"
  if runtimePath.len > 0:
    bundle.add readFile(runtimePath)
    if not bundle.endsWith("\n"): bundle.add "\n"
  for a in info.artifacts:
    # Drop the per-module `"use strict";` (already emitted once at the top).
    for line in readFile(a).splitLines:
      if line == "\"use strict\";": continue
      bundle.add line
      bundle.add "\n"
  # Program entry. `main` is the exportc entry the system module emits; a console
  # app is invoked with (argc, argv).
  bundle.add "main(0, []);\n"
  writeFile(outp, bundle)
  stdout.writeLine "[jslink] " & outp & ": bundled " & $info.artifacts.len &
    " module artifact(s) (apptype=" & info.apptype & ")"

main()

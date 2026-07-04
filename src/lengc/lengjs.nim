#
#
#           Leng Compiler — JavaScript backend plugin
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Standalone Leng -> JavaScript backend, structured as a `{.build.}`-schedulable
## plugin rather than an in-tree `lengc` subcommand. This is Araq's "backends are
## plugins, not codegens baked into the compiler" direction for PR #2043: Nimony
## keeps user-level pragmas verbatim in the IR, and a custom backend is a separate
## binary that reads a module's Leng IR (`.c.nif`) off the wire and emits its
## artifact, scheduled as a DAG node via `{.build(...).}` — exactly like `arkham`
## (CPU) and `ghast` (GPU/SPIR-V, nativenif PR#73).
##
##   lengjs <module.c.nif> <out.js>
##
## The argv contract matches the `.build` tool-routing convention
## (`<tool> <args…> <module.c.nif> <out>`) and Ghast's `ghast <in> <out>`, so the
## compiler can schedule this without knowing anything JS-specific. All codegen
## lives in `jscodegen`; this module is only the plugin entry point + the
## paramStr contract.

import std / [os, syncio]
import noptions
import jscodegen

proc main() =
  if paramCount() < 2:
    quit "usage: lengjs <module.c.nif> <out.js>"
  let inp = paramStr(1)
  let outp = paramStr(2)
  # A backend tool always renders the single module it is handed; which module is
  # the program's `main` is the scheduler's concern, not the plugin's, so the
  # module handed over is emitted as-is.
  var s = State(config: ConfigRef(), bits: 32)   # the JS target is a 32-bit platform
  generateJSCode(s, inp, outp, {gfMainModule})

main()

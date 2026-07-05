#
#
#           Leng Compiler — WebAssembly backend plugin
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Standalone Leng -> WebAssembly backend, structured as a `{.build.}`-schedulable
## plugin exactly like `lengjs` (JS) and `arkham` (native): it reads one module's
## Leng IR (`.c.nif`) off the wire and emits its `.wasm` artifact.
##
##   lengwasm <module.c.nif> <out.wasm>
##
## WASM is the *same* linear-memory model the JS backend uses (PR #2043) — so this
## reuses `jslayout` verbatim and only the emitted instruction differs. See
## `wasmcodegen`. The argv contract matches the other backend plugins.

import std / os
import noptions
import wasmcodegen

proc main() =
  if paramCount() < 2:
    quit "usage: lengwasm <module.c.nif> <out.wasm>"
  let inp = paramStr(1)
  let outp = paramStr(2)
  var flags = {gfMainModule}
  for i in 3 .. paramCount():
    if paramStr(i) == "--program": flags.incl gfWholeProgram
  var s = State(config: ConfigRef(), bits: 32)   # the WASM target is 32-bit
  generateWasmCode(s, inp, outp, flags)

main()

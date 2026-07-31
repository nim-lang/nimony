# Regression reproducer for an optimizer (-O) bug in the induction-variables pass.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_set_ops_iv.nim
# Expected (see topt_set_ops_iv.output):
#     219
#     33
#     11
#
# Actual (BUG): lengc rejected the module with
#     [Error] node is not a type: (err)
#
# A `char` set is 256 bits, so hexer desugars every set operation into a local
# word buffer plus a `while` loop over its 32 bytes — exactly the pointer
# strength-reduction pattern `induction_variables` looks for. That local's type
# is the *nominal* symbol hexer generated for `array[32, uint8]`, not a literal
# `(array …)`, so typing it needs a symbol lookup.
#
# `optdriver.rebuildTree` opens a typenav scope per proc but registers only the
# proc's *params*, and `induction_variables` (unlike `cse`, which registers
# locals through its aliasing walk) never registered the body's locals. So
# `getNominalType` on the `(at LOCAL iv)` base fell through to the `(err)`
# sentinel and `addPtrVarDecl` baked `(ptr (err))` into the synthesized pointer's
# declared type, which the backends then rejected.
#
# Fixed: the pass's traversal now registers each local as it walks past the
# declaration, into the same per-proc scope `rebuildTree` already put the params
# in — a decl always precedes the loops that use it.
#
# Only fails under `--opt:speed`/`--opt:size`, which enable shoggoth; at the
# default opt level the optimizer is off. `tests/nimony/opt` is the `Optimized`
# harness category, so every file here is compiled with `--opt:speed`.
#
# The three cases below are set difference, union and intersection — the same
# three operations that broke self-compilation, in treemangler.nim,
# nifbuilder.nim and semdecls.nim respectively.

import std/syncio

const
  ControlLetters = {'A', 'Z', 'E', 'S', 'O', 'U', 'X', 'R'}
  ControlChars = {'\0'..'\31'}

proc needsEscape(c: char): bool =
  # set difference
  c notin ({'a'..'z', '0'..'9', '_'} - ControlLetters)

proc isControl(c: char): bool =
  # set union
  c in (ControlChars + {'.'})

proc isLateLower(c: char): bool =
  # set intersection
  c in ({'a'..'z'} * {'p'..'z'})

proc countMatching(p: proc (c: char): bool): int =
  result = 0
  for i in 0..255:
    if p(char(i)): inc result

echo countMatching(needsEscape)   # 256 - 37 = 219
echo countMatching(isControl)     # 32 control chars + '.' = 33
echo countMatching(isLateLower)   # 'p'..'z' = 11

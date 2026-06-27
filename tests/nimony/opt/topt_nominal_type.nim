# Regression reproducer for a pre-existing optimizer (-O) bug.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_nominal_type.nim
# Expected (see topt_nominal_type.output):
#     shape with 6 sides
#     shape with 7 sides
#     135
#
# Actual (BUG): the shoggoth `--opt:speed` pipeline emits an INLINE object type
# `(object (fld …) …)` somewhere a nominal type *Symbol* is required, and lengc
# then errors:
#     gentypes.nim(475) [Error] nominal type not allowed here: (object …)
#
# NOTE: this only fails under `--opt:speed`/`--opt:size` (which enable shoggoth);
# at the default opt level the optimizer is off and the program runs fine. The
# `tests/nimony/opt` directory is the `Optimized` harness category, so every
# file here is compiled with `--opt:speed` (see hastur's `toCommand`).
#
# Fixed: lengc's var-type inference for optimizer-synthesized temps
# (codegen.nim genVarDecl) used `getType`, which navigates a nominal `Shape`
# symbol to its inline `(object …)` body; a C var must be declared by its type
# name, so it now uses `getNominalType`.
#
# Bisect notes (none of these alone reproduce; the combination does):
#   - removing either `area` or `describe` makes it pass;
#   - a single proc taking `Shape`, or the seq/loop alone, both pass.
# So the trigger is two by-value `Shape` procs (distinct return types) PLUS the
# seq-of-objects loop. The fault is in a shoggoth opt pass (scalarizer/SROA or
# CSE) spelling out a nominal object type inline.

import std/syncio

type
  Color = enum
    red, green, blue
  Shape = object
    sides: int
    color: Color

proc area(s: Shape): int =
  result = s.sides * s.sides

proc describe(s: Shape): string =
  result = "shape with " & $s.sides & " sides"

var shapes: seq[Shape] = @[]
for i in 0..<5:
  shapes.add Shape(sides: i + 3, color: green)

var total = 0
for sh in shapes:
  total = total + area(sh)
  if sh.sides > 5:
    echo describe(sh)

echo total

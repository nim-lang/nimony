# `xelim` lowers a short-circuit condition with a two-target condition compiler
# (`trCondJump`, doc/final_ir.md): `if a and f(): T else: E` becomes a flat
# guard chain of `(if c (jmp L))` plus shared `(lab L)` merges, instead of the
# old "materialise a bool, then re-test it" diamond.
#
# What can go wrong when a nested `if`/`elif` chain is flattened:
#
#  - short-circuit order: an operand behind a guard must not be evaluated (and
#    its temporaries must not be built) when the guard skips it;
#  - a `let` written inside a condition operand stays visible in the arm body,
#    yet its initialiser must still run only on the paths that reach it;
#  - a value with a destructor built inside a condition must be destroyed once,
#    at the end of that condition — not at the end of the proc, and not twice
#    (once on the guard's `jmp` and once on the fall-through).

import std / [syncio, assertions]

type Box = object
  s: string

var evals = 0
var destroys = 0

proc `=destroy`(b: Box) =
  if b.s.len > 0: inc destroys

proc `=dup`(b: Box): Box = Box(s: b.s)
proc `=copy`(dest: var Box; src: Box) = dest.s = src.s

proc make(tag: string): Box =
  inc evals
  result = Box(s: tag)

proc pure(x: int): bool = x > 0

proc chain(a, b, c: bool; n = 0): string =
  # `(a or b) and c` — the shape whose naive lowering duplicates arms
  if (a or b) and c:
    result = "T"
  elif pure(n) and make("m").s.len == 1:
    result = "U"
  else:
    result = "E"

proc withLet(k: bool; n: int): string =
  # a `let` inside the second operand: skipped when `k` is false, but its name
  # is still in scope for the arm body
  if k and (let m = n * 2; m > 4):
    result = "big " & $m
  else:
    result = "small"

proc main =
  assert chain(true, false, true) == "T"
  assert chain(false, false, false, 1) == "U"
  assert chain(false, false, false, -1) == "E"
  # the `or` short-circuits: `make` runs only for the two calls that reach the
  # second arm's condition
  echo "evals: ", evals
  echo "destroys: ", destroys
  assert evals == destroys

  echo withLet(true, 3)
  echo withLet(true, 1)
  echo withLet(false, 100)

main()

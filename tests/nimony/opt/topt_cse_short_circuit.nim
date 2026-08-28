# CSE must not hoist a load out of a short-circuited operand.
#
# Repro:   bin/nimony c -r -d:danger tests/nimony/opt/topt_cse_short_circuit.nim
# Expected (see .output): 1
#
# `c.owner != nil and c.owner.p != nil` guards the second conjunct with the
# first, so `c.owner.p` must not be evaluated when `c.owner` is nil. Shoggoth's
# CSE recorded that load as a candidate on its first occurrence and hoisted the
# temp to the first occurrence's enclosing STATEMENT — sound for an ordinary
# sub-expression, wrong here, because the whole `if` is one statement and the
# operand it came from is not reached whenever that statement is. The condition
# came out as "load c.owner.p into t; if c.owner != nil and t != nil", i.e. a
# nil dereference on every call with a nil owner, in BOTH backends (it is a
# Leng-level rewrite). `-d:danger` because that is where shoggoth runs; with
# `SHOGGOTH_DISABLE=cse` it always printed the right answer.
#
# The shapes that matter: the guarded load appears TWICE (once in the guard,
# once in the body) so it becomes a candidate at all, and `mk` hides the nil
# from the nil-tracking analysis so the call survives to run time.

import std / syncio

type
  Inner = ref object
    v: int
  Owner = object
    p: Inner
  Cur = object
    owner: ptr Owner

proc same(c: Cur; q: Inner): bool =
  if c.owner != nil and c.owner.p != nil: q == c.owner.p
  else: false

proc mk(o: ptr Owner; live: bool): Cur =
  if live: Cur(owner: o) else: Cur(owner: cast[ptr Owner](0))

proc main =
  var o = Owner(p: Inner(v: 7))
  var n = 0
  for live in [true, false]:
    let c = mk(addr o, live)
    if same(c, o.p): inc n          # live: true; nil owner: false, no crash
  echo n

main()

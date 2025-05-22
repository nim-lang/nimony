
# issue 1108

import std / syncio

type
  Configuration* = object
    paths*: seq[string]

proc dangerous(x: var string) = discard

proc delegateCmdLineOptionsValid*(c: var Configuration; cmd: var string) =
  for pv in items(toOpenArray c.paths):
    cmd.add " --path:"
    cmd.add pv
    dangerous pv # valid here because derived from `c.paths` which is mutable
    pv = "also valid"

iterator pairs(c: Configuration): (var string, var string) =
  for p in items(toOpenArray c.paths):
    yield (p, p)

proc delegateCmdLineOptionsInvalid*(c: Configuration; cmd: var string) =
  for (aa, bb) in pairs(c):
    discard

  for p in items(toOpenArray c.paths):
    cmd.add " --path:"
    cmd.add p
    dangerous p  # INVALID: Mutates p even though it came from `c` which is readonly
    p = "also not good" # likewise

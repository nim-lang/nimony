# Nifler parses Nim source with the HOST compiler's parser (`--path: "$nim"` in
# `nim.cfg`, then `compiler / syntaxes` in `bridge.nim`). That parser therefore
# has to know every bit of syntax Nimony's own sources and stdlib use -- and one
# of them, concept refinement, only exists in Nim `devel`:
#
#   IntegerArithmetic* = concept of Arithmetic     # lib/std/math.nim
#
# Nim 2.2.10's `parseTypeClass` requires at least one concept parameter before
# the `of`, so it answers `identifier expected, but got 'keyword of'` and every
# module that reaches `std/math` -- which is most of the suite, through the
# plugins -- fails to parse. Rather than demand a `devel` install (CI pins one;
# a release install is what people actually have), let `devel`'s parser stand in
# for whatever the host ships.
#
# `nimparser/parser.nim` is NOT in this repo: it is Nim's file, and a copy
# checked in here would be a fork nobody re-syncs. `hastur build nifler` checks
# it out at the commit `nimparser/upstream.commit` pins (`hastur update parser`
# moves that pin); see `syncNimParser` in `src/hastur/deps.nim`.
#
# `patchFile` redirects the module by (package, name): the host's parser lives
# in `$nim/compiler`, whose `compiler.nimble` names the package `compiler`, so
# `compiler_parser` is the key. It is a LOOKUP override, which is what this
# needs -- `compiler / syntaxes` imports `parser` unqualified and would
# otherwise resolve it next to itself in `$nim/compiler`, where no `--path` of
# ours can reach.
#
# Conditional, and that is the escape hatch: with no checked-out parser -- an
# empty pin, an offline machine, or someone who deleted the file on purpose --
# nifler builds against the host compiler's own parser, which is exactly right
# on a `devel` install and the only way out should a pinned parser ever stop
# compiling against a newer `$nim/compiler`. `thisDir()` because `fileExists` is
# relative to the CURRENT directory while `patchFile` is relative to this file's,
# and `&` rather than `os`'s `/` because a NimScript config imports nothing.
if fileExists(thisDir() & "/nimparser/parser.nim"):
  patchFile("compiler", "parser", "nimparser/parser")

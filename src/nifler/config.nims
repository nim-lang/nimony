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
# a release install is what people actually have), vendor `devel`'s parser and
# let it stand in for whatever the host ships.
#
# `patchFile` redirects the module by (package, name): the host's parser lives
# in `$nim/compiler`, whose `compiler.nimble` names the package `compiler`, so
# `compiler_parser` is the key. It is a LOOKUP override, which is what this
# needs -- `compiler / syntaxes` imports `parser` unqualified and would
# otherwise resolve it next to itself in `$nim/compiler`, where no `--path` of
# ours can reach.
#
# `nimparser/parser.nim` is verbatim upstream, so it stays diffable against the
# Nim tree; `nimparser/upstream.commit` records where it came from. Re-sync with
# a plain copy from `nim-lang/Nim` at that path.
patchFile("compiler", "parser", "nimparser/parser")

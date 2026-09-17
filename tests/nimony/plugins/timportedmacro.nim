# A macro imported from another module and used as a proc pragma — the
# `{.async.}` shape every async/effect library takes. The importer never
# semchecks the macro's declaration, so it is absent from its own
# compiledMacros and the plugin has to be found on disk, built by the
# dependency compile; and that build must agree with the outer one on module
# suffixes, which needs the compiler-internal src/lib (nifbuilder/nifreader,
# pulled in by std/macros) on the OUTER search path too. This suite's
# nimony.paths deliberately does NOT list src/lib: with the fix it must not
# have to. Without the fix: "cannot open <mod>.s.deps.nif" from the plugin
# build.
import std/syncio
import deps/mimportedmacro

proc work(x: int): int {.traced.} =
  result = x * 2

echo work(21)

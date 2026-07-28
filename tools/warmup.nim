# Prebuilt cache seed for hastur's parallel test runner.
#
# `hastur`'s `warmupSharedCache` compiles THIS file once into
# `nimcache/warmup/`, then copies the resulting bundles into every
# per-test cache directory before that test runs (`prefillFromWarmup`).
# Everything imported here is already parsed, semchecked and lowered by
# the time a test starts, so the ~700-test suite pays the `system` +
# common-stdlib compile once instead of once per test.
#
# Keep this list SMALL and only add near-universal modules. Every file
# produced here is copied into all ~700 per-test cache dirs, so a module
# used by a handful of tests costs more in prefill I/O than it saves.
# `std/assertions` (171 importers) and `std/syncio` (142) cover the bulk
# of `tests/` and `examples/`; adding `math`/`strutils` (12/7 importers)
# measurably grew the prefill without improving test time.
#
# This must stay compilable by `nimony c`. A failure here is non-fatal:
# hastur logs it and falls back to cold per-test compiles, so a silent
# breakage shows up only as the suite getting much slower.

import std/[assertions, syncio]

proc main =
  # Touch each import so nothing is dead-code-eliminated before it lands
  # in the cache.
  let s = "warmup"
  assert s.len == 6
  if s.len == 0:
    echo s

main()

# Hastur test-suite warmup: compiled once before the parallel fan-out so
# every per-test nimcache can start from a populated directory of
# system + stdlib bundles. The `import`s below are the modules that
# nearly every test pulls in (directly or transitively); leave it broad
# but stable — adding rarely-used modules just bloats the warmup
# without speeding up the common case.

import std / [syncio, os, strutils, formatfloat, math]

echo "warmup ok"

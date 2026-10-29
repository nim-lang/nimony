# A 3-module `{.cyclic.}` group (#2763): `export` of a type from a cyclically
# imported module and a generic instantiation across the cycle.
import std/syncio
import deps/mcyc3_dns
import deps/mcyc3_chan

proc reexported(r: Reader; w: Writer): bool = true

echo use()
echo reexported(Reader(), Writer())

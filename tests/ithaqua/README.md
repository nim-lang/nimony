# `tests/ithaqua` — the wasmdiff corpus

Each `.nim` file here is one fixture for `hastur wasmdiff`: the SAME source
pushed through both C-free pipelines — `nimony n` (arkham + nifasm) as the
executable oracle, and `nimony w` (ithaqua) as the backend under test — with
their stdout required to match byte for byte and their exit codes to agree.

There are no `.output` files on purpose. The expected output of a fixture is
"whatever the oracle printed on this machine, this run", which is what lets the
corpus cover things a golden file cannot hold still: `float` formatting, `rand`
draws, and the 64-bit/32-bit `int` difference between the two legs. Each fixture
therefore carries an empty `.nojoin` sidecar — without one it would be swept
into its directory's joined program, whose expected output is its members'
`.output` files concatenated, and the group would diverge on every single run
before falling back to running all 18 individually anyway.

The fixtures still run as ordinary tests in `hastur all` (unchecked output, but a
compile and a clean exit), which is free stdlib coverage on the C backend.

Two subdirectories hold quarantined one-sided failures — the sweep walks only
the top level:

* `nativebugs/` — the ORACLE is the broken side; the wasm leg is right.
* `wasmgaps/`   — ithaqua cannot compile the construct yet; the oracle is right.

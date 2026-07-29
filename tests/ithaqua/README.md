# wasmdiff fixtures

Each `*.nim` here is compiled through BOTH the native backend (`nimony n`,
arkham + nifasm — the executable oracle) and the wasm backend (`nimony w`,
hexer → dce → ithaqua in the sibling `../nativenif`), then run (natively /
under `node run_wasm.js`) and required to produce byte-identical stdout and
matching exit codes: `hastur wasmdiff`.

Two closure fixtures (heap environments with RTTI; a registry of closures
dispatched by index) are deliberately not here yet: they currently crash the
NATIVE leg of the diff (`symId on TagLit` in sem/hexer; an asm-NIF object
type mismatch in the native backend) — the wasm leg is not the blocker. They
arrive together with the closure/lambdalifting fixes that make the oracle
side hold.

`nativebugs/` quarantines minimal repros for native-backend bugs found by
this differential testing (the wasm output was correct in each case); see
its README ledger.

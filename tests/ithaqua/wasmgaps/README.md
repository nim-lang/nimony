# Wasm-backend gap repros

Each file here is a MINIMAL repro of a construct the WASM leg (`nimony w` ->
`jorogumo w`) cannot compile yet, while the native oracle handles it fine — so
it is quarantined out of `tests/ithaqua/` (wasmdiff walks only the top level)
until the renderer grows the lowering.

| file | construct | wasm failure mode |
|---|---|---|
| `try_except.nim` | `try` / bare `except` around a `.raises` call | **compiles again** since the `src/nativenif.commit` bump to the unified web back end, whose EH work restructured the guarded-pad idiom into blocks that close in reverse event order. It stays quarantined only because promotion needs a wasmdiff run (`node` on PATH) to show the two legs agree, not just that one of them builds. |

Verify one with:

    bin/nimony w --out:/tmp/x.wasm tests/ithaqua/wasmgaps/<f>.nim

Re-add the corresponding lines to the main fixtures when each gap is closed —
`str_ops.nim` is the one that gave this directory its first entry.

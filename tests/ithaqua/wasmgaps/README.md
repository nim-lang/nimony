# Wasm-backend (ithaqua) gap repros

Each file here is a MINIMAL repro of a
construct the WASM leg (`nimony w` -> ithaqua) cannot compile yet, while the
native oracle handles it fine — so it is quarantined out of `tests/ithaqua/`
(wasmdiff walks only the top level) until ithaqua grows the lowering.

| file | construct | wasm failure mode |
|---|---|---|
| `try_except.nim` | `try` / bare `except` around a `.raises` call | compile: `ithaqua: jmp to an unscanned label: \`exlab.0h2\`` — the landing-pad label is jumped to before the scan that assigns it a `block` depth. wasm `end` is positional, so the pad blocks have to nest in reverse close-event order (see `doc/ithaqua.md`), and one label is reached from outside the nest ithaqua built. |

Verify one with:

    bin/nimony w --out:/tmp/x.wasm tests/ithaqua/wasmgaps/<f>.nim

Re-add the corresponding lines to the main fixtures when each gap is closed —
`str_ops.nim` is the one that gave this directory its first entry.

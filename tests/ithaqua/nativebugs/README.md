# Native-backend (oracle) bug repros

Each file here is a MINIMAL repro of a bug in the NATIVE backend
(`nimony n` — arkham x64 + nifasm), found by the wasmdiff differential
harness while sweeping the stdlib (Phase W / WW4). In every case the
WASM leg (`nimony w`) produces the correct output; the ORACLE is the
broken side, so these are quarantined out of `tests/ithaqua/` (wasmdiff
walks only the top level) until the native backend is fixed. Re-add the
corresponding lines to the main fixtures when each bug is resolved.

| file | construct | native failure mode |
|---|---|---|
| `sort_empty.nim` | `algorithm.sort` on a len<2 seq (empty or one-element) | FIXED 2026-07-24 at the stdlib level (early return before the uninit temp buffer; the manual `dealloc b.rawData` freed a bogus pointer on ALL backends — also the root of a wasm heap-scribble). Kept as a regression file. |
| `cmp_ignore_case.nim` | `strutils.cmpIgnoreCase` | compile: `arkham x64n: scalar store rhs Undef` (register allocator hands the store a module-level-symbol Location) |
| `parse_float.nim` | `parseutils.parseBiggestFloat` | compile: nifasm `Type mismatch: expected (i 64), got nil at (mov)` |
| `unicode_ops.nim` | `unicode.runeLen` (and more of std/unicode) | wrong values at runtime (`runeLen("café naïve")` = 12, want 10), then dies mid-run |
| `float32_ops.nim` | `float32` conversion + arithmetic | wrong values (`float32(1.5)` prints `0.0`; `0.1'f32 + 0.2'f32` prints `-1.5881868e-23`) |
| `rand_float.nim` | `random.rand(float)` | prints the raw u64 draw scale (`8.15e+18`) instead of a [0,1) float |

Verify any of these one-sided (wasm only) with:

    bin/nimony w --out:/tmp/x.wasm tests/ithaqua/nativebugs/<f>.nim
    node tests/ithaqua/run_wasm.js /tmp/x.wasm

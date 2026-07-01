# M2 spike — Typed-Array linear memory

This directory validates the memory model Araq specified for the JS backend
([PR #2043](https://github.com/nim-lang/nimony/pull/2043)):

> We need the low level representation so that all low level libs that happen to
> use `cast` can work reliably. This means we base `object` and `array` etc on
> Typed Arrays.

So Nim `object`/`array`/`seq`/`string` are **not** native JS objects/arrays;
they are bytes in one growable `ArrayBuffer` — a byte-addressable linear memory,
the same shape as the C model and WebAssembly linear memory. A pointer is an
integer offset into that buffer.

It is a **spike**, not shipping code: `run.sh` here is separate from the golden
suite, and the bump allocator in `mem.js` is a stand-in for the real
`allocFixed`/mimalloc primitives (an open question to Araq). What it proves is
the *model*, and specifically that it removes the one wall the M0/M1 backend hit.

## What it proves

`model.spike.js` builds, by hand, the code the codegen must emit and asserts it
runs correctly under Node:

- **Struct layout.** `Point = object x, y: int32` → 8 bytes, `x@0`/`y@4`.
  Construction is `alloc` + typed stores at byte offsets; access is a typed load.
  (Mirrors the shipping `tdata` test's `mkpoint`, now over the buffer.)
- **SSO string, both branches.** A `string` value is 16 bytes; the low bit of
  byte 0 is the SSO discriminant (set = inline ≤15 bytes, clear = heap pointer).
  `readRawData(s)` returns a byte offset either way, and `strLen(s)` reads the
  packed length.
- **String `echo` end-to-end.** `write(stdout, s)` is `fwrite(readRawData(s), 1,
  len, stdout)` — reading raw bytes straight from the buffer. Both an inline and
  a heap string echo correctly. This is exactly `readRawData → fwrite` over the
  packed SSO representation, the path that was blocked when objects were native
  JS objects.

```
$ bash run.sh
hi spike
a longer, heap-allocated string!
spike: PASS (struct layout + SSO inline/heap string echo over linear memory)
```

## Interop boundary (`interop.spike.js`)

Backs the interop answers on the PR with running code — the split between
linear memory (Nim-native data) and a JS handle registry (real JS values):

- **JS primitives.** A `number` has a machine rep, so it is stored *unboxed* in
  the buffer; a `string` has no fixed byte shape, so the field stores a registry
  handle. (`Person { age: int32; name: jsstring }` — `age` in the buffer, `name`
  a handle.)
- **Allocator-backed factory.** JS can't carve Nim bytes itself, so a generated
  `_nim_Person_new` calls the (spike) allocator and returns an offset.
- **ES-class export.** `class Person` wraps the offset with get/set accessors and
  a `dispose()` that releases the registry root — so JS writes `new Person(30,
  "Ada")` over a Nim object living in linear memory.

```
$ node interop.spike.js
interop spike: PASS (registry + allocator factory + ES-class export)
```

## Emission over the layout pass (`structcopy.spike.js`)

Bridges the two halves the other spikes prove separately — `jslayout` (byte
offsets) and `mem.js` (linear memory) — by showing a real lowered Leng pattern
translate *mechanically* onto the computed offsets, and it pins down the one
semantic the JS backend must own that C gets for free: **aggregate value copy.**

The source (`type Point = object; x, y: int` with `mk`/`bump`) is compiled
through `nimony`, and the offsets are taken verbatim from `bin/jslayout_dump`
(`Point size=16 align=8, x@0, y@8`) — not hand-guessed. Each emission rule is
1:1 with a lowered node: `oconstr`→`alloc`+`setI64` at offsets, `dot`→`i64`
load, aggregate `var q = p`→`alloc`+`copyMem`. The load-bearing assertion is
that `bump` copies rather than aliases, so mutating the copy leaves the original
untouched — which the lowered IR does *not* pre-lower to a copy, so codegen must
emit it.

```
$ node structcopy.spike.js
structcopy spike: PASS (oconstr+dot+aggregate copyMem over layout-pass offsets)
```

## Consolidated runtime (`runtime.js` + `runtime.spike.js`)

The pieces above each proved one slice with its own throwaway shims. `runtime.js`
gathers them into the single runtime a real bundle links against (the names
`jslink` prepends ahead of the module artifacts): the allocator primitives, SSO
strings, stdio, the interop registry, and fat-pointer equality. It is a spike in
exactly one respect — the allocator is a placeholder — and that boundary is
**isolated and labelled** in a boxed `ALLOCATOR — RESOLVED` section. Araq settled
it (PR #2043): the native Nim allocator is *compiled to JS* like any other Nim
code, so `allocFixed`/`dealloc` come from the pipeline over linear memory — there
is essentially no allocator ABI. The JS runtime provides only the OS pages it
sits on, `mmap`/`munmap`, and because the allocator does not assume one
contiguous growable block, those pages can be independent non-growable
TypedArray regions (no resizable `ArrayBuffer` required). The spike keeps a
bump-over-`mmap` stand-in for the compiled allocator so the surface runs.
`runtime.spike.js` exercises every section as one module.

```
$ node runtime.spike.js
runtime spike: PASS (allocator ABI surface + strings + stdio + registry + ptrs)
```

## Files

- `mem.js` — the linear-memory runtime: resizable `ArrayBuffer`, bump `alloc`,
  little-endian typed load/store, `copy` (memcpy), `bytes` (a raw view for
  `fwrite`).
- `model.spike.js` — struct layout + SSO string echo over linear memory.
- `interop.spike.js` — the JS-interop boundary: registry + factory + ES class.
- `structcopy.spike.js` — buffer emission over the layout-pass offsets:
  `oconstr`/`dot`/aggregate `copyMem`, proving value-copy semantics.
- `runtime.js` — the consolidated runtime surface (allocator ABI boundary +
  strings + stdio + registry + pointers); `runtime.spike.js` exercises it.

## What this makes concrete for the codegen

The remaining M2 work now has a definite target:

1. **Layout pass** — compute `sizeof`/alignment/field-offsets for Leng types
   (C ABI). New: the C and LLVM backends defer struct layout to their toolchains,
   so there is no existing offset computation to reuse.
2. **Emit against the buffer** — rework `oconstr`/`aconstr`/`dot`/`at` from JS
   object/array literals (`{f:v}`, `[e0,e1]`, `obj.f`, `arr[i]`) to
   `alloc` + `mem.setX(p+off, …)` / `mem.getX(p+off)` at the computed offsets.
3. **Allocator** — replace the bump allocator with the real allocation ABI
   (pending Araq's steer: allocator-over-`ArrayBuffer` vs. a minimal shim).
4. **Interop layer** — `jsstring`/`importjs`, with the existing fat-pointer
   `[base,key]` model kept for `var` parameters over native JS objects.

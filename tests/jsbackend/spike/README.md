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

## Files

- `mem.js` — the linear-memory runtime: resizable `ArrayBuffer`, bump `alloc`,
  little-endian typed load/store, `copy` (memcpy), `bytes` (a raw view for
  `fwrite`).
- `model.spike.js` — the hand-written demonstration above.

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

#!/usr/bin/env bash
# Regression test for the Leng JavaScript backend.
#
# The backend is a standalone `{.build.}`-schedulable plugin (`bin/lengjs`, the
# Ghast-style delivery Araq specified in PR #2043), invoked as
#   lengjs <module.c.nif> <out.js>
# — the same argv contract the compiler's `.build` scheduler uses. Each test is a
# hand-authored, self-contained Leng module (no system deps), so the suite needs
# only `bin/lengjs` and `node`.
#
# For each module: (1) golden check — generated JS must match the checked-in
# `.expected.js`; (2) functional check — the JS runs under Node and computes the
# expected values.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
lengjs="$root/bin/lengjs"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Generate <name>.js from <name>.c.nif via the plugin and golden-check it.
gen() {
  local name="$1"
  "$lengjs" "$here/$name.c.nif" "$work/$name.js"
  if ! diff -u "$here/$name.expected.js" "$work/$name.js"; then
    echo "FAILURE: generated JS differs from golden $name.expected.js"
    exit 1
  fi
}

have_node() { command -v node >/dev/null 2>&1; }

# ── M0/M1: pure compute (procs, locals, arithmetic, control flow) ────────────
gen tcompute
if have_node; then
  {
    cat "$work/tcompute.js"
    echo 'if (add_0_tcompute(2,3)===5 && fib_0_tcompute(10)===55 && fib_0_tcompute(20)===6765)'
    echo '  { console.log("functional: PASS"); }'
    echo 'else { console.log("functional: FAIL"); process.exit(1); }'
  } | node
else
  echo "node not found; skipped functional check (golden check passed)"
fi

# ── data structures over linear memory: object construction + field access and
# array construction + indexing, all laid out in the buffer (declared types).
gen tdata
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tdata.js"
    echo 'if (mkpoint_0_tdata(3,4)===7 && arrsum_0_tdata()===60)'
    echo '  { console.log("functional(data): PASS"); }'
    echo 'else { console.log("functional(data): FAIL"); process.exit(1); }'
  } | node
fi

# ── objects over LINEAR MEMORY (M2, Araq's Typed-Array model): a declared object
# is laid out in an ArrayBuffer — construction is `allocFixed` + typed stores at
# the field byte-offsets `jslayout` computes, field access is a typed load.
gen tbuffer
if have_node; then
  {
    # tiny linear-memory runtime (the shape runtime.js provides): allocFixed over
    # an ArrayBuffer + typed load/store. mkpoint(3,4) builds a Point in the buffer
    # and reads its fields back -> 7.
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tbuffer.js"
    echo 'if (mkpoint_0_tbuffer(3,4)===7 && mkpoint_0_tbuffer(20,22)===42)'
    echo '  { console.log("functional(buffer): PASS"); }'
    echo '  else { console.log("functional(buffer): FAIL got "+mkpoint_0_tbuffer(3,4)); process.exit(1); }'
  } | node
fi

# ── objects, full story (M2): object params + field reads (psum), a Nim value
# copy `var q = p` (mem.copy) and field write `q.x = q.x + 1`, all over the
# buffer. The value-semantics assertion: mkbump returns q.x + p.x = (a+1) + a, so
# an aliasing bug (q sharing p's storage) would give (a+1) + (a+1) instead.
gen tstructcopy
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n) };'
    cat "$work/tstructcopy.js"
    echo 'if (drive_0_tsc(3,4)===7 && mkbump_0_tsc(5,9)===11 && mkbump_0_tsc(20,0)===41)'
    echo '  { console.log("functional(objects): PASS"); }'
    echo '  else { console.log("functional(objects): FAIL got "+mkbump_0_tsc(5,9)); process.exit(1); }'
  } | node
fi

# ── aggregate ASSIGNMENT is a byte copy, not an offset alias. `dst = src` where
# both are buffer-resident aggregates must `mem.copy` — otherwise `dst` would
# alias `src`'s bytes, and an ARC move (which `wasMoved`s the source right after
# the assignment) would zero `dst` too. `copytest` copies `src`{a:10} into `dst`,
# then sets `src.a = 999`; `dst.a` must still read 10.
gen tasgncopy
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n) };'
    cat "$work/tasgncopy.js"
    echo 'if (copytest_0_tasgncopy()===10)'
    echo '  { console.log("functional(asgncopy): PASS"); }'
    echo '  else { console.log("functional(asgncopy): FAIL got "+copytest_0_tasgncopy()); process.exit(1); }'
  } | node
fi

# ── strings-through-pointers (the system.nim string idiom): a proc takes a
# `ptr Str` and reads a field via `(dot (deref s) bytes)` — the base is a
# `(deref sym)`, not a bare symbol — and reads the SSO length byte through
# `deref(cast (ptr u8) (addr …))`, whose width must come from the cast's `(ptr
# u8)`, not default to i64. `Str` mirrors the real 16-byte string (bytes:u64 @0,
# more:ptr @8); `bytes` is packed like "hi" (len 2, 'h','i'). This is exactly what
# made `echo "hi"` finally print: len reads 2 (not the whole word), so the small-
# string branch is taken and the data pointer is `base+1`.
gen tstrptr
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u8At:(p)=>_dv.getUint8(p) };'
    cat "$work/tstrptr.js"
    # drive = slen(2) + firstchar('h'==104) = 106. A width bug (i64 read) would send
    # slen down the heap branch; a fat-pointer base bug would crash on `(deref s)`.
    echo 'if (drive_0_tstrptr()===106)'
    echo '  { console.log("functional(strptr): PASS"); }'
    echo '  else { console.log("functional(strptr): FAIL got "+drive_0_tstrptr()); process.exit(1); }'
  } | node
fi

# ── array indexing through a pointer `(at a i)` where `a: ptr [N]T` — how an array
# is passed by reference (seqimpl's `@` array->seq conversion indexes the source
# this way). Element access steps through the pointer: `a + i*stride`, not the
# legacy `a[i]`. A pointer to `[7,11,13]` sums to 31. (Regression guard for the
# seq-of-objects bug: aggregate elements read as zero when this fell to `a[i]`.)
gen tptrarray
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tptrarray.js"
    echo 'if (driver_0_tptrarray()===31) { console.log("functional(ptrarray): PASS"); }'
    echo 'else { console.log("functional(ptrarray): FAIL got "+driver_0_tptrarray()); process.exit(1); }'
  } | node
fi

# ── function pointers (proc value + indirect call) — the closure dispatch path.
# A proc taken as a value becomes a function-table index (`_fnid(fn)`); a call
# through a proc variable is `_fns[idx](args)` (JS can't call an integer). `dbl` is
# called indirectly through `f`: dbl(21) = 42.
gen tfnptr
if have_node; then
  {
    echo 'const _fns=[null]; const _fnmap=new Map();'
    echo 'function _fnid(fn){ let i=_fnmap.get(fn); if(i===undefined){ i=_fns.length; _fns.push(fn); _fnmap.set(fn,i); } return i; }'
    cat "$work/tfnptr.js"
    echo 'if (callit_0_tfnptr()===42) { console.log("functional(fnptr): PASS"); }'
    echo 'else { console.log("functional(fnptr): FAIL got "+callit_0_tfnptr()); process.exit(1); }'
  } | node
fi

# ── flexarrays over the buffer — a `(flexarray T)` `(aconstr …)` is allocated by
# element count (its fixed layout is 0) and filled by typed stores. This backs the
# RTTI/vtable tables closures need. `[100,200,300]` -> reads sum to 600.
gen tflex
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tflex.js"
    echo 'if (flx_0_tflex()===600) { console.log("functional(flex): PASS"); }'
    echo 'else { console.log("functional(flex): FAIL got "+flx_0_tflex()); process.exit(1); }'
  } | node
fi

# ── whole-aggregate store through a pointer `(deref p) = obj` — the `new`'d
# heap-cell initialisation path (`p[] = SomeObject(…)`). A `mem.copy` of the
# object's bytes, not the empty-accessor `mem.()` that a scalar store would emit
# for an aggregate pointee. Writes a Pt through a pointer, reads both fields back:
# 11 + 31 = 42.
gen tderefstore
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n) };'
    cat "$work/tderefstore.js"
    echo 'if (thru_0_tderefstore()===42)'
    echo '  { console.log("functional(derefstore): PASS"); }'
    echo '  else { console.log("functional(derefstore): FAIL got "+thru_0_tderefstore()); process.exit(1); }'
  } | node
fi

# ── pointer-indexed stores `(pat p i) = v` — the seq/openarray element-write path
# (`s[i] = v` after the seq's data pointer is loaded). A typed store at
# `p + i*stride`, not the `mem.i64n(...) = v` load-as-lvalue that slipped through
# before this case existed. Writes three i64 through a pointer into a stack array
# and reads them back: 100 + 200 + 300 = 600.
gen tpatstore
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tpatstore.js"
    echo 'if (patrw_0_tpatstore()===600)'
    echo '  { console.log("functional(patstore): PASS"); }'
    echo '  else { console.log("functional(patstore): FAIL got "+patrw_0_tpatstore()); process.exit(1); }'
  } | node
fi

# ── long strings (>14 chars): a static long-string literal's `data` flexarray is
# written into linear memory. `LStr {len; data: flexarray char}` is allocated
# with room for the payload (fixed 8 + "hello"=5 -> 13 bytes), the codegen emits
# `mem.writeStr(dest, "hello")`, and the element read `data[1]` ('e'==101) plus
# len (5) = 106. This is the construction side of the heap-string path (the read
# side — deref-through-pointer field chains — is exercised by the real programs).
gen tstrlong
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); const _u8 = new Uint8Array(_dv.buffer); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; _u8.fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  u8At:(p)=>_dv.getUint8(p), writeStr:(p,s)=>{ for(let i=0;i<s.length;i++) _u8[p+i]=s.charCodeAt(i); } };'
    cat "$work/tstrlong.js"
    echo 'if (mk_0_tstrlong()===106)'
    echo '  { console.log("functional(strlong): PASS"); }'
    echo '  else { console.log("functional(strlong): FAIL got "+mk_0_tstrlong()); process.exit(1); }'
  } | node
fi

# ── exceptions (goto-style EH -> labeled blocks, Araq's PR #2043 direction):
# Hexer lowers try/except to an error-code ABI — a raising call returns an
# ErrorCode (`var canRaise = mayFail(r)`), `if canRaise.err` does a forward `jmp`
# to a handler that lives inside a dead `if (false)` landing pad. The backend maps
# those `jmp`/`lab`s to `break` in nested labeled blocks (no relooper): `jmp L` ->
# `break L`, the normal path `break`s a synthetic skip label past the handler.
# `run(false)` returns the value (42); `run(true)` takes the handler (-1). The
# body's locals are `var` (function-scoped) so they survive the block nesting.
gen texc
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);},'
    echo '  copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n) };'
    cat "$work/texc.js"
    echo 'if (run_0_texc(false)===42 && run_0_texc(true)===-1)'
    echo '  { console.log("functional(exceptions): PASS"); }'
    echo '  else { console.log("functional(exceptions): FAIL got "+run_0_texc(false)+","+run_0_texc(true)); process.exit(1); }'
  } | node
fi

# ── labeled-block var hoisting: a `(lab …)` (a loop break/continue target, or a
# goto pad) makes the codegen wrap statements in a labeled block. A function-
# scoped local declared before the label but read after it must be a `var` (whole
# function), not a block-scoped `let` — otherwise it is out of scope at the read
# (this is what broke `result` in stdlib loops, e.g. `std/tables` resize and the
# float `default`). `compute` sets `result`=41 before `done:`, then reads it after.
gen tlabhoist
if have_node; then
  {
    cat "$work/tlabhoist.js"
    echo 'if (compute_0_tlabhoist()===42)'
    echo '  { console.log("functional(labhoist): PASS"); }'
    echo '  else { console.log("functional(labhoist): FAIL got "+compute_0_tlabhoist()); process.exit(1); }'
  } | node
fi

# ── inheritance (object layout + construction): a `Derived` embeds its `Base` at
# offset 0, so an INHERITED field is read at the base's offset, and the oconstr's
# positional base initializer `(oconstr Base …)` is constructed in place at offset
# 0. `objectFields` walks the base chain (so `d.k` — `k` declared on `Base` — is
# found) and `baseobj` is a no-op on the offset. `mk(30,12)` builds a `Derived`
# {k:30 (inherited), m:12} and `sum` reads k + m = 42.
gen tinherit
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tinherit.js"
    echo 'if (mk_0_tinherit(30,12)===42)'
    echo '  { console.log("functional(inheritance): PASS"); }'
    echo '  else { console.log("functional(inheritance): FAIL got "+mk_0_tinherit(30,12)); process.exit(1); }'
  } | node
fi

# ── variant / `case` objects: the branch fields live in a `union` whose members
# are anonymous objects. `jslayout` overlays the branches (every branch's fields
# share the union's offset) and flattens them into the field map, so an
# `oconstr`/`dot` on a branch field lands at the right byte offset. `x` (a normal
# field) at 0, the discriminant `k` at 8, and the union at 16; `mk(0,9)` builds
# `N(x:100, k:0, a:9)` and returns `x + a` = 109 (a would clobber x at offset 0
# if the branch field were mis-laid).
gen tvariant
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);}, setU8:(p,v)=>_dv.setUint8(p,v&0xff) };'
    cat "$work/tvariant.js"
    echo 'if (mk_0_tvariant(0,9)===109)'
    echo '  { console.log("functional(variant): PASS"); }'
    echo '  else { console.log("functional(variant): FAIL got "+mk_0_tvariant(0,9)); process.exit(1); }'
  } | node
fi

# ── addresses (unified byte-pointer model): a pointer is an INTEGER byte offset.
# An address-taken scalar local is spilled to a buffer slot, so `addr x` is its
# offset and `deref` is a typed load/store — no boxing, no fat pointers.
gen taddr
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/taddr.js"
    # through: *p+1 on a spilled local; usebump: mutate via pointer param;
    # addrparam: a value param whose address is taken is spilled at entry.
    echo 'if (through_0_taddr()===42 && usebump_0_taddr()===15 && addrparam_0_taddr(7)===99)'
    echo '  { console.log("functional(addr): PASS"); }'
    echo 'else { console.log("functional(addr): FAIL"); process.exit(1); }'
  } | node
fi

# ── addresses (aggregates, byte pointers): addr of an object field is `base+off`,
# of an array element is `base + i*stride`; pointer equality is integer `===`.
gen taddr2
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/taddr2.js"
    # fieldaddr: write through addr of a field; elemaddr: through addr of an
    # element; samefield/difffield: integer pointer equality (same vs different).
    echo 'if (fieldaddr_0_taddr2()===100 && elemaddr_0_taddr2()===99 &&'
    echo '    samefield_0_taddr2()===true && difffield_0_taddr2()===false)'
    echo '  { console.log("functional(addr2): PASS"); }'
    echo '  else { console.log("functional(addr2): FAIL"); process.exit(1); }'
  } | node
fi

# ── importc/exportc: an `importc` proc/global names an external entity, so it is
# referenced by its C name and not emitted (a runtime provides it); an `exportc`
# symbol is emitted under its C name. Resolution is cross-module in real builds.
gen timportc
if have_node; then
  {
    # the runtime supplies the importc `extTriple`; `triple` itself is not emitted.
    # `counter` is a module-level global -> it lives in a buffer slot (boxed the
    # same in every module), so it needs the linear-memory runtime and is read
    # via `mem.i64n`, not by bare name.
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    echo 'function extTriple(x){return x*3;}'
    cat "$work/timportc.js"
    echo 'if (run_0_timportc()===21 && mem.i64n(counter)===21)'
    echo '  { console.log("functional(ffi): PASS"); }'
    echo '  else { console.log("functional(ffi): FAIL"); process.exit(1); }'
  } | node
fi

# ── cross-module boxing consistency: a module-level global scalar has static
# storage, so in the linear-memory model it lives in a buffer slot in EVERY
# module that touches it — the boxing decision follows the decl kind, not where
# `(addr)` textually appears. Here one proc mutates `slot` through its ADDRESS
# and another READS it plainly; both must go through `mem` on the same slot, so
# the write is visible to the plain read. (Before the fix, a use in a module
# that didn't take the address read the raw slot offset instead of the value —
# the blocker for cross-module heap exceptions, whose `exc` threadvar is boxed
# in `system` yet used bare in the raising module. True cross-module wiring is
# verified end-to-end by the heap-exception programs, which need the full
# toolchain and so live outside this self-contained suite.)
gen tgmoddef
if have_node; then
  {
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)), setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p), setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true), setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true), setU8:(p,v)=>_dv.setUint8(p,v&0xff), u8At:(p)=>_dv.getUint8(p), setU16:(p,v)=>_dv.setUint16(p,v,true), u16:(p)=>_dv.getUint16(p,true), setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true), setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)), i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true), setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true), copy:(d,s,n)=>new Uint8Array(_dv.buffer).copyWithin(d,s,s+n), writeStr:(p,s)=>{for(let i=0;i<s.length;i++)new Uint8Array(_dv.buffer)[p+i]=s.charCodeAt(i);} };'
    cat "$work/tgmoddef.js"
    echo 'bumpViaAddr_0_tgmoddef(5);'            # slot: 7 -> 12 via its address
    echo 'if (defget_0_tgmoddef()===12 && mem.i64n(slot)===12)'  # plain read agrees
    echo '  { console.log("functional(gmodbox): PASS"); }'
    echo '  else { console.log("functional(gmodbox): FAIL"); process.exit(1); }'
  } | node
fi

# ── checked arithmetic: `keepovf`/`ovf` (overflow-checked ops) reduce to a
# plain assignment (JS has no 64-bit overflow trap; `ovf` is always false), and
# `store` assigns with its operands reversed relative to `asgn`.
gen tarith
if have_node; then
  {
    cat "$work/tarith.js"
    echo 'if (checkedAdd_0_tarith(20,22)===42 && storeTest_0_tarith()===42)'
    echo '  { console.log("functional(arith): PASS"); }'
    echo '  else { console.log("functional(arith): FAIL"); process.exit(1); }'
  } | node
fi

# ── 64-bit ints as BigInt (Araq's steer: `int`/`uint` are 32-bit `Number`, only
# explicit `int64`/`uint64` become `BigInt`). Guards the exact op that used to
# crash `echo <float>`: a `uint64` mask + shift past 2^53 (3.5's real bit pattern
# -> exponent 1024), plus a multiply that overflows 2^53 (exact, wrapped mod 2^64).
gen tbig64
if have_node; then
  {
    cat "$work/tbig64.js"
    echo 'if (mul64_0_tbig64(3000000000n,4000000000n)===12000000000000000000n && extractExp_0_tbig64(4615063718147915776n)===1024n)'
    echo '  { console.log("functional(big64): PASS"); }'
    echo '  else { console.log("functional(big64): FAIL got "+mul64_0_tbig64(3000000000n,4000000000n)+" "+extractExp_0_tbig64(4615063718147915776n)); process.exit(1); }'
  } | node
fi

# ── float arithmetic: `/` must be real division, not integer truncation
# (`fdiv(10,4)` = 2.5, not 2). `float` values are JS Numbers, no coercion.
gen tfloat
if have_node; then
  {
    cat "$work/tfloat.js"
    echo 'if (fdiv_0_tfloat(10.0,4.0)===2.5 && Math.abs(fcompute_0_tfloat(10.0,4.0)-14/6)<1e-9)'
    echo '  { console.log("functional(float): PASS"); }'
    echo '  else { console.log("functional(float): FAIL got "+fdiv_0_tfloat(10.0,4.0)); process.exit(1); }'
  } | node
fi

# ── narrowing integer conversions mask to the target width (`uint8(300)`==44,
# `int8(200)`==-56, sign-extended). Source is a 32-bit Number here.
gen tconv
if have_node; then
  {
    cat "$work/tconv.js"
    echo 'if (toU8_0_tconv(300)===44 && toI8_0_tconv(200)===-56 && toU16_0_tconv(70000)===4464)'
    echo '  { console.log("functional(conv): PASS"); }'
    echo '  else { console.log("functional(conv): FAIL got "+toU8_0_tconv(300)+" "+toI8_0_tconv(200)); process.exit(1); }'
  } | node
fi

# ── echo (end-to-end I/O): mirrors how a real `echo <int>` lowers — Nim procs
# (`echoInt`/`run`) call `importc` stdio (`stdout`, `putInt`, `putChar`) that a
# runtime provides. Proves the generated code executes and produces output.
gen techo
if have_node; then
  {
    # tiny runtime: capture stdout instead of writing it, then assert the text.
    echo 'let _out="";'
    echo 'const stdout = {};'
    echo 'function rtPutInt(f, n) { _out += String(n); }'
    echo 'function rtPutChar(f, c) { _out += String.fromCharCode(c); }'
    cat "$work/techo.js"
    echo 'run_0_techo();'
    echo 'if (_out === "55\n5\n") { console.log("functional(echo): PASS"); }'
    echo 'else { console.log("functional(echo): FAIL got " + JSON.stringify(_out)); process.exit(1); }'
  } | node
fi

# ── type layout (M2, Typed-Array model): the C-ABI sizeof/alignment/field-offset
# computation `jslayout` performs so object/array values can be laid out in an
# ArrayBuffer. Runs only when the debug tool `bin/jslayout_dump` has been built
# (it is not needed by the codegen path, so the rest of the suite stays minimal).
if [ -x "$root/bin/jslayout_dump" ]; then
  gotLayout="$("$root/bin/jslayout_dump" "$here/tlayout.c.nif")"
  if ! diff -u "$here/tlayout.expected.txt" <(printf '%s\n' "$gotLayout"); then
    echo "FAILURE: computed type layout differs from golden tlayout.expected.txt"
    exit 1
  fi
  echo "layout: PASS"
else
  echo "bin/jslayout_dump not built; skipped layout check"
fi

# ── bundling (the `{.bundle.}` linker half): `jslink` reads the project link
# manifest NIF (the format nimony's deps.nim emits) and assembles the final JS
# bundle from the per-module `kind "artifact"` entries — prepending a runtime,
# dropping each file's own `"use strict";`, and appending the program entry.
# Runs only when `bin/jslink` has been built.
if [ -x "$root/bin/jslink" ]; then
  # two self-contained JS "artifacts" (as if emitted per-module by lengjs) …
  printf '"use strict";\nfunction helper(){ return 21; }\n' > "$work/a.js"
  printf '"use strict";\nfunction main(){ globalThis.RESULT = helper()*2; }\n' > "$work/b.js"
  printf '// runtime\n' > "$work/rt.js"
  # … listed in a manifest in the real (link (file … (kind …))) grammar.
  cat > "$work/t.linkmanifest.nif" <<NIF
(.nif27)
(link
 (apptype "console")
 (output "prog.js")
 (file "$work/a.js" (kind "artifact"))
 (file "$work/b.js" (kind "artifact")))
NIF
  "$root/bin/jslink" "$work/t.linkmanifest.nif" "$work/prog.js" "$work/rt.js" >/dev/null
  # exactly one "use strict"; both artifacts present; entry appended and runs.
  stricts=$(grep -c '^"use strict";$' "$work/prog.js" || true)
  if [ "$stricts" != "1" ]; then
    echo "FAILURE: jslink bundle has $stricts \"use strict\"; directives (want 1)"; exit 1
  fi
  if command -v node >/dev/null 2>&1; then
    {
      cat "$work/prog.js"
      echo 'if (globalThis.RESULT===42) { console.log("bundle: PASS"); }'
      echo 'else { console.log("bundle: FAIL got "+globalThis.RESULT); process.exit(1); }'
    } | node
  else
    echo "bundle: PASS (structure; node not found for run check)"
  fi
else
  echo "bin/jslink not built; skipped bundle check"
fi

echo "jsbackend: OK"

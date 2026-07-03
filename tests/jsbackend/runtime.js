// Canonical JS runtime for the Leng JS backend: one ArrayBuffer as linear
// memory, plus the small set of C primitives the lowered code imports (allocator,
// atomics, memcpy, stdio). Araq's boundary: eventually the native Nim allocator
// is compiled to JS over mmap/munmap; until then `mi_malloc`/`mi_free` are shims
// over a bump allocator (no reclamation — fine for straight-line programs).
const _ab = new ArrayBuffer(1 << 22);
const _dv = new DataView(_ab);
const _u8 = new Uint8Array(_ab);
let _brk = 8;                                   // offset 0 reserved as nil
const _sizes = new Map();                        // block -> size, so realloc copies exactly
function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; _u8.fill(0,p,p+n); _sizes.set(p,n); return p; }

const mem = {
  setI8:(p,v)=>_dv.setInt8(p,v), i8:(p)=>_dv.getInt8(p),
  setU8:(p,v)=>_dv.setUint8(p,v), u8At:(p)=>_dv.getUint8(p),
  setI16:(p,v)=>_dv.setInt16(p,v,true), i16:(p)=>_dv.getInt16(p,true),
  setI32:(p,v)=>_dv.setInt32(p,v,true), i32:(p)=>_dv.getInt32(p,true),
  setU32:(p,v)=>_dv.setUint32(p,v,true), u32:(p)=>_dv.getUint32(p,true),
  setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)),
  setU64:(p,v)=>_dv.setBigUint64(p,BigInt(v),true), u64n:(p)=>Number(_dv.getBigUint64(p,true)),
  i64b:(p)=>_dv.getBigInt64(p,true), u64b:(p)=>_dv.getBigUint64(p,true),   // exact 64-bit reads (int64/uint64 -> BigInt)
  setF64:(p,v)=>_dv.setFloat64(p,v,true), f64:(p)=>_dv.getFloat64(p,true),
  copy:(d,s,n)=>_u8.copyWithin(d,s,s+n),
  bytes:(p,n)=>_u8.subarray(p,p+n),
  writeStr:(p,s)=>{ for(let i=0;i<s.length;i++) _u8[p+i]=s.charCodeAt(i); },
};

// allocator (bump; the real mimalloc compiles here later)
function mi_malloc(n){ return allocFixed(Number(n)); }
function mi_free(p){ /* bump allocator: no reclamation */ }
function mi_realloc(p,n){ n=Number(n); const q=allocFixed(n); const old=_sizes.get(Number(p))||0; _u8.copyWithin(q,Number(p),Number(p)+Math.min(old,n)); return q; }
function mi_calloc(c,s){ return allocFixed(Number(c)*Number(s)); }
function mi_zalloc(n){ return allocFixed(Number(n)); }
function mi_usable_size(p){ return _sizes.get(Number(p)) || 0; }
function memcpy(d,s,n){ _u8.copyWithin(Number(d),Number(s),Number(s)+Number(n)); return d; }
function memset(p,v,n){ _u8.fill(v&0xff,Number(p),Number(p)+Number(n)); return p; }
function strlen(p){ let n=0; while(_u8[Number(p)+n]!==0) n++; return n; }
function memcmp(a,b,n){ a=Number(a);b=Number(b);n=Number(n); for(let i=0;i<n;i++){ const d=_u8[a+i]-_u8[b+i]; if(d!==0) return d<0?-1:1; } return 0; }

// Function table: a proc pointer in linear memory is an integer index into
// `_fns` (WASM's model — JS can't call an integer). `_fnid(fn)` interns a proc to
// its stable index when it's taken as a value; the codegen emits `_fns[idx](args)`
// for an indirect call (a proc variable / closure field). Index 0 is nil.
const _fns = [null];
const _fnmap = new Map();
function _fnid(fn){ let i=_fnmap.get(fn); if(i===undefined){ i=_fns.length; _fns.push(fn); _fnmap.set(fn,i); } return i; }

// C11 memory-order constants (imported by the atomic ops; ignored by the shims).
const __ATOMIC_RELAXED = 0, __ATOMIC_CONSUME = 1, __ATOMIC_ACQUIRE = 2,
      __ATOMIC_RELEASE = 3, __ATOMIC_ACQ_REL = 4, __ATOMIC_SEQ_CST = 5;

// atomics for ARC refcounts: operate on an i64 slot at byte offset `p`
function __atomic_add_fetch(p,v,o){ const n=mem.i64n(p)+Number(v); mem.setI64(p,n); return n; }
function __atomic_sub_fetch(p,v,o){ const n=mem.i64n(p)-Number(v); mem.setI64(p,n); return n; }
function __atomic_load_n(p,o){ return mem.i64n(p); }
function __atomic_store_n(p,v,o){ mem.setI64(p,v); }

// stdio
const stdout = {};
function fwrite(ptr,size,nmemb,f){ process.stdout.write(Buffer.from(_u8.subarray(ptr,ptr+size*nmemb))); return nmemb; }
function fprintf(f,fmt,...a){ let i=0; process.stdout.write(String(fmt).replace(/%ll[du]|%l[du]|%[dus]/g,()=>String(a[i++]))); }
function fputc(c,f){ process.stdout.write(Buffer.from([c&0xff])); return c; }
function nimFlushStdStreams(){}
function copyMem_0_sysvq0asl(d,s,n){ if(typeof d==='number'&&typeof s==='number') _u8.copyWithin(d,s,s+n); }
function exit(c){ process.exit(Number(c)||0); }

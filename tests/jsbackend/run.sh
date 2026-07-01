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

# ── M1.5: data structures (object construction, field access, arrays, indexing)
gen tdata
if have_node; then
  {
    cat "$work/tdata.js"
    echo 'if (mkpoint_0_tdata(3,4)===7 && arrsum_0_tdata()===60)'
    echo '  { console.log("functional(data): PASS"); }'
    echo 'else { console.log("functional(data): FAIL"); process.exit(1); }'
  } | node
fi

# ── data over LINEAR MEMORY (M2, Araq's Typed-Array model): a declared object
# is laid out in an ArrayBuffer — construction is `allocFixed` + typed stores at
# the field byte-offsets `jslayout` computes, field access is a typed load. This
# is the buffer path in the shipping codegen (only for declared object types;
# arrays and JS-interop stay on the legacy mapping for now). `tdata` above keeps
# the legacy JS-object mapping (its `Point` is undeclared), so both paths run.
gen tbuffer
if have_node; then
  {
    # tiny linear-memory runtime (the shape runtime.js provides): allocFixed over
    # an ArrayBuffer + typed load/store. mkpoint(3,4) builds a Point in the buffer
    # and reads its fields back -> 7.
    echo 'const _dv = new DataView(new ArrayBuffer(1<<16)); let _brk = 8;'
    echo 'function allocFixed(n){ const p=(_brk+7)&~7; _brk=p+n; new Uint8Array(_dv.buffer).fill(0,p,p+n); return p; }'
    echo 'const mem = { setI64:(p,v)=>_dv.setBigInt64(p,BigInt(v),true), i64n:(p)=>Number(_dv.getBigInt64(p,true)) };'
    cat "$work/tbuffer.js"
    echo 'if (mkpoint_0_tbuffer(3,4)===7 && mkpoint_0_tbuffer(20,22)===42)'
    echo '  { console.log("functional(buffer): PASS"); }'
    echo '  else { console.log("functional(buffer): FAIL got "+mkpoint_0_tbuffer(3,4)); process.exit(1); }'
  } | node
fi

# ── addresses (locals): a pointer is a fat `[base, key]` pair; an addr-taken
# local is boxed (`[value]`), so its address is `[x, 0]` and writes through the
# pointer (`p[0][p[1]] = …`) mutate the underlying local.
gen taddr
if have_node; then
  {
    cat "$work/taddr.js"
    # through: *p+1 on a boxed local; usebump: mutate via pointer param;
    # addrparam: a value param whose address is taken is boxed at entry.
    echo 'if (through_0_taddr()===42 && usebump_0_taddr()===15 && addrparam_0_taddr(7)===99)'
    echo '  { console.log("functional(addr): PASS"); }'
    echo 'else { console.log("functional(addr): FAIL"); process.exit(1); }'
  } | node
fi

# ── addresses (aggregates): addr of an object field is `[obj, "field"]` and of
# an array element is `[arr, idx]`; two fat pointers compare component-wise
# (`nimPtrEq`), since fresh `[base,key]` arrays are never `===`.
gen taddr2
if have_node; then
  {
    cat "$work/taddr2.js"
    # fieldaddr: write through addr of a field; elemaddr: through addr of an
    # element; samefield/difffield: fat-pointer equality (same key vs different).
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
    echo 'function extTriple(x){return x*3;}'
    cat "$work/timportc.js"
    echo 'if (run_0_timportc()===21 && counter===21)'
    echo '  { console.log("functional(ffi): PASS"); }'
    echo '  else { console.log("functional(ffi): FAIL"); process.exit(1); }'
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

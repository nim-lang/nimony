#!/usr/bin/env bash
# Regression test for the lengc JavaScript backend (M0/M1: pure compute).
#
# 1. golden check: `lengc js tcompute.c.nif` must match tcompute.expected.js
# 2. functional check: the generated JS runs under Node and computes the
#    expected values.
#
# tcompute.c.nif is a hand-authored, self-contained Leng module (no system
# deps), so this test needs only `bin/lengc` and `node`.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
lengc="$root/bin/lengc"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

"$lengc" js --nimcache:"$work" "$here/tcompute.c.nif"
got="$work/tcompute.js"

if ! diff -u "$here/tcompute.expected.js" "$got"; then
  echo "FAILURE: generated JS differs from golden tcompute.expected.js"
  exit 1
fi

if command -v node >/dev/null 2>&1; then
  {
    cat "$got"
    echo 'if (add_0_tcompute(2,3)===5 && fib_0_tcompute(10)===55 && fib_0_tcompute(20)===6765)'
    echo '  { console.log("functional: PASS"); }'
    echo 'else { console.log("functional: FAIL"); process.exit(1); }'
  } | node
else
  echo "node not found; skipped functional check (golden check passed)"
fi

# ── M1.5: data structures (object construction, field access, arrays, indexing)
"$lengc" js --nimcache:"$work" "$here/tdata.c.nif"
gotData="$work/tdata.js"

if ! diff -u "$here/tdata.expected.js" "$gotData"; then
  echo "FAILURE: generated JS differs from golden tdata.expected.js"
  exit 1
fi

if command -v node >/dev/null 2>&1; then
  {
    cat "$gotData"
    echo 'if (mkpoint_0_tdata(3,4)===7 && arrsum_0_tdata()===60)'
    echo '  { console.log("functional(data): PASS"); }'
    echo 'else { console.log("functional(data): FAIL"); process.exit(1); }'
  } | node
fi

# ── addresses: locals whose address is taken are boxed into 1-element arrays,
# so writes through a pointer mutate the underlying local.
"$lengc" js --nimcache:"$work" "$here/taddr.c.nif"
gotAddr="$work/taddr.js"

if ! diff -u "$here/taddr.expected.js" "$gotAddr"; then
  echo "FAILURE: generated JS differs from golden taddr.expected.js"
  exit 1
fi

if command -v node >/dev/null 2>&1; then
  {
    cat "$gotAddr"
    # through: *p+1 on a boxed local; usebump: mutate via pointer param;
    # addrparam: a value param whose address is taken is boxed at entry.
    echo 'if (through_0_taddr()===42 && usebump_0_taddr()===15 && addrparam_0_taddr(7)===99)'
    echo '  { console.log("functional(addr): PASS"); }'
    echo 'else { console.log("functional(addr): FAIL"); process.exit(1); }'
  } | node
fi

echo "jsbackend: OK"

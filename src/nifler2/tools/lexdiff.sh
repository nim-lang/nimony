#!/bin/sh
# Differential test for `nimlexer`: every `.nim` file under the given
# directories is tokenized twice -- once by Nim 2's `compiler/lexer.nim` and
# once by `nimlexer` -- and the two token streams must be byte-identical.
#
#   nim c -o:bin/refdump src/nifler2/tools/refdump.nim
#   bin/nimony c src/nifler2/tools/nimlexdump.nim   # then copy it to bin/
#   src/nifler2/tools/lexdiff.sh lib src tests/nimony
#
# Identifiers are compared after `nimIdentNormalize` and keywords by kind
# alone, because Nim's identifier cache cannot report the spelling it saw:
# `Foo` and `foO` share one `PIdent`. Everything else -- kinds, literal
# values, `indent`, and the leading/trailing/eof spacing the parser needs --
# is compared exactly.

set -e
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ok=0
bad=0
find "$@" -name '*.nim' | sort > "$tmp/files"
while IFS= read -r f; do          # `read`, not `for`: paths may contain spaces
  bin/refdump "$f" > "$tmp/ref" 2>/dev/null || true
  bin/nimlexdump "$f" > "$tmp/new" 2>/dev/null || true
  if cmp -s "$tmp/ref" "$tmp/new"; then
    ok=$((ok + 1))
  else
    bad=$((bad + 1))
    echo "DIFFERS: $f"
    diff "$tmp/ref" "$tmp/new" | head -8
  fi
done < "$tmp/files"
echo "$ok files agree, $bad differ"
[ "$bad" -eq 0 ]

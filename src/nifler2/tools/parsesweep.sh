#!/bin/sh
# How much of Nim `nifler2` parses. Runs it over every `.nim` file under the
# given directories and groups the failures by message, because the messages
# are what say which production is still wrong.
#
#   bin/nimony c -o:bin/nifler2 src/nifler2/nifler2.nim
#   src/nifler2/tools/parsesweep.sh lib src tests
#
# The tree it produces is NOT yet the tree `src/nifler` produces -- see the
# `fanOut` gap in `parserrt.nim`; this only measures acceptance.
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ok=0
bad=0
find "$@" -name '*.nim' | sort > "$tmp/files"
while IFS= read -r f; do          # `read`, not `for`: paths may contain spaces
  if msg=$(bin/nifler2 p "$f" "$tmp/out.nif" 2>&1) && [ -z "$msg" ]; then
    ok=$((ok + 1))
  else
    bad=$((bad + 1))
    echo "$f: $msg" >> "$tmp/fails"
    printf '%s\n' "$msg" | sed -e 's/^[^ ]*(\([0-9]*\), \([0-9]*\)) //' >> "$tmp/msgs"
  fi
done < "$tmp/files"
echo "$ok files parse, $bad fail"
if [ -s "$tmp/msgs" ]; then
  echo "--- failures by message"
  sort "$tmp/msgs" | uniq -c | sort -rn | head -25
  echo "--- first 10 files"
  head -10 "$tmp/fails"
fi

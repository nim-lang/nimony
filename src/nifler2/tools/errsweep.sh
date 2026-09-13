#!/bin/sh
# Compare the first error message of `nifler` and `nifler2` on every file
# `nifler` rejects. nifler recovers and reports on; nifler2 stops at the
# first error, so the first line is what is compared.
#
#   src/nifler2/tools/errsweep.sh ../nim/tests
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
same=0; diff=0; accepted=0; crashed=0
find "$@" -name '*.nim' | sort > "$tmp/files"
while IFS= read -r f; do
  if bin/nifler --portablePaths -f p "$f" "$tmp/a.nif" > "$tmp/a.txt" 2>&1; then continue; fi
  a=$(head -1 "$tmp/a.txt")
  case "$a" in
    *") Error: "*) ;;
    *) crashed=$((crashed + 1)); echo "nifler crashed: $f" >> "$tmp/report"; continue ;;
  esac
  if bin/nifler2 --portablePaths p "$f" "$tmp/b.nif" > "$tmp/b.txt" 2>&1; then
    accepted=$((accepted + 1)); echo "accepted by nifler2: $f" >> "$tmp/report"; continue
  fi
  b=$(head -1 "$tmp/b.txt")
  if [ "$a" = "$b" ]; then same=$((same + 1))
  else
    diff=$((diff + 1))
    printf '%s\n  nifler : %s\n  nifler2: %s\n' "$f" "$a" "$b" >> "$tmp/report"
    # the message pair, with positions, quoted parts and paths blanked out
    { printf '%s' "$a" | sed -e 's/^.*) Error: //' -e "s/'[^']*'/'_'/g" -e 's/at .*(.*) ?/at _ ?/'
      printf '  <->  '
      if [ "${a%%) Error: *}" = "${b%%) Error: *}" ]; then printf '(same pos) '; fi
      printf '%s\n' "$b" | sed -e 's/^.*) Error: //' -e "s/'[^']*'/'_'/g"
    } >> "$tmp/kinds"
  fi
done < "$tmp/files"
echo "$same same first error, $diff different, $accepted accepted by nifler2, $crashed nifler crashes"
if [ -s "$tmp/kinds" ]; then
  echo "--- differences by message pair (nifler <-> nifler2)"
  sort "$tmp/kinds" | uniq -c | sort -rn | head -40
fi
[ -s "$tmp/report" ] && cat "$tmp/report"

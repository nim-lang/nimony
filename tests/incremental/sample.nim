# Driver for hastur's incremental-build regression test (`hastur incremental`).
# Kept tiny so the test stays fast; the test machinery edits and restores
# this file in place, so anything that depends on the exact content here
# also lives in `src/hastur.nim`.

import std/syncio

echo "incremental sample"

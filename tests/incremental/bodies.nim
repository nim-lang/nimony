# Driver for the `generic-body` and `folded-body` phases of hastur's
# incremental-build regression (`hastur incremental`); see `bodiesdep.nim`.

import std/syncio
import bodiesdep

const K = folded()

echo "generic ", tag(0)
echo "folded ", K

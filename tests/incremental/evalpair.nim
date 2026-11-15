## Two modules evaluating constants at compile time, both importing
## `evalpairdep`: see Phase 17 of `src/hastur/incrementaltests.nim`.
import std/syncio
import evalpairone, evalpairtwo
echo "evalpair ", b, " ", c

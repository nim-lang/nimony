## Instantiates `incl` of `genericmid.nim`, and with it `putIt` of
## `genericdep.nim`, which this module does not import.
import std/syncio
import genericmid

var w: Wrap
echo "generic ", w.incl 3

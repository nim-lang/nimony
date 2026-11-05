## Calls `connect` through the re-export in `reexportfacade.nim`: sem fills
## in its defaulted arguments in declaration order.
import std/syncio
import reexportfacade

echo "reexport ", connect("x")

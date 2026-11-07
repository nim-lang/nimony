## Built with and without `-d:incrementalStale`; see `optiondep.nim`.
import std/syncio
import optiondep

echo "optionuser ", probe()

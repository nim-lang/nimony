## Imports `mainlib.nim`, whose `when isMainModule` section must not run here.
import std/syncio
import mainlib

echo "mainuser ", libValue()

# `supportsCopyMem` used outside the main module (deps/mcopymem.nim).
import std/syncio
import deps/mcopymem

echo resIsPlain, " ", plainIsPlain(), " ", viaWhen()

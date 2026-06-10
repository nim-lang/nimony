## Deprecated compatibility shim.
##
## `addFloat` and `$` for `float`/`float32` now live in `system` (implemented
## with the bundled Schubfach/Dragonbox dtoa — see `system/formatfloat`), so
## they are available everywhere without an import. This module is kept only so
## existing `import std/formatfloat` lines keep compiling; it exports nothing.

## A package-style module: imported as `deps/pkgmod`, resolves to
## deps/pkgmod/pkgmod.nim (foo -> foo/foo.nim fallback).
proc pkgAnswer*(): int = 42

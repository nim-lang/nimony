#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path-derivation rules shared by `deps.nim` (which decides where each
## module's `.html` lands) and `dagon` (which synthesises cross-module URLs
## from import suffixes). Both must agree byte-for-byte on what relpath a
## given source `.nim` maps to, otherwise hyperlinks miss their targets.

import std / [os, strutils]

const ExternalBucket* = "_external"

const UnreservedChar = {'A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'}

proc sanitizePart(s: string): string =
  ## Make a single path component URL/filesystem-safe. Replaces any byte
  ## outside the unreserved set with `_`. Used for the `_external/…` bucket
  ## where the source path is unconstrained.
  result = newStringOfCap(s.len)
  for c in s:
    if c in UnreservedChar: result.add c
    else: result.add '_'

proc startsWithPath(s, prefix: string): bool {.inline.} =
  ## True iff `s` starts with `prefix` followed by a path separator (or
  ## equals `prefix`). Avoids the `lib/std/system` vs. `lib_other/` confusion
  ## that a plain `startsWith` would have.
  if not s.startsWith(prefix): return false
  if s.len == prefix.len: return true
  let next = s[prefix.len]
  result = next == '/' or next == '\\'

proc deriveRelpath*(srcPath, projectRoot, stdlibRoot: string): string =
  ## Compute the htmldocs-relative path for a source `.nim` file. Three
  ## buckets, in priority order:
  ##   1. Under `stdlibRoot`: keep the part after `stdlibRoot/` and swap
  ##      `.nim` → `.html` (e.g. `lib/std/system.nim` → `std/system.html`).
  ##   2. Under `projectRoot`: same, relative to the project (e.g.
  ##      `myproj/src/foo.nim` → `src/foo.html`).
  ##   3. Anywhere else: `_external/<sanitized-basename>.html`. Loses path
  ##      info; collisions punted (rare in practice — stray imports of
  ##      absolute paths from outside the project tree).
  let abs = absolutePath(srcPath)
  let absStdlib = if stdlibRoot.len > 0: absolutePath(stdlibRoot) else: ""
  let absProject = if projectRoot.len > 0: absolutePath(projectRoot) else: ""

  if absStdlib.len > 0 and abs.startsWithPath(absStdlib):
    let rest = abs.substr(absStdlib.len + 1)
    return rest.changeFileExt("html")
  if absProject.len > 0 and abs.startsWithPath(absProject):
    let rest = abs.substr(absProject.len + 1)
    return rest.changeFileExt("html")
  let bn = splitFile(srcPath).name
  return ExternalBucket / sanitizePart(bn) & ".html"

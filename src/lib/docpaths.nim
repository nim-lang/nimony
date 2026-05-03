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

proc toUnixPath*(s: string): string =
  ## On Windows, replace `\` with `/` so paths emitted into NIF / HTML are
  ## byte-identical across operating systems (Windows, Linux, macOS produce
  ## the same artifacts). No-op on POSIX. Apply at every boundary where a
  ## path crosses into a NIF token, an HTML attribute, or a CLI arg whose
  ## downstream consumer might also be cross-OS.
  when defined(windows):
    s.replace('\\', '/')
  else:
    s

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

proc externalBucketPath(srcPath: string): string =
  let bn = splitFile(srcPath).name
  result = ExternalBucket / sanitizePart(bn) & ".html"

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
  ##
  ## `projectRoot` and `stdlibRoot` are expected to be absolute (callers in
  ## `deps.nim` and `dagon` use `absoluteParentDir`/`stdlibDir`). If `srcPath`
  ## is relative, fall through to bucket 3.
  if srcPath.isAbsolute:
    if stdlibRoot.len > 0 and srcPath.startsWithPath(stdlibRoot):
      return srcPath.substr(stdlibRoot.len + 1).changeFileExt("html")
    if projectRoot.len > 0 and srcPath.startsWithPath(projectRoot):
      return srcPath.substr(projectRoot.len + 1).changeFileExt("html")
  result = externalBucketPath(srcPath)

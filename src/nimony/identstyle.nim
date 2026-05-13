#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Style-insensitive identifier index over `pool.strings`.
##
## Backs Nimony's `.feature: "ignoreStyle".` and the gated style-insensitive
## builtin-pragma lookup. Lazily groups every interned StrId in `pool.strings`
## by its Nim-2-style normalized form (first char kept, the rest lowercased,
## underscores dropped). Sem-frontend lookups call `styleSiblings(name)` to
## walk every distinctly-spelled StrId that shares its style group.
##
## Two normalisation rules are exposed:
##
## * `normalizeStyleIdent`: Nim 2 identifier rule — preserves `s[0]`.
##   Used for `IgnoreStyleFeature`-gated identifier lookup.
## * `normalizeStyleFull`: `cmpIgnoreStyle` rule — lowercases everything,
##   including the first character. Used for builtin pragma names (Nim has
##   always matched those case-insensitively all the way down).

import std / tables
import ".." / lib / [bitabs, nifstreams]
import ".." / models / [tags, nimony_tags]

var
  styleGroups: Table[StrId, seq[StrId]]
  styleHighWaterMark: int = 0  # number of `pool.strings` entries already grouped

proc normalizeStyleIdent*(s: string): string =
  ## Nim 2 identifier normalization: keep s[0] as-is, lowercase the rest and
  ## drop underscores. Empty string maps to empty.
  result = newStringOfCap(s.len)
  if s.len == 0: return
  result.add s[0]
  for i in 1 ..< s.len:
    let ch = s[i]
    if ch == '_': discard
    elif ch >= 'A' and ch <= 'Z': result.add chr(ord(ch) + (ord('a') - ord('A')))
    else: result.add ch

proc normalizeStyleFull*(s: string): string =
  ## Fully case-insensitive normalization (matches Nim's `cmpIgnoreStyle`):
  ## drops underscores and lowercases every character.
  result = newStringOfCap(s.len)
  for ch in s:
    if ch == '_': discard
    elif ch >= 'A' and ch <= 'Z': result.add chr(ord(ch) + (ord('a') - ord('A')))
    else: result.add ch

proc ensureStyleGroups() {.sideEffect.} =
  ## Walk every `pool.strings` entry added since the last call and add it to
  ## its style group. Idempotent: subsequent calls do nothing until more
  ## strings are interned. The loop body may itself intern the normalized
  ## form, growing `pool.strings` further — the loop picks those up on the
  ## next iteration (normalization is idempotent, so this terminates).
  while styleHighWaterMark < pool.strings.len:
    let id = StrId(styleHighWaterMark + 1)
    inc styleHighWaterMark
    let norm = pool.strings.getOrIncl(normalizeStyleIdent(pool.strings[id]))
    var slot = addr styleGroups.mgetOrPut(norm, @[])
    if id notin slot[]: slot[].add id

iterator styleSiblings*(name: StrId): StrId {.sideEffect.} =
  ## Yields every interned StrId that shares `name`'s normalized form,
  ## including `name` itself. Used by sem-frontend lookups when style-
  ## insensitive matching is requested.
  ensureStyleGroups()
  let norm = pool.strings.getOrIncl(normalizeStyleIdent(pool.strings[name]))
  ensureStyleGroups()  # `getOrIncl` may have grown the pool above
  # Bind the slot to a local before iterating: nimony's borrow check refuses
  # to iterate the temporary `seq` returned by `getOrDefault` directly.
  let siblings = styleGroups.getOrDefault(norm)
  for k in siblings:
    yield k

# -------------- builtin-pragma style index --------------------------
#
# Looks up a builtin `NimonyPragma` from a user-written ident under
# `cmpIgnoreStyle` rules (`normalizeStyleFull`, lowercases the first char
# too — Nim has always matched pragma names that way). Lazy: built on first
# call from the canonical tag names of every `rawTagIsNimonyPragma` tag.
# Sem code consults this only when `pragmaKind` already returned `NoPragma`
# and the user opted into `IgnoreStyleFeature`.

var
  pragmaStyleIndex: Table[StrId, NimonyPragma]

proc ensurePragmaStyleIndex*() {.sideEffect.} =
  if pragmaStyleIndex.len != 0: return
  # Iterate raw tag ids rather than NimonyPragma directly because the latter
  # is a "holey" enum (explicit non-contiguous values) and `for p in
  # NimonyPragma` doesn't compile cleanly here.
  for tagIdInt in 1 .. high(TagEnum).int:
    let raw = cast[TagEnum](tagIdInt)
    if not rawTagIsNimonyPragma(raw): continue
    let p = cast[NimonyPragma](raw)
    let canonicalName = pool.tags[TagId(tagIdInt)]
    let norm = pool.strings.getOrIncl(normalizeStyleFull(canonicalName))
    pragmaStyleIndex[norm] = p

proc pragmaKindByStyle*(name: StrId): NimonyPragma {.sideEffect.} =
  ## Looks up `name`'s canonical NimonyPragma under fully case-insensitive
  ## matching. Callers fall back here when `pragmaKind` returned `NoPragma`
  ## and the user opted into `IgnoreStyleFeature`. Returns `NoPragma` if
  ## nothing matches.
  ensurePragmaStyleIndex()
  let norm = pool.strings.getOrIncl(normalizeStyleFull(pool.strings[name]))
  result = pragmaStyleIndex.getOrDefault(norm, NoPragma)

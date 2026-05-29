#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution, for
#    details about the copyright.
#

## NIFC tag decoding for `nifcore` cursors.
##
## Reuses the canonical, NIF-API-independent enums from `models/nifc_tags`
## (`NifcStmt`, `NifcExpr`, `NifcType`, `NifcPragma`, `NifcOther`, …) whose
## ordinals are the master tag ordinals. To make a parsed `nifcore` buffer's
## `cursorTagId` line up with those ordinals, seed the buffer's `TagPool`
## from the same master `TagData` (just like `nifstreams` seeds its global
## pool). Then `cast[NifcStmt](tagId)` works exactly as in `nifc_model` — but
## over `nifcore` cursors, keeping the enums clear of any NIF-API coupling.

import std / assertions
import nifcore
import ".." / models / [nifc_tags, tags]
export nifc_tags

proc createNifcTagPool*(): TagPool =
  ## A `nifcore` tag pool seeded so each NIFC tag's `TagId` equals its master
  ## `TagEnum` ordinal. Pass to `parseFromFile`/`parseFromBuffer` as
  ## `sharedTags` so `tagEnumOf`/`stmtKind`/… can decode by ordinal.
  result = newTagPool()
  for e in TagEnum:
    if e == InvalidTagId: continue
    let id = result.registerTag(TagData[e][0])
    assert uint32(id) == uint32(TagData[e][1]),
      "nifc tag pool misalignment for " & TagData[e][0]

template tagEnumOf*(c: Cursor): TagEnum =
  (if c.kind == TagLit: cast[TagEnum](uint32(c.cursorTagId)) else: InvalidTagId)

proc stmtKind*(c: Cursor): NifcStmt {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsNifcStmt(e): cast[NifcStmt](e) else: NoStmt

proc exprKind*(c: Cursor): NifcExpr {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsNifcExpr(e): cast[NifcExpr](e) else: NoExpr

proc pragmaKind*(c: Cursor): NifcPragma {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsNifcPragma(e): cast[NifcPragma](e) else: NoPragma

proc substructureKind*(c: Cursor): NifcOther {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsNifcOther(e): cast[NifcOther](e) else: NoSub

proc typeKind*(c: Cursor): NifcType {.inline.} =
  if c.kind == DotToken: return VoidT       # an empty type slot reads as void
  let e = tagEnumOf(c)
  if rawTagIsNifcType(e): cast[NifcType](e) else: NoType

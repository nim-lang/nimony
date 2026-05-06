## Compatibility shim that lets hexer modules (dce1, dce2) recognise NIFC
## tags on a `nifcursors.Cursor` without importing `nifc/nifc_model`.
##
## `nifc_model` was ported to nifprims (different in-memory token layout)
## as part of the experimental `nifprims` rollout, so its `stmtKind` /
## `substructureKind` / `pragmaKind` procs now take `nifprims.Cursor`. The
## hexer caller's cursor is still `nifcursors.Cursor`, hence this small
## re-implementation.
##
## Re-exports the NIFC tag enums verbatim, then defines kind accessors for
## the classic `nifcursors.Cursor`. Identical bodies to `nifc_model`, just
## the parameter type differs.

include ".." / lib / nifprelude
import ".." / models / [nifc_tags, callconv_tags, tags]
export nifc_tags, callconv_tags

template tagEnum*(c: Cursor): TagEnum = cast[TagEnum](tag(c))

proc stmtKind*(c: Cursor): NifcStmt {.inline.} =
  if c.kind == ParLe and rawTagIsNifcStmt(tagEnum(c)):
    result = cast[NifcStmt](tagEnum(c))
  else:
    result = NoStmt

proc substructureKind*(c: Cursor): NifcOther {.inline.} =
  if c.kind == ParLe and rawTagIsNifcOther(tagEnum(c)):
    result = cast[NifcOther](tag(c))
  else:
    result = NoSub

proc pragmaKind*(c: Cursor): NifcPragma {.inline.} =
  if c.kind == ParLe:
    let e = tagEnum(c)
    if rawTagIsNifcPragma(e):
      result = cast[NifcPragma](e)
    else:
      result = NoPragma
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsNifcPragma(cast[TagEnum](tagId)):
      result = cast[NifcPragma](tagId)
    else:
      result = NoPragma
  else:
    result = NoPragma

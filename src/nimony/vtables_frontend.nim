#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.


## Frontend support for vtables. Strictly speaking this should all be
## done in the backend, but then there is an additional implied ordering
## between modules that kills parallelism. So we prepare the vtables
## in the frontend and make them part of a module's interface.

include nifprelude
import nifindexes, symparser, treemangler, typekeys
import ".." / nimony / [nimony_model, decls, programs, typenav,
  renderer, typeprops, semdata]

# maybe we can use this later to provide better error messages
proc sameMethodSignatures(a, b: Cursor; checkClass = false): bool =
  var a = a
  var b = b
  if a.substructureKind != ParamsU: return false
  if b.substructureKind != ParamsU: return false
  inc a
  inc b
  if checkClass:
    # TODO: for xsubtype in inheritanceChain(xtyp):
    skip a
    skip b
  else:
    # first parameter is of the class type and must be ignored:
    skip a
    skip b
  while a.kind != ParRi and b.kind != ParRi:
    let pa = takeLocal(a, SkipFinalParRi)
    let pb = takeLocal(b, SkipFinalParRi)
    if not sameTrees(pa.typ, pb.typ):
      return false
  if a.kind == ParRi and b.kind == ParRi:
    inc a
    inc b
    # check return types
    return sameTrees(a, b)
  else:
    return false

proc sameMethodBucket(a, b: Cursor): bool =
  when false:
    let aDecl = asRoutine(a)
    let bDecl = asRoutine(b)
    result = sameMethodBucket(aDecl.params, bDecl.params)
  result = true

proc methodDef*(c: var SemContext; fn: SymId) =
  let sym = tryLoadSym(fn)
  if sym.status == LacksNothing:
    var decl = sym.decl
    for buf in c.methodDefs.mitems:
      let def = beginRead(buf)
      if sameMethodBucket(def, decl):
        endread(buf)
        return

    var buf = createTokenBuf()
    takeTree(buf, decl)
    c.methodDefs.add buf

proc methodKey*(name: string; a: Cursor): string =
  # First parameter was the class type and has already been skipped here!
  var a = a
  var b = createMangler(60)
  while a.kind != ParRi:
    let pa = takeLocal(a, SkipFinalParRi)
    mangle b, pa.typ
  inc a
  # also add return type:
  mangle b, a
  result = name & ":" & b.extract()

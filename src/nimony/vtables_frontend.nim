#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.


## Frontend support for vtables. Strictly speaking this should all be
## done in the backend, but then there is an additional implied ordering
## between modules that kills parallelism. So we prepare the vtables
## in the frontend and make them part of a module's interface.

import std/assertions
include nifprelude
import nifindexes, symparser, treemangler, typekeys
import nimony_model, decls, programs, typenav,
  renderer, sigmatch, semdata, typeprops

when false:
  # maybe we can use this later to provide better error messages
  proc sameMethodSignatures(a, b: Cursor): bool =
    var a = a
    var b = b
    if a.substructureKind != ParamsU: return false
    if b.substructureKind != ParamsU: return false
    inc a
    inc b
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

proc methodKeyImpl(name: string; a: Cursor): string =
  # First parameter was the class type and has already been skipped here!
  var a = a
  var b = createMangler(60)
  while a.kind != ParRi:
    let pa = takeLocal(a, SkipFinalParRi)
    mangle b, pa.typ, Frontend
  inc a
  # also add return type:
  mangle b, a, Frontend
  skip a
  # handle pragmas:
  let props = extractProcProps(a)
  b.addKeyw $props.cc
  # Mangle the raises type, not just a boolean
  if props.usesRaises:
    if not cursorIsNil(props.raisesType):
      b.addKeyw "raises"
      mangle b, props.raisesType, Frontend
    else:
      # .raises without type defaults to ErrorCode - mangle the default
      b.addKeyw "raises"
      var errorCodeBuf = createTokenBuf(1)
      errorCodeBuf.addSymUse pool.syms.getOrIncl(ErrorCodeName), NoLineInfo
      let errorCodeCursor = cursorAt(errorCodeBuf, 0)
      mangle b, errorCodeCursor, Frontend
  else:
    b.addKeyw "raisesNo"
  b.addKeyw $props.usesClosure
  result = name & ":" & b.extract()

proc destroyMethodKey*(): string =
  ## Known method key for =destroy hooks (no extra params, void return, default pragmas)
  var b = createMangler(60)
  b.addEmpty() # void return type
  b.addKeyw "ccNone"
  b.addKeyw "raisesNo"  # Hooks don't raise
  b.addKeyw "closureNo"
  result = "=destroy:" & b.extract()

proc traceMethodKey*(): string =
  ## Known method key for =trace hooks (pointer param, void return, default pragmas)
  var b = createMangler(60)
  b.addKeyw "pointer" # marker param type
  b.addEmpty() # void return type
  b.addKeyw "ccNone"
  b.addKeyw "raisesNo"  # Hooks don't raise
  b.addKeyw "closureNo"
  result = "=trace:" & b.extract()

proc methodKey*(methodName: string; paramRest: Cursor): string =
  ## Compute the method signature key for vtable matching.
  ## For =destroy and =trace hooks, uses canonical signatures to ensure proper override matching.
  ## For other methods, mangles the params and return type.
  if methodName == "=destroy":
    result = destroyMethodKey()
  elif methodName == "=trace":
    result = traceMethodKey()
  else:
    result = methodKeyImpl(methodName, paramRest)

proc loadVTable*(typ: SymId): seq[semdata.MethodIndexEntry] =
  ## Load vtable methods from the type's (methods (kv key symId) ...) pragma.
  ## For generic instances, also loads methods from the generic base.
  result = @[]
  let res = tryLoadSym(typ)
  if res.status != LacksNothing:
    return
  # Try to process as type - asTypeDecl will work for TypeY symbols
  # For non-types, pragmas cursor will be invalid and we'll return early
  let typeDecl = asTypeDecl(res.decl)
  var pragmas = typeDecl.pragmas
  if pragmas.kind == ParLe:
    inc pragmas # skip (pragmas
    while pragmas.kind != ParRi:
      if pragmas.kind == ParLe and pragmas.pragmaKind == MethodsP:
        inc pragmas # skip (methods
        while pragmas.kind == ParLe and pragmas.substructureKind == KvU:
          inc pragmas # skip (kv
          if pragmas.kind == StringLit:
            let signature = pragmas.litId
            inc pragmas
            if pragmas.kind == Symbol:
              let methodSym = pragmas.symId
              result.add semdata.MethodIndexEntry(fn: methodSym, signature: signature)
              inc pragmas
            skipParRi pragmas
          else:
            skip pragmas
        skipParRi pragmas # skip methods )
      else:
        skip pragmas

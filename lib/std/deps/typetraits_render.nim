## Type-to-string rendering for the `typetraits` plugin (`$`).
## Mirrors the type-position subset of `src/nimony/renderer.nim` (`gtype`).

import std/strutils
import plugins

proc takeNumberType(n: var NifCursor; base: string): string =
  result = base
  n.into:
    let size = n.intVal
    if size != -1:
      # Structural `(i 64)` is Nimony's `int`; explicit `int64` stays a symbol.
      if base == "int" and size == 64:
        discard
      else:
        result.add $size
    inc n
    while n.hasMore:
      skip n

proc renderTypeName*(defs: NifCursor; n: var NifCursor): string

proc renderInvocation(defs: NifCursor; n: var NifCursor): string =
  var head = n
  skip n
  var parts: seq[string] = @[]
  while n.hasMore:
    parts.add renderTypeName(defs, n)
    skip n
  result = renderTypeName(defs, head) & "[" & parts.join(", ") & "]"

proc nominalName(defs: NifCursor; sym: SymId): string =
  let decl = findTypeDecl(defs, sym)
  if not decl.hasMore:
    return symBasename(sym)
  let typevars = asTypeDecl(decl).typevars
  if typevars.typeKind == AtT:
    var tv = typevars
    tv.into:
      if not tv.hasMore:
        result = symBasename(sym)
      else:
        result = renderInvocation(defs, tv)
  else:
    result = symBasename(sym)

proc renderTypeName*(defs: NifCursor; n: var NifCursor): string =
  if n.kind == Symbol:
    return nominalName(defs, n.symId)
  if n.kind == IntLit:
    result = $n.intVal
    inc n
    return
  if n.kind == UIntLit:
    result = $n.uintVal
    inc n
    return
  if n.kind != TagLit:
    inc n
    return "?"

  case n.typeKind
  of IT:
    result = takeNumberType(n, "int")
  of UT:
    result = takeNumberType(n, "uint")
  of FT:
    result = takeNumberType(n, "float")
  of CT:
    result = "char"
    skip n
  of BoolT, VoidT, UntypedT, TypedT, AutoT:
    result = $n.typeKind
    skip n
  of TypedescT:
    result = "typedesc["
    n.into:
      if n.hasMore:
        result.add renderTypeName(defs, n)
        skip n
    result.add "]"
  of AtT:
    n.into:
      if not n.hasMore:
        result = "?"
        return
      result = renderInvocation(defs, n)
  of RangetypeT:
    n.into:
      if n.hasMore:
        skip n
        var lo = n
        skip n
        if n.hasMore:
          result = renderTypeName(defs, lo) & ".." & renderTypeName(defs, n)
          skip n
        else:
          result = renderTypeName(defs, lo)
      else:
        result = "range"
  of ArrayT:
    result = "array["
    n.into:
      if n.hasMore:
        var elem = n
        skip n
        if n.hasMore:
          result.add renderTypeName(defs, n)
          result.add ", "
        result.add renderTypeName(defs, elem)
        while n.hasMore:
          skip n
    result.add "]"
  of DistinctT:
    n.into:
      if n.hasMore:
        result = "distinct " & renderTypeName(defs, n)
        skip n
      else:
        result = "distinct"
  of RefT:
    result = "ref "
    n.into:
      if n.hasMore and n.otherKind notin {NotnilU, NilU, UncheckedU}:
        result.add renderTypeName(defs, n)
        skip n
      while n.hasMore:
        skip n
  of PtrT:
    result = "ptr "
    n.into:
      if n.hasMore and n.otherKind notin {NotnilU, NilU, UncheckedU}:
        result.add renderTypeName(defs, n)
        skip n
      while n.hasMore:
        skip n
  of SetT:
    result = "set["
    n.into:
      if n.hasMore:
        result.add renderTypeName(defs, n)
        skip n
    result.add "]"
  of TupleT, ClosureTupleT:
    result = "tuple["
    n.into:
      var addSep = false
      while n.hasMore:
        if addSep:
          result.add ", "
        else:
          addSep = true
        case n.otherKind
        of KvU:
          n.into:
            var key = n
            skip n
            let keyStr = if key.kind == Ident: key.identText else: symText(key)
            result.add keyStr & ": " & renderTypeName(defs, n)
            skip n
        else:
          result.add renderTypeName(defs, n)
          skip n
    result.add "]"
  of TypekindT:
    n.into:
      if n.hasMore:
        result = renderTypeName(defs, n)
        skip n
      else:
        result = "?"
  else:
    result = "?"
    skip n

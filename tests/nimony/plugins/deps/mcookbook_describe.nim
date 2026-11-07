import plugins
import cookbook_support

proc transform(input: NifCursor): NifBuilder =
  let typ = callArgs(input)
  if typ.kind != Symbol:
    return errorTree("describe needs a named object type", typ)
  let decl = findTypeDecl(loadTypeDefinitions(), typ.symId)
  if not decl.hasMore:
    return needTypes(typ.symId)
  let body = asTypeDecl(decl).body
  if body.typeKind != ObjectT:
    return errorTree("describe needs an object type", typ)
  let obj = asObjectDecl(body)
  if obj.parentType.kind != DotToken:
    return errorTree("this example supports objects without inheritance", typ)
  # Reject variant sections rather than silently leaving their fields out
  var first = true
  for child in body.children:
    if first: first = false
    elif child.otherKind != FldU:
      return errorTree("this example supports plain object fields only", child)
  result = createTree()
  result.withTree CallX, input.info:
    result.bindSym "describeImpl"
    result.addIdent "describe"
    result.addIdent "value"
    var typeNode = typ
    result.takeTree typeNode
    result.addStrLit symBasename(typ.symId) & "("
    result.bindSym "string"
    result.bindSym "add"
    result.withTree StmtsS, input.info:
      var firstField = true
      for field in obj.fields:
        result.withTree CallX, field.name.info:
          result.bindSym "appendField"
          result.addIdent "result"
          result.addStrLit (if firstField: "" else: ", ") & symBasename(field.name.symId) & ": "
          result.withTree DotX, field.name.info:
            result.addIdent "value"
            result.addIdent symBasename(field.name.symId)
          result.bindSym "add"
          result.bindSym "&"
          result.bindSym "$"
        firstField = false

saveTree transform(loadPluginInput())

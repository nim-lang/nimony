import plugins

# Describes the declarations of a typed block through the typed views
# (`asLocal`, `asRoutine`, `asTypeDecl`, `asObjectDecl`, `asForStmt`)

proc typeText(t: NifCursor): string =
  if t.kind == Symbol: symBasename(t.symId)
  elif t.kind == TagLit: tagText(t)
  else: "?"

proc describe(body: NifCursor): string =
  result = ""
  for s in body.children:
    case s.stmtKind
    of LetS, VarS:
      let l = asLocal(s)
      result.add tagText(s) & " " & symBasename(l.name.symId) & ": " & typeText(l.typ) & "\n"
    of ConstS:
      let l = asLocal(s)
      result.add "const " & symBasename(l.name.symId) & ": " & typeText(l.typ) & "\n"
    of ProcS:
      let r = asRoutine(s)
      var ps = ""
      for p in r.params.children:
        let l = asLocal(p)
        ps.add symBasename(l.name.symId) & ": " & typeText(l.typ) & ";"
      result.add "proc " & symBasename(r.name.symId) & "(" & ps & ") -> " &
        typeText(r.retType) & (if isGeneric(r): " generic" else: "") & "\n"
    of ForS:
      let f = asForStmt(s)
      let v = asLocal(firstChild(f.vars))
      result.add "for " & symBasename(v.name.symId) & " in " & tagText(f.iter) & "\n"
    of TypeS:
      let t = asTypeDecl(s)
      result.add "type " & symBasename(t.name.symId)
      if t.body.typeKind == ObjectT:
        for f in asObjectDecl(t.body).fields:
          result.add " " & symBasename(f.name.symId) & ": " & typeText(f.typ)
      result.add "\n"
    else:
      result.add "other " & tagText(s) & "\n"

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  # (stmts describe <body>)
  var c = firstChild(n)
  skip c                                # the template's name
  result.addStrLit describe(c)

saveTree transform(loadPluginInput())

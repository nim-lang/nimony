import plugins

proc parseMode(n: NifCursor): string =
  var n = templateArgs(n)
  if n.kind != StringLit:
    return ""
  result = n.stringValue

proc addConstHeader(t: var NifBuilder; name: string) =
  t.addIdent name
  t.addEmptyNode3()

proc addTemplateHeader(t: var NifBuilder; name: string) =
  t.addIdent name
  t.addEmptyNode3()
  t.withTree ParamsU, NoLineInfo:
    discard
  t.addIdent "untyped"
  t.addEmptyNode2()

proc renderMode(mode: string): NifBuilder =
  result = createTree()
  case mode
  of "const-int":
    result.withTree StmtsS, NoLineInfo:
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("X")
        result.addIntLit(1)
  of "const-float":
    result.withTree StmtsS, NoLineInfo:
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("X")
        result.addFloatLit(3.14)
  of "const-string":
    result.withTree StmtsS, NoLineInfo:
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("X")
        result.addStrLit("abc")
  of "const-float-chain":
    result.withTree StmtsS, NoLineInfo:
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("PI")
        result.addFloatLit(3.14)
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("DEG2RAD")
        result.withTree DivX, NoLineInfo:
          result.addEmptyNode()
          result.addIdent("PI")
          result.addFloatLit(180.0)
  of "const-float-chain-infix":
    result.withTree StmtsS, NoLineInfo:
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("PI")
        result.addFloatLit(3.14)
      result.withTree ConstS, NoLineInfo:
        result.addConstHeader("DEG2RAD")
        result.withTree InfixX, NoLineInfo:
          result.addIdent("/")
          result.addIdent("PI")
          result.addFloatLit(180.0)
  of "template-float":
    result.withTree StmtsS, NoLineInfo:
      result.withTree TemplateS, NoLineInfo:
        result.addTemplateHeader("X")
        result.withTree StmtsS, NoLineInfo:
          result.addFloatLit(3.14)
  of "template-int":
    result.withTree StmtsS, NoLineInfo:
      result.withTree TemplateS, NoLineInfo:
        result.addTemplateHeader("X")
        result.withTree StmtsS, NoLineInfo:
          result.addIntLit(1)
  of "template-string":
    result.withTree StmtsS, NoLineInfo:
      result.withTree TemplateS, NoLineInfo:
        result.addTemplateHeader("X")
        result.withTree StmtsS, NoLineInfo:
          result.addStrLit("abc")
  else:
    result = errorTree("unknown mode: " & mode)

let input = loadPluginInput()
saveTree renderMode(parseMode(input))

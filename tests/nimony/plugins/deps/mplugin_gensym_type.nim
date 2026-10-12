import plugins

proc transform(root: NifCursor): NifBuilder =
  let typ = genSym()
  result = createTree()
  result.withTree BlockS, root.info:
    result.addEmptyNode()
    result.withTree StmtsS, root.info:
      result.withTree TypeS, root.info:
        result.addSymDef typ, root.info
        result.addEmptyNode3()
        result.withTree ObjectT, NoLineInfo:
          result.addEmptyNode()
          result.withTree FldU, NoLineInfo:
            result.addIdent "x"
            result.addEmptyNode2()
            result.bindSym("int")
            result.addEmptyNode()
      result.withTree CallX, root.info:
        result.bindSym("sizeof")
        result.addSymUse typ, root.info

saveTree transform(loadPluginInput())

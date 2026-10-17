import plugins

proc transform(): NifBuilder =
  let procedure = genSym()
  result = createTree()
  result.withTree BlockS, NoLineInfo:
    result.addEmptyNode()
    result.withTree StmtsS, NoLineInfo:
      result.withTree ProcS, NoLineInfo:
        result.addSymDef procedure, NoLineInfo
        result.addEmptyNode3()
        result.withTree ParamsU, NoLineInfo: discard
        result.bindSym("int")
        result.addEmptyNode2()
        result.withTree StmtsS, NoLineInfo:
          result.withTree RetS, NoLineInfo: result.addIntLit 42
      result.withTree CallX, NoLineInfo:
        result.addSymUse procedure, NoLineInfo

let input = loadPluginInput()
saveTree transform()

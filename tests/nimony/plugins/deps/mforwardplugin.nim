import plugins

proc addRoutine(r: var NifBuilder; name, other, base: string; body: bool) =
  ## `proc name*(n: int): bool`, forward declared or with the body
  ## `if n == 0: result = base else: result = other(n - 1)`.
  r.withTree ProcS, NoLineInfo:
    r.addIdent name
    r.addIdent "x"
    r.addEmptyNode2
    r.withTree ParamsU, NoLineInfo:
      r.withTree ParamU, NoLineInfo:
        r.addIdent "n"
        r.addEmptyNode2
        r.addIdent "int"
        r.addEmptyNode
    r.addIdent "bool"
    r.addEmptyNode2
    if body:
      r.withTree StmtsS, NoLineInfo:
        r.withTree IfS, NoLineInfo:
          r.withTree ElifU, NoLineInfo:
            r.withTree InfixX, NoLineInfo:
              r.addIdent "=="
              r.addIdent "n"
              r.addIntLit 0
            r.withTree StmtsS, NoLineInfo:
              r.withTree AsgnS, NoLineInfo:
                r.addIdent "result"
                r.addIdent base
          r.withTree ElseU, NoLineInfo:
            r.withTree StmtsS, NoLineInfo:
              r.withTree AsgnS, NoLineInfo:
                r.addIdent "result"
                r.withTree CallX, NoLineInfo:
                  r.addIdent other
                  r.withTree InfixX, NoLineInfo:
                    r.addIdent "-"
                    r.addIdent "n"
                    r.addIntLit 1
    else:
      r.addEmptyNode

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  result.withTree StmtsS, NoLineInfo:
    result.addRoutine "isOdd", "isEven", "false", false
    result.addRoutine "isEven", "isOdd", "true", true
    result.addRoutine "isOdd", "isEven", "false", true
    # no forward declaration: `isEven2` calls `isOdd2`, declared after it
    result.addRoutine "isEven2", "isOdd2", "true", true
    result.addRoutine "isOdd2", "isEven2", "false", true

saveTree transform(loadPluginInput())

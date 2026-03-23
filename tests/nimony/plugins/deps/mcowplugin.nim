import nimonyplugins

proc tr(n: Node): Tree =
  var t = createTree()
  var alias = t
  let frozen = freeze(t)
  let info = n.info

  alias.withTree StmtsS, info:
    alias.withTree CallS, info:
      alias.addIdent "echo"
      alias.addStrLit "detached"

  discard frozen
  result = alias

var inp = loadTree()
saveTree tr(inp)

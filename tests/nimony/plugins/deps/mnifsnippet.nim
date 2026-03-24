import nimonyplugins

proc tr(n: Node): Tree =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg

  let first = nif("""(call echo $arg)""", {"arg": ~arg})
  let second = nif("""(call echo $msg)""", {"msg": ~"seen"})
  let third = nif("""(call echo $count)""", {"count": ~17})
  let tail = nif("""(call echo "done")""")

  let generated = nif("""(stmts $first $second $third $tail)""",
    {"first": ~first, "second": ~second, "third": ~third, "tail": ~tail})
  result = createTree()
  var copy = generated
  result.takeTree(copy)

var inp = loadTree()
saveTree tr(inp)

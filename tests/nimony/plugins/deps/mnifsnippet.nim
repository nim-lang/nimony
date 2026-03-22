import nimonyplugins

proc tr(n: Node): Tree =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg

  let first: Tree = nif("""(call echo $arg)""", {"arg": %arg})
  let second: Tree = nif("""(call echo $msg)""", {"msg": %"seen"})
  let third: Tree = nif("""(call echo $count)""", {"count": %17})
  let tail: Tree = nif("""(call echo "done")""", {:})

  result = nif("""(stmts $first $second $third $tail)""",
    {"first": %first, "second": %second, "third": %third, "tail": %tail})

var inp = loadTree()
saveTree tr(beginRead inp)

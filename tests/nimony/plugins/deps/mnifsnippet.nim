import nimonyplugins

proc tr(n: Node): FrozenTree =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg

  let first = nif("""(call echo $arg)""", {"arg": ~arg})
  let second = nif("""(call echo $msg)""", {"msg": ~"seen"})
  let third = nif("""(call echo $count)""", {"count": ~17})
  let tail = nif("""(call echo "done")""")

  result = nif("""(stmts $first $second $third $tail)""",
    {"first": ~first, "second": ~second, "third": ~third, "tail": ~tail})

var inp = loadTree()
saveTree tr(beginRead inp)

import std / [os, syncio]
import nimonyplugins

proc tr(n: Node): Tree =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg

  let first = """(call echo $arg)""" ~$~ {"arg": ~arg}
  let second = """(call echo $msg)""" ~$~ {"msg": ~"seen"}
  let third = """(call echo $count)""" ~$~ {"count": ~17}
  let tail = nifFragment("""(call echo "done")""")

  result = """(stmts $first $second $third $tail)""" ~$~
    {"first": ~first, "second": ~second, "third": ~third, "tail": ~tail}

var inp = loadPluginInput()
writeFile os.paramStr(2), renderTree(tr(inp))

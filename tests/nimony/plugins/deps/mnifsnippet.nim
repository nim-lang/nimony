import std / [os, syncio]
import nimonyplugins

proc tr(n: NifCursor): NifBuilder =
  let head = if n.stmtKind == StmtsS: firstChild(n) else: n
  let first = """(call echo $arg)""" %~ {"arg": ~head}
  let second = """(call echo $msg)""" %~ {"msg": ~"seen"}
  let third = """(call echo $count)""" %~ {"count": ~17}
  let tail = nifFragment("""(call echo "done")""")

  result = """(stmts $first $second $third $tail)""" %~
    {"first": ~first, "second": ~second, "third": ~third, "tail": ~tail}

var inp = loadPluginInput()
writeFile os.paramStr(2), renderTree(tr(inp))

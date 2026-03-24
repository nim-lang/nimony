import std / [os, syncio]
import nimonyplugins

proc tr(n: Node): Node =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg

  let prepared = ~"prepared"

  let byIdent = nif("""(call $callee $msg)""",
    {"callee": ~ident("echo"), "msg": ~"seen"})
  let byBool = nif("""(call echo $flag)""", {"flag": ~true})
  let byChar = nif("""(call echo $ch)""", {"ch": ~'Z'})
  let byInt64 = nif("""(call echo $num)""", {"num": ~17'i64})
  let byUint32 = nif("""(call echo $num)""", {"num": ~18'u32})
  let byFloat32 = nif("""(call echo $num)""", {"num": ~2.5'f32})
  let byPrepared = nif("""(call echo $msg)""", {"msg": ~prepared})
  let byNode = nif("""(call echo $msg)""", {"msg": ~arg})

  result = nif("""(stmts $a $b $c $d $e $f $g $h)""", {
    "a": ~byIdent,
    "b": ~byBool,
    "c": ~byChar,
    "d": ~byInt64,
    "e": ~byUint32,
    "f": ~byFloat32,
    "g": ~byPrepared,
    "h": ~byNode
  })

var inp = loadTree()
writeFile os.paramStr(2), renderNode(tr(inp))

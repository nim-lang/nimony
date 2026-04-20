import std / [os, syncio]
import nimonyplugins

proc tr(n: NifCursor): NifBuilder =
  var n = n
  if n.stmtKind == StmtsS:
    inc n

  let prepared = ~"prepared"

  let byIdent = """(call $callee $msg)""" %~
    {"callee": ~ident("echo"), "msg": ~"seen"}
  let byBool = """(call echo $flag)""" %~ {"flag": ~true}
  let byChar = """(call echo $ch)""" %~ {"ch": ~'Z'}
  let byInt64 = """(call echo $num)""" %~ {"num": ~17'i64}
  let byUint32 = """(call echo $num)""" %~ {"num": ~18'u32}
  let byFloat32 = """(call echo $num)""" %~ {"num": ~2.5'f32}
  let byPrepared = """(call echo $msg)""" %~ {"msg": ~prepared}
  let byNode = """(call echo $msg)""" %~ {"msg": ~n}

  result = """(stmts $a $b $c $d $e $f $g $h)""" %~ {
    "a": ~byIdent,
    "b": ~byBool,
    "c": ~byChar,
    "d": ~byInt64,
    "e": ~byUint32,
    "f": ~byFloat32,
    "g": ~byPrepared,
    "h": ~byNode
  }

var inp = loadPluginInput()
writeFile os.paramStr(2), renderTree(tr(inp))

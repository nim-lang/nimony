## Nim's `parseStmt` and `parseExpr` for plugins.
##
## Nim source text in, the `nim-parsed` NIF that nimsem reads out: the same
## tree nifler writes for the same code, so a plugin can splice it into its
## output as it is. The parser is nifler2's, which is why this is a module of
## its own rather than part of `plugins` -- a plugin that does not import it
## does not compile a Nim parser.
##
## ```nim
## import plugins, nimparser
##
## proc tr(n: NifCursor): NifBuilder =
##   var arg = callArgs(n)
##   var code = parseStmt(stringValue(arg), arg.info)
##   result = createTree()
##   result.takeTree code
## ```

import std / assertions
import plugins
import ".." / ".." / lib / nifcore except symId, `$`, addSymUse, addSymDef
# only the entry point: `nimgrammar` re-exports `nifpools`, whose builders
# intern into the compiler's global pool rather than the plugin's
from ".." / ".." / nifler2 / nimgrammar import parseSnippet

proc copyWithInfo(dest: var NifBuilder; c: var NifCursor; info: LineInfo) =
  ## Every value of the parsed tree is positioned at `info`: the positions the
  ## parser recorded are relative to a string that is not a file.
  if c.kind == TagLit:
    dest.openTree(c.resolvedTagId, info)
    c.into:
      while c.hasMore:
        copyWithInfo(dest, c, info)
    dest.closeTree()
  else:
    case c.kind
    of DotToken: dest.addDotToken()
    of Ident: dest.addIdent strVal(c)
    of StrLit: dest.addStrLit strVal(c)
    of CharLit: dest.addCharLit charLit(c)
    of IntLit: dest.addIntLit intVal(c)
    of UIntLit: dest.addUIntLit uintVal(c)
    of FloatLit: dest.addFloatLit floatVal(c)
    else: raiseAssert "the parser emits no " & $c.kind
    dest.appendLineInfo info
    inc c

proc parseNim(code: string; info: LineInfo; asExpr: bool): NifCursor =
  var dest = createTree()
  var err = ""
  var tree = parseSnippet(code, asExpr, dest.pool, dest.tags, err)
  if err.len > 0:
    dest = errorTree(err, info)
  else:
    var c = beginRead(tree)
    copyWithInfo(dest, c, info)
    endRead c
  result = snapshot(dest)

proc parseStmt*(code: string; info = NoLineInfo): NifCursor =
  ## `code` as a statement list, `(stmts ...)`, every node positioned at
  ## `info`. A syntax error is returned as an error tree at `info`, which the
  ## compiler reports when the plugin's output contains it.
  parseNim(code, info, false)

proc parseExpr*(code: string; info = NoLineInfo): NifCursor =
  ## `code` as exactly one expression, every node positioned at `info`. Like
  ## `parseStmt`, an error comes back as an error tree.
  parseNim(code, info, true)

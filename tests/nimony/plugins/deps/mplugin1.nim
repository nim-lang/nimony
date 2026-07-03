import plugins
import std / assertions

proc replaceBorrows(t: var Replacer; replacement: NifBuilder) =
  ## Compile-time regression: builder replacement must remain usable.
  replace(t, Expr, replacement)
  assert not replacement.isEmpty

proc tr(n: NifCursor): NifBuilder =
  assert n.tagText == "stmts"

  var child = createTree()
  child.addIntLit 7
  var children = createTree()
  children.addTree(child)
  children.addTree(child)
  assert renderTree(children) == "7 7"
  assert renderTree(child) == "7"

  var sample = createTree()
  sample.withTree StmtsS, NoLineInfo:
    sample.withTree CallS, NoLineInfo:
      sample.addIdent "echo"
  var scan = snapshot(sample)
  var nestedTags = 0
  scan.linearScan:
    inc nestedTags
    assert scan.stmtKind == CallS
  assert nestedTags == 1
  assert not scan.hasMore
  assert scan.tag.tagText == "err"

  var safeError = errorTree("exhausted cursor", scan)
  assert renderTree(safeError) == "(err . \"exhausted cursor\")"

  var symbols = createTree()
  symbols.addSymUse("plugin.symbol")
  let symbolCursor = snapshot(symbols)
  assert symbolCursor.symId.symText == "plugin.symbol"

  var unknownPragmas = createTree()
  for i in 0 ..< 600:
    unknownPragmas.addIdent("unknownPragma" & $i)
  var unknown = snapshot(unknownPragmas)
  while unknown.hasMore:
    assert unknown.pragmaKind == NoPragma
    unknown.skip()
  var customTag = createTree()
  customTag.openTree("customAfterPragmaLookups")
  customTag.closeTree()
  assert renderTree(customTag) == "(customAfterPragmaLookups)"

  result = createTree()
  let info = n.info
  var head = callArgs(n)
  assert head.kind == StrLit
  assert head.info.isValid
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree head

var inp = loadPluginInput()
saveTree tr(inp)

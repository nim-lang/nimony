import std / [assertions]

import nimonyplugins
import ".." / ".." / ".." / ".." / src / lib / [lineinfos]

proc payload(n: Node): uint32 =
  doAssert isPayload(n.info)
  result = getPayload(n.info)

proc echoedString(n: Node): string =
  var it = n
  doAssert it.stmtKind == CallS
  inc it
  doAssert it.identText == "echo"
  inc it
  result = it.stringValue

proc mutateSink(n: sink Node; nextInfo: PackedLineInfo): uint32 =
  var local = n
  local.setInfo(nextInfo)
  result = getPayload(local.info)

proc makeProgram(): Tree =
  result = createTree()
  result.withTree StmtsS, toPayload(10):
    result.withTree CallS, toPayload(11):
      result.addIdent "echo"
      result.addStrLit "alpha"
    result.withTree CallS, toPayload(12):
      result.addIdent "echo"
      result.addStrLit "beta"

proc verifyHooks() =
  block:
    var tree = makeProgram()
    var baseline = snapshot(tree)

    doAssert payload(baseline) == 10

    var rootCopy = baseline
    rootCopy.setInfo(toPayload(20))
    doAssert payload(rootCopy) == 20
    doAssert payload(baseline) == 10

    rootCopy.setInfo(toPayload(22))
    doAssert payload(rootCopy) == 22
    doAssert payload(baseline) == 10

    var first = baseline
    inc first
    doAssert payload(first) == 11

    var sibling = first
    first.setInfo(toPayload(21))
    doAssert payload(first) == 21
    doAssert payload(sibling) == 11

    var freshFirst = baseline
    inc freshFirst
    doAssert payload(freshFirst) == 11

    doAssert mutateSink(sibling, toPayload(31)) == 31
    doAssert payload(sibling) == 11

    var builder = createTree()
    builder.withTree CallS, toPayload(50):
      builder.addIdent "echo"
      builder.addStrLit "before"

    var beforeBuilderMutation = snapshot(builder)
    builder.withTree CallS, toPayload(51):
      builder.addIdent "echo"
      builder.addStrLit "after"

    var afterBuilderMutation = snapshot(builder)

    doAssert echoedString(beforeBuilderMutation) == "before"

    var afterSecond = afterBuilderMutation
    doAssert echoedString(afterSecond) == "before"
    skip afterSecond
    doAssert payload(afterSecond) == 51
    doAssert echoedString(afterSecond) == "after"

proc tr(n: Node): Tree =
  discard n
  verifyHooks()

  result = createTree()
  result.withTree StmtsS, NoLineInfo:
    result.withTree CallS, NoLineInfo:
      result.addIdent "echo"
      result.addStrLit "ok"

var inp = loadNode()
saveTree tr(inp)

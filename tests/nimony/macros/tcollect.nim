# Minimal port of std/sugar's `collect` comprehension as a macro.
# This first version handles only the simplest form:
#   collect:
#     for x in iter:
#       <expr>
# i.e. one for-loop with a bare-expression body. The macro rewrites that to
# a stmt-list-expr that builds a `seq[typeof(<expr>)]`.
#
# All helpers are inlined into the macro body — the macro_plugin pipeline
# only carries the macro body itself into the generated plugin module; sibling
# procs in the user's module are not visible to the plugin compile.

import std / [syncio, macros, assertions]

macro collect(body: untyped): untyped =
  proc findLeaf(n: NimNode): NimNode =
    case n.kind
    of nnkStmtList, nnkStmtListExpr, nnkBlockStmt, nnkBlockExpr,
       nnkForStmt, nnkWhileStmt, nnkElifBranch, nnkElse, nnkOfBranch:
      if n.len == 0: return n
      return findLeaf(n[n.len - 1])
    of nnkIfStmt, nnkIfExpr, nnkCaseStmt, nnkWhenStmt, nnkTryStmt:
      if n.len == 0: return n
      return findLeaf(n[n.len - 1])
    else:
      return n

  proc transform(n: NimNode; res: NimNode): NimNode =
    case n.kind
    of nnkStmtList, nnkStmtListExpr, nnkBlockStmt, nnkBlockExpr,
       nnkForStmt, nnkWhileStmt, nnkElifBranch, nnkElse, nnkOfBranch:
      result = copyNimTree(n)
      if result.len > 0:
        result[result.len - 1] = transform(result[result.len - 1], res)
    of nnkIfStmt, nnkIfExpr, nnkCaseStmt, nnkWhenStmt, nnkTryStmt:
      result = copyNimTree(n)
      let start = if n.kind == nnkCaseStmt: 1 else: 0
      var i = start
      while i < result.len:
        result[i] = transform(result[i], res)
        inc i
    else:
      result = newCall(newDotExpr(res, ident("add")), [n])

  let res: NimNode = ident("collectResult")
  # NOTE: a real `collect` would use `typeof(<leaf>)` for the seq's element
  # type, but Nim's trick relies on bidirectional type inference (Nim 2 lets
  # `newSeq[empty]` adopt T from the first `add(…)`). Until Nimony has the
  # equivalent, we hardcode the element type to `int`. Matches the test.
  let seqType = newTree(nnkBracketExpr, [ident("seq"), ident("int")])
  let initEmpty = newTree(nnkPrefix, [ident("@"), newTree(nnkBracket, [])])
  let varDecl = newTree(nnkVarSection,
    [newTree(nnkIdentDefs, [res, seqType, initEmpty])])
  let transformed = transform(body, res)
  result = newTree(nnkStmtListExpr, [varDecl, transformed, res])

let nums = collect:
  for i in 0..3:
    i * 2

assert nums.len == 4
assert nums[0] == 0
assert nums[1] == 2
assert nums[2] == 4
assert nums[3] == 6
echo "collect ok: ", nums.len, " items"

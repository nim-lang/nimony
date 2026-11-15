import std / assertions
import plugins

proc checkSyntheticViews() =
  for kind in [AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, AshrX,
               BitandX, BitorX, BitxorX, EqX, NeqX, LeX, LtX]:
    var tree = createTree()
    tree.withTree kind, NoLineInfo:
      tree.withTree IT, NoLineInfo:
        tree.addIntLit 32
      tree.addIntLit 7
      tree.addIntLit 3
    let node = snapshot(tree)
    let before = renderNode(node)
    let binary = asBinaryOp(node)
    assert binary.typ.typeKind == IT
    assert intValue(firstChild(binary.typ)) == 32
    assert intValue(binary.operand1) == 7
    assert intValue(binary.operand2) == 3
    assert renderNode(node) == before
  for kind in [ConvX, HconvX, DconvX]:
    var tree = createTree()
    tree.withTree kind, NoLineInfo:
      tree.withTree FT, NoLineInfo:
        tree.addIntLit 64
      tree.addIntLit 42
    let node = snapshot(tree)
    let conversion = asConversion(node)
    assert conversion.typ.typeKind == FT
    assert intValue(conversion.operand) == 42
    assert node.exprKind == kind
  for armCount in [1, 3]:
    for withElse in [false, true]:
      var tree = createTree()
      tree.withTree IfS, NoLineInfo:
        for i in 0 ..< armCount:
          tree.withTree ElifU, NoLineInfo:
            tree.addIntLit i
            tree.addIntLit (10 + i)
        if withElse:
          tree.withTree ElseU, NoLineInfo:
            tree.addIntLit 99
      let node = snapshot(tree)
      let before = renderNode(node)
      let statement = asIfStmt(node)
      for pass in 0 ..< 2:
        var count = 0
        for arm in branches(statement):
          if arm.isElse:
            assert withElse and count == armCount
            assert not arm.cond.hasMore
            assert intValue(arm.body) == 99
          else:
            assert intValue(arm.cond) == count
            assert intValue(arm.body) == 10 + count
          inc count
        assert count == armCount + (if withElse: 1 else: 0)
      assert renderNode(node) == before
  var tree = createTree()
  tree.withTree WhileS, NoLineInfo:
    tree.withTree TrueX, NoLineInfo: discard
    tree.withTree StmtsS, NoLineInfo: discard
  let node = snapshot(tree)
  let loop = asWhileStmt(node)
  assert loop.cond.exprKind == TrueX
  assert loop.body.stmtKind == StmtsS
  assert node.stmtKind == WhileS

type Coverage = object
  binary, conversion, conditional, loop: bool

proc visit(node: NifCursor; seen: var Coverage) =
  if node.kind != TagLit: return
  let before = renderNode(node)
  case node.exprKind
  of AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, AshrX,
     BitandX, BitorX, BitxorX, EqX, NeqX, LeX, LtX:
    let binary = asBinaryOp(node)
    assert binary.typ.hasMore and binary.operand1.hasMore and binary.operand2.hasMore
    assert isValid(binary.operand1.info) and isValid(binary.operand2.info)
    seen.binary = true
  of ConvX, HconvX, DconvX:
    let conversion = asConversion(node)
    assert conversion.typ.hasMore and conversion.operand.hasMore
    assert isValid(conversion.operand.info)
    seen.conversion = true
  else: discard
  case node.stmtKind
  of IfS:
    var count = 0
    for arm in branches(asIfStmt(node)):
      assert arm.body.hasMore
      if arm.isElse: assert not arm.cond.hasMore
      else: assert isValid(arm.cond.info)
      inc count
    assert count > 0
    seen.conditional = true
  of WhileS:
    let loop = asWhileStmt(node)
    assert loop.cond.hasMore and loop.body.hasMore
    seen.loop = true
  else: discard
  assert renderNode(node) == before
  for child in node.children: visit(child, seen)

proc transform(input: NifCursor): NifBuilder =
  var argument = callArgs(input)
  if argument.stmtKind == StmtsS:
    checkSyntheticViews()
    let before = renderNode(argument)
    var seen = Coverage()
    visit(argument, seen)
    assert seen.binary and seen.conversion and seen.conditional and seen.loop
    assert renderNode(argument) == before
    result = createTree()
    result.takeTree argument
    return
  let before = renderNode(argument)
  let call = asCall(argument)
  assert call.fn.hasMore
  var count = 0
  for a in args(call):
    inc count
    if a.exprKind == DotX:
      let dot = asDot(a)
      assert dot.obj.hasMore
      assert dot.field.kind == Symbol
      assert isValid(dot.field.info)
  let name = if call.fn.kind == Symbol: symBasename(call.fn.symId) else: "field"
  case name
  of "zero": assert count == 0
  of "one": assert count == 1
  of "two": assert count == 2
  of "field":
    assert call.fn.exprKind == DotX
    assert symBasename(asDot(call.fn).field.symId) == "fn"
    assert count == 1
  else: assert false, "unexpected callee"
  assert renderNode(argument) == before
  # Visiting the arguments a second time must give the same sequence
  var secondCount = 0
  for a in args(call): inc secondCount
  assert count == secondCount
  result = createTree()
  result.takeTree argument

saveTree transform(loadPluginInput())

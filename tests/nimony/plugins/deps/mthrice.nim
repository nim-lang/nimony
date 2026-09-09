import plugins

# A plugin template that stands for a VALUE: it folds `n * 3`, but only once
# `n` is a concrete literal. While `n` is still a type variable there is
# nothing to fold, so the expansion is deferred and the compiler asks again
# after instantiation.
proc transform(n: NifCursor): NifBuilder =
  var arg = callArgs(n)
  result = createTree()
  if arg.kind == IntLit:
    result.addIntLit intValue(arg) * 3
  else:
    result = deferExpansion()

let input = loadPluginInput()
saveTree transform(input)

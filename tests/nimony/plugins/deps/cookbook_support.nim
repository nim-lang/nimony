import std / syncio

template dbg*(value: untyped) =
  ## Print the expression and evaluate it only once
  bind astToStr, echo
  echo astToStr(value), " = ", value

template appendField*(target, label, value, append, concat, format: untyped) {.untyped.} =
  append(target, concat(label, format(value)))

template describeImpl*(publicName, parameter, T, heading, returnType, append, fields: untyped) {.untyped.} =
  proc publicName(parameter: T): returnType =
    result = heading
    fields
    append(result, ")")

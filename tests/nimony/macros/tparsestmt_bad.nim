# A syntax error in `parseStmt`'s code is a compile error at the macro call.
import std / macros

macro broken(): untyped =
  result = parseStmt("if x\n  discard")

broken()

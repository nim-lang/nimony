# Fake compiler pass exercising the operand-headed statement check.
#
# `(pragmax (pragmas ...) X)` and `(comesfrom SYM S*)` are transparent wrappers
# whose FIRST child is an operand, not a statement (see `nimony_model`'s
# `OperandHeadedS`). A branch that opens the node and walks every child as a
# statement walks that operand. Both the violating and the conforming forms are
# here so the golden proves the check fires *and* that it stays quiet.

import std/assertions
include "../../src/lib/nifprelude"
import "../../src/nimony/nimony_model"

type Context = object
  counter: int

proc trViolating(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Each of these opens the node and recurses into every child. VIOLATION.
  case n.stmtKind
  of ComesfromS:
    copyInto dest, n:
      while n.hasMore:
        trViolating c, dest, n
  of PragmaxS:
    copyInto dest, n:
      while n.hasMore:
        trViolating c, dest, n
  else:
    takeTree dest, n

proc trConforming(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## The same walks done right: the leading operand is discharged first.
  case n.stmtKind
  of ComesfromS:
    # `bodyInto` steps over the origin symbol itself.
    n.bodyInto:
      while n.hasMore:
        trConforming c, dest, n
  of PragmaxS:
    # The hand-written form the existing 25 sites use.
    copyInto dest, n:
      takeTree dest, n   # the pragma list
      while n.hasMore:
        trConforming c, dest, n
  of StmtsS, ScopeS:
    # Not operand-headed: every child really is a statement.
    copyInto dest, n:
      while n.hasMore:
        trConforming c, dest, n
  else:
    takeTree dest, n

proc trDispatching(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Lists the tags but never walks the children itself - it hands off. This is
  ## the shape of the big "generic container" group branches in `eraiser` and
  ## `deferstmts`, and it must NOT be reported: a branch that does not descend
  ## cannot get the leading operand wrong.
  case n.stmtKind
  of PragmaxS, ComesfromS, StmtsS, IfS, WhileS, CaseS:
    trConforming c, dest, n
  else:
    takeTree dest, n

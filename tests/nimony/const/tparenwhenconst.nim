import std/assertions

# Regression: a *parenthesised* `when` expression in a const initialiser lowers
# to `(expr (stmts) <value>)` — an empty leading statement list, then the
# selected branch. The compile-time evaluator (expreval) must skip the empty
# leading stmts and fold the trailing value; it previously evaluated the first
# child and failed with `cannot evaluate expression at compile time:
# (; <value>)`. (Bare `when`, and parens around a plain expr, already folded.)

const B = (when false: 10 else: 20)
const C = (when true: 30 else: 40)
const F = (when defined(linux): 1'i32 else: 2'i32)   # the std/ioring SOL_SOCKET shape

# parens around a nested when, and a leading-blank-stmt grouping, still fold:
const G = (when true: (when false: 1 else: 2) else: 3)

assert B == 20
assert C == 30
assert F == (when defined(linux): 1'i32 else: 2'i32)
assert G == 2

# guards: the forms that already worked must keep working
const bare = when true: 5 else: 6
const paren = (7)
assert bare == 5
assert paren == 7

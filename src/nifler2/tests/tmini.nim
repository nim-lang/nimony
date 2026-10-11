## End-to-end test of the generated parser: source text in, tree out.

import std / [syncio, strutils]
import mini

proc parse(src: string): string =
  var p = openParser(src)
  pModule p
  if p.tok.kind != tkEof:
    error p, "unexpected trailing input"
  result = render(p)
  if p.errors.len > 0:
    result = "ERR: " & p.errors.join(" | ")

proc check(name, src, expected: string) =
  let got = parse(src)
  if got == expected:
    echo "ok   ", name
  else:
    echo "FAIL ", name
    echo "  want: ", expected
    echo "  got:  ", got

check "assignment", "x = 1",
  "(stmts (asgn x 1))"

check "command", "foo a b",
  "(stmts (cmd foo a b))"

check "bare expr", "x",
  "(stmts x)"

check "semicolons", "x = 1; y = 2",
  "(stmts (asgn x 1) (asgn y 2))"

check "two lines", "x = 1\ny = 2",
  "(stmts (asgn x 1) (asgn y 2))"

check "indented block", "if c:\n  y = 2\n  bar z",
  "(stmts (if c (asgn y 2) (cmd bar z)))"

check "inline body", "if c: y = 2",
  "(stmts (if c (asgn y 2)))"

check "nested blocks", "while d:\n  if e:\n    q = 3\n  r = 4",
  "(stmts (while d (if e (asgn q 3)) (asgn r 4)))"

check "block then sibling", "if c:\n  y = 2\nz = 3",
  "(stmts (if c (asgn y 2)) (asgn z 3))"

check "left assoc of command", "f a b c",
  "(stmts (cmd f a b c))"

# --- errors the indentation machinery must catch
check "bad dedent", "if c:\n  y = 2\n r = 3",
  "ERR: (3,1) invalid indentation, got 'r' | (3,1) unexpected trailing input, got 'r'"

# --- precedence climbing (`binary`)
check "precedence", "x = 1 + 2 * 3",
  "(stmts (asgn x (infix + 1 (infix * 2 3))))"

check "precedence left", "x = 1 - 2 - 3",
  "(stmts (asgn x (infix - (infix - 1 2) 3)))"

check "precedence right", "x = 1 ^ 2 ^ 3",
  "(stmts (asgn x (infix ^ 1 (infix ^ 2 3))))"

check "parens beat precedence", "x = (1 + 2) * 3",
  "(stmts (asgn x (infix * (infix + 1 2) 3)))"

# `binary` requires the operator to be on the same line, so `+ 2` is not
# folded into the previous expression. This grammar has no prefix-operator
# rule, so what is left cannot start a statement either -- which is exactly
# how the rejection shows up.
check "operator must not start a line", "x = 1\n+ 2",
  "ERR: (2,0) unexpected trailing input, got '+'"

# --- the `^tag[...]` anchor: left-associative postfix chains
check "dot chain", "a.b.c",
  "(stmts (dot (dot a b) c))"

check "call", "f(1, 2)",
  "(stmts (call f 1 2))"

check "index", "a[i]",
  "(stmts (at a i))"

check "mixed chain", "a.b(c).d[e]",
  "(stmts (at (dot (call (dot a b) c) d) e))"

check "call with no args", "f()",
  "(stmts (call f))"

# --- the semantic predicate: a space before '(' makes it a command
check "space makes a command", "f (1)",
  "(stmts (cmd f 1))"

check "no space is a call", "f(1)",
  "(stmts (call f 1))"

check "suffix inside a block", "if c:\n  a.b = 1",
  "(stmts (if c (asgn (dot a b) 1)))"

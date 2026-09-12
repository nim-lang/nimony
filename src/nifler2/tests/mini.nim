## A miniature indentation-based language, used to exercise the generator end
## to end before the full Nim grammar is wired up. It is deliberately the
## smallest grammar that uses every mechanism the design rests on:
##
## * an indentation block (`indented`, `IND{=}`) and a guard rule (`notInd`)
## * retroactive wrapping after automatic left factoring (`exprStmt`)
## * a declared fallback (`%else`)
## * a left-associative postfix chain via the `^tag[...]` anchor (`suffix`)
## * a semantic predicate (`&noSpaceBefore`: `f(x)` is a call, `f (x)` a command)
## * precedence climbing (`binary`) and a parameterized rule
## * separated repetition, both with a terminal and with a bare guard

grammar:
  module "stmts[ stmt ^* (';' | IND{=}) ]"

  notInd "NO_IND | IND{=} | IND{<}"

  stmt "if[ 'if' expr({-1}) ':' body ]"
  stmt "while[ 'while' expr({-1}) ':' body ]"
  stmt "%else exprStmt"

  body "indented( stmt ^+ (';' | IND{=}) )"
  body "notInd stmt"

  exprStmt "asgn[ expr({-1}) '=' expr({-1}) ]"
  exprStmt "cmd[ expr({-1}) expr({-1})+ ]"
  exprStmt "expr({-1})"

  expr(limit: int) "binary(primary, getPrecedence, isRightAssoc, infix)"

  primary "atom suffix*"

  suffix "&noSpaceBefore ^call[ '(' expr({-1}) ^* ',' ')' ]"
  suffix "&noSpaceBefore ^at[ '[' expr({-1}) ']' ]"
  suffix "^dot[ '.' IDENT ]"

  atom "IDENT"
  atom "INT_LIT"
  atom "'(' expr({-1}) ')'"

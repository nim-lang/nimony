#
#
#           Nimony Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Annotations for compiler pass verification.
##
## These pragmas are ignored by the Nim compiler but parsed by `check_tags`
## from the nifled output to derive what each proc consumes and produces.
##
## Usage:
##   proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
##     {.ensuresNif: addedExpr(dest).}
##
## Procs annotated with ensuresNif must have unique names (no overloading).
## Use `include` not `import` since custom pragmas can't be exported.

template ensuresNif*(x: untyped) {.pragma.}
template requiresNif*(x: untyped) {.pragma.}

# ---------------------------------------------------------------------------
# Postconditions: what a proc adds to `dest`
# ---------------------------------------------------------------------------

template addedExpr*(x: untyped): untyped = x    ## Adds one expression child
template addedType*(x: untyped): untyped = x    ## Adds one type child
template addedStmt*(x: untyped): untyped = x    ## Adds one statement child
template addedDef*(x: untyped): untyped = x     ## Adds one SymbolDef child
template addedSym*(x: untyped): untyped = x     ## Adds one symbol use child
template addedLit*(x: untyped): untyped = x     ## Adds one literal child
template addedAny*(x: untyped): untyped = x     ## Adds one child of unknown kind
template addedDot*(x: untyped): untyped = x     ## Adds one DotToken
template addedNothing*(x: untyped): untyped = x ## Adds nothing to dest

# ---------------------------------------------------------------------------
# Preconditions: what a proc expects from `n`
# ---------------------------------------------------------------------------

template isExpr*(x: untyped): untyped = x       ## Cursor at an expression
template isType*(x: untyped): untyped = x       ## Cursor at a type
template isStmt*(x: untyped): untyped = x       ## Cursor at a statement
template isAny*(x: untyped): untyped = x        ## Cursor at any node

#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[

Hexer
------

Hexer is our middle-end. It transforms Nimony code into NIFC code. This requires
multiple different steps.

- Iterator inlining.
- Lambda lifting.
- Inject dups.
- Lower control flow expressions to control flow statements (eliminate the expr/nkStmtListExpr construct).
- Inject destructors.
- Map builtins like `new` and `+` to "compiler procs".
- Translate exception handling.


NIFC generation
~~~~~~~~~~~~~~~

- It copies used imported symbols into the current NIF file. As a fix point operation
  until no foreign symbols are left.
- `importc`'ed symbols are replaced by their `.c` variants.
- `importc`'ed symbols might lead to `(incl "file.h")` injections.
- Nim types must be translated to NIFC types.
- Types and procs must be moved to toplevel statements.


Grammar
-------

Hexer accepts Nimony's grammar.

]##

## The CLI lives in the `shoggoth` binary (`src/lengc/shoggoth/shoggoth.nim`),
## which runs these entry points as its `c`, `d`, `dl` and `de` commands next to
## the optional NIFC optimizer (`shoggoth opt`).

import lengcgen, dce2

export expand, deadCodeElimination, computeLiveSet, dceEmit

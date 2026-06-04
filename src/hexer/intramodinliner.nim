#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Intra-module NIFC inliner.
##
## This is the last Hexer-side preparation pass: it inlines same-module calls
## whose callee already carries NIFC's `(inline ...)` pragma, then enriches that
## pragma with the parameter-use weights consumed later by the inter-module
## inliner in the NIFC optimizer.

import dce_inliner

export intraModuleInline

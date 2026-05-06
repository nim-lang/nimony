## nifprims-based prelude — drop-in replacement for `nifprelude` in modules
## that have been ported to the virtual-ParRi in-memory representation.
##
## `nifprims` subsumes both `nifcursors` and `nifstreams`; `nifreader` and
## `nifbuilder` (text I/O) are unchanged from the original prelude.

{.push warning[UnusedImport]: off.}

import bitabs, nifprims, lineinfos, nifreader, nifbuilder

{.pop.}

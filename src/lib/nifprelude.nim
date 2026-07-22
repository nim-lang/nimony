## imports the set of NIF related modules that have won

{.push warning[UnusedImport]: off.}

import bitabs, nifpools, lineinfos, nifbuilder
from nifreader import nil   # text reader; qualified-only: its NifKind must
                            # not shadow nifcore's token kinds

{.pop.}

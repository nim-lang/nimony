## imports the set of NIF related modules that have won

{.push warning[UnusedImport]: off.}

import bitabs, nifpools, nifbuilder
import nifroles           # role pragmas the validator reads off declarations
export nifroles
from nifreader import nil   # text reader; qualified-only: its NifKind must
                            # not shadow nifcore's token kinds

{.pop.}

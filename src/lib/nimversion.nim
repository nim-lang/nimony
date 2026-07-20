#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Single source of truth for the toolchain version string. Every tool
## (nimony, nimsem, hexer, lengc, nifler, nifmake) imports `Version` from here
## instead of `slurp`ing `doc/version.md` on its own — so the relative path to
## the version file lives in exactly one place.
##
## `slurp` resolves the path relative to *this* file (`src/lib/`), which is the
## same depth as every tool's own main module (`src/<tool>/`), so the literal
## `../../doc/version.md` is unchanged from the copies it replaces.

import std / syncio  # `slurp` is a `std/syncio` proc under nimony (a system
                     # magic under the host Nim bootstrap compiler).

const
  Version* = slurp("../../doc/version.md")

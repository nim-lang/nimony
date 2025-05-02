#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Module plugin handling

import std / [tables, sets, syncio, formatfloat, assertions, strutils]
include nifprelude
import nimony_model, semdata

import ".." / gear2 / modnames

proc handleTypePlugins*(c: var SemContext) =
  for k, _ in c.pendingTypePlugins:
    c.pluginBlacklist.incl(k)
  c.pendingTypePlugins.clear()

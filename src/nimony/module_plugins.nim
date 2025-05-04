#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Module plugin handling

import std / [tables, sets, syncio, formatfloat, assertions, strutils]
include nifprelude
import nimony_model, semdata, semos

import ".." / gear2 / modnames

proc handleTypePlugins*(c: var SemContext) =
  var inp = move c.dest

  for k, v in c.pendingTypePlugins:
    c.pluginBlacklist.incl(v)

    var types = createTokenBuf(30)
    types.addParLe StmtsS, NoLineInfo
    types.addSymUse k, NoLineInfo
    types.addParRi()

    var dest = createTokenBuf(3000)
    runPlugin(c, dest, NoLineInfo, pool.strings[v], inp.toString, types.toString)
    inp = ensureMove dest

  for k in c.pendingModulePlugins:
    c.pluginBlacklist.incl(k)
    var dest = createTokenBuf(3000)
    runPlugin(c, dest, NoLineInfo, pool.strings[k], inp.toString)
    inp = ensureMove dest

  c.pendingTypePlugins.clear()
  c.pendingModulePlugins.shrink(0)

  c.dest = ensureMove inp

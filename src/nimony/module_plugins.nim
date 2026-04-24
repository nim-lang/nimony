#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Module plugin handling

import std / [tables, sets, hashes, syncio, formatfloat, assertions, strutils]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, semdata, semos

import ".." / gear2 / modnames

proc handleTypePlugins*(c: var SemContext; dest: var TokenBuf) =
  var inp = move dest

  # Snapshot the pending lists first because the loop bodies mutate other
  # fields of `c`; borrowing `c.pendingTypePlugins`/`c.pendingModulePlugins`
  # for the whole iteration would block that.
  var typePlugins: seq[(SymId, StrId, PackedLineInfo)] = @[]
  for k, vInfo in c.pendingTypePlugins:
    typePlugins.add (k, vInfo[0], vInfo[1])
  var modulePlugins: seq[(StrId, PackedLineInfo)] = @[]
  for kInfo in c.pendingModulePlugins:
    modulePlugins.add (kInfo[0], kInfo[1])
  c.pendingTypePlugins.clear()
  c.pendingModulePlugins.shrink(0)

  for (k, v, info) in typePlugins:
    c.pluginBlacklist.incl(v)

    var types = createTokenBuf(30)
    types.addParLe StmtsS, NoLineInfo
    types.addSymUse k, NoLineInfo
    types.addParRi()

    var destB = createTokenBuf(3000)
    runPlugin(c, destB, info, pool.strings[v], inp.toString, types.toString)
    inp = ensureMove destB

  for (k, info) in modulePlugins:
    c.pluginBlacklist.incl(k)
    var destB = createTokenBuf(3000)
    runPlugin(c, destB, info, pool.strings[k], inp.toString)
    inp = ensureMove destB

  dest = ensureMove inp

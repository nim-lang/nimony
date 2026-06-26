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

  for k, p in c.pendingTypePlugins:
    c.pluginBlacklist.incl(p.path)

    var types = createTokenBuf(30)
    types.addParLe StmtsS, NoLineInfo
    types.addSymUse k, NoLineInfo
    types.addParRi()

    var destB = createTokenBuf(3000)
    runPlugin(c, destB, p.info, pool.strings[p.path], inp.toString, types.toString)
    inp = ensureMove destB

  for p in c.pendingModulePlugins:
    c.pluginBlacklist.incl(p.path)
    # Prepend the module name as the first child of the input so a shared
    # plugin can dispatch on which module triggered it. Same convention as
    # template and for-loop plugins — the plugin reads it with `pluginName`.
    var named = createTokenBuf(inp.len + 4)
    var ic = beginRead(inp)
    named.takeToken ic # (stmts
    named.add identToken(pool.strings.getOrIncl(c.thisModuleSuffix), p.info)
    while ic.hasMore:
      named.takeTree ic
    named.addParRi()
    var destB = createTokenBuf(3000)
    runPlugin(c, destB, p.info, pool.strings[p.path], named.toString)
    inp = ensureMove destB

  c.pendingTypePlugins.clear()
  c.pendingModulePlugins.shrink(0)

  dest = ensureMove inp

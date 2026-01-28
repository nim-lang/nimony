#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## NIF set-of-modules handling.

include ".." / lib / nifprelude
import std / [assertions, tables]
import symparser, nifindexes

type
  NifModule = ref object
    stream: nifstreams.Stream
    index: Table[string, NifIndexEntry]  # Simple embedded index for offsets

  NifProgram* = object # a NIF program is a set of NIF modules
    mods: Table[string, NifModule]
    scheme: SplittedModulePath

proc setupNifProgram*(scheme: sink SplittedModulePath): NifProgram =
  result = NifProgram(scheme: scheme, mods: initTable[string, NifModule]())

proc lookupDeclaration*(c: var NifProgram; s: SplittedSymName): TokenBuf =
  if s.module == "":
    raiseAssert "Cannot lookup declaration without module name: " & s.name
  else:
    var m: NifModule
    if not c.mods.hasKey(s.module):
      c.scheme.name = s.module
      var stream = nifstreams.open($c.scheme)
      let index = readEmbeddedIndex(stream)
      m = NifModule(stream: stream, index: index)
      c.mods[s.module] = m
    else:
      m = c.mods[s.module]

    let entry = m.index.getOrDefault($s)
    if entry.offset == 0:
      raiseAssert "Symbol not found in NIF module: " & $s
    else:
      result = createTokenBuf()
      m.stream.r.jumpTo entry.offset
      nifcursors.parse(m.stream, result, entry.info)

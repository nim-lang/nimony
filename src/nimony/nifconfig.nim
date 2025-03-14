#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Read the configuration from the `.cfg.nif` file.

import std / [sets, tables]

include nifprelude

type
  NifConfig* = object
    defines*: HashSet[string]
    paths*, nimblePaths*: seq[string]
    currentPath*: string
    nifcachePath*: string
    bits*: int
    compat*: bool

proc parseConfig(c: Cursor; result: var NifConfig) =
  var c = c
  var nested = 0
  while true:
    case c.kind
    of ParLe:
      inc nested
      case pool.tags[c.tag]
      of "defines":
        inc c
        while c.kind != ParRi:
          if c.kind == StringLit:
            result.defines.incl pool.strings[c.litId]
          inc c
      of "paths":
        inc c
        while c.kind != ParRi:
          if c.kind == StringLit:
            result.paths.add pool.strings[c.litId]
          inc c
      of "nimblepaths":
        inc c
        while c.kind != ParRi:
          if c.kind == StringLit:
            result.nimblePaths.add pool.strings[c.litId]
          inc c
      of "intbits":
        inc c
        if c.kind == IntLit:
          result.bits = pool.integers[c.intId]
          inc c
      of "compat":
        inc c
        if c.kind == IntLit:
          result.compat = bool(pool.integers[c.intId])
          inc c
      else:
        inc c
    of ParRi:
      dec nested
      if nested == 0: break
      inc c
    else:
      inc c

proc parseNifConfig*(configFile: string; result: var NifConfig) =
  var f = nifstreams.open(configFile)
  discard processDirectives(f.r)
  var buf = fromStream(f)
  var c = beginRead(buf)
  try:
    parseConfig(c, result)
  finally:
    f.close()

proc getOptionsAsOneString*(config: NifConfig): string =
  ## Returns the concatenation of options that affects generated files.
  result = "--cwd:" & config.currentPath

  for i in config.defines:
    result.add(" -d:" & i)

  result.add " --bits:" & $config.bits

when isMainModule:
  var conf = default(NifConfig)
  parseNifConfig "src/nifler/nifler.cfg.nif", conf
  echo $conf.bits

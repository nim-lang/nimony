#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Read the configuration from the `.cfg.nif` file.

import std / [os, sets, strutils]

import ".." / lib / platform

include ".." / lib / nifprelude

type
  TrackMode* = enum
    TrackNone, TrackUsages, TrackDef
  TrackPosition* = object
    mode*: TrackMode
    line*, col*: int32
    filename*: string

  NifConfig* = object
    defines*: HashSet[string]
    paths*, nimblePaths*: seq[string]
    baseDir*: string # base directory for the configuration system
    nifcachePath*: string
    bits*: int
    compat*: bool
    targetCPU*: TSystemCPU
    targetOS*: TSystemOS
    toTrack*: TrackPosition
    cc*: string
    linker*: string
    ccKey*: string

proc initNifConfig*(baseDir: sink string): NifConfig =
  result = NifConfig(
    baseDir: baseDir,
    nifcachePath: "nimcache",
    defines: toHashSet(["nimony"]),
    bits: sizeof(int)*8,
    targetCPU: platform.nameToCPU(system.hostCPU),
    targetOS: platform.nameToOS(system.hostOS),
    cc: "gcc",
    linker: ""
  )

proc setTargetCPU*(config: var NifConfig; symbol: string): bool =
  result = false
  let cpu = platform.nameToCPU(symbol)
  if cpu != cpuNone:
    config.targetCPU = cpu
    result = true

proc setTargetOS*(config: var NifConfig; symbol: string): bool =
  result = false
  let os = platform.nameToOS(symbol)
  if os != osNone:
    config.targetOS = os
    result = true

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
          result.bits = int pool.integers[c.intId]
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
  result = "--base:" & config.baseDir

  for i in config.defines:
    result.add(" -d:" & i)

  result.add " --bits:" & $config.bits
  result.add " --cpu:" & platform.CPU[config.targetCPU].name
  result.add " --os:" & platform.OS[config.targetOS].name

proc isDefined*(config: NifConfig; symbol: string): bool =
  if symbol in config.defines:
    result = true
  elif cmpIgnoreStyle(symbol, platform.CPU[config.targetCPU].name) == 0:
    result = true
  elif cmpIgnoreStyle(symbol, platform.OS[config.targetOS].name) == 0:
    result = true
  elif cmpIgnoreStyle(symbol, config.ccKey) == 0:
    result = true
  else:
    case symbol.normalize
    of "x86": result = config.targetCPU == cpuI386
    of "itanium": result = config.targetCPU == cpuIa64
    of "x8664": result = config.targetCPU == cpuAmd64
    of "posix", "unix":
      result = config.targetOS in {osLinux, osMorphos, osSkyos, osIrix, osPalmos,
                            osQnx, osAtari, osAix,
                            osHaiku, osVxWorks, osSolaris, osNetbsd,
                            osFreebsd, osOpenbsd, osDragonfly, osMacosx, osIos,
                            osAndroid, osNintendoSwitch, osFreeRTOS, osCrossos, osZephyr, osNuttX}
    of "linux":
      result = config.targetOS in {osLinux, osAndroid}
    of "bsd":
      result = config.targetOS in {osNetbsd, osFreebsd, osOpenbsd, osDragonfly, osCrossos}
    of "freebsd":
      result = config.targetOS in {osFreebsd, osCrossos}
    of "emulatedthreadvars":
      result = platform.OS[config.targetOS].props.contains(ospLacksThreadVars)
    of "msdos": result = config.targetOS == osDos
    of "mswindows", "win32": result = config.targetOS == osWindows
    of "macintosh":
      result = config.targetOS in {osMacos, osMacosx, osIos}
    of "osx", "macosx":
      result = config.targetOS in {osMacosx, osIos}
    of "sunos": result = config.targetOS == osSolaris
    of "freertos", "lwip":
      result = config.targetOS == osFreeRTOS
    of "littleendian": result = CPU[config.targetCPU].endian == littleEndian
    of "bigendian": result = CPU[config.targetCPU].endian == bigEndian
    of "cpu8": result = config.bits == 8
    of "cpu16": result = config.bits == 16
    of "cpu32": result = config.bits == 32
    of "cpu64": result = config.bits == 64
    of "nimrawsetjmp":
      result = config.targetOS in {osSolaris, osNetbsd, osFreebsd, osOpenbsd,
                            osDragonfly, osMacosx}
    else: result = false

when isMainModule:
  var conf = default(NifConfig)
  parseNifConfig "src/nifler/nifler.cfg.nif", conf
  echo $conf.bits

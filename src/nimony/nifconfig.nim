#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Read the configuration from the `.cfg.nif` file.

import std / [os, sets, strutils]
when defined(nimony):
  import std / syncio
else:
  import std / sequtils

import ".." / lib / platform

include ".." / lib / nifprelude

when defined(nimony):
  func addUnique(s: var seq[string]; x: sink string) =
    for i in 0 ..< s.len:
      if s[i] == x: return
    s.add x

  # `system.hostCPU`/`hostOS` are compile-time magics that Nimony doesn't
  # expose; derive the string values from `when defined(...)` branches.
  const
    hostCPU =
      when defined(amd64): "amd64"
      elif defined(i386): "i386"
      elif defined(arm64): "arm64"
      elif defined(arm): "arm"
      elif defined(riscv64): "riscv64"
      elif defined(powerpc64le): "powerpc64el"
      elif defined(powerpc64): "powerpc64"
      elif defined(powerpc): "powerpc"
      elif defined(mips64): "mips64"
      elif defined(mips): "mips"
      elif defined(sparc64): "sparc64"
      elif defined(sparc): "sparc"
      elif defined(wasm32): "wasm32"
      else: "amd64"
    hostOS =
      when defined(windows): "windows"
      elif defined(macosx): "macosx"
      elif defined(linux): "linux"
      elif defined(freebsd): "freebsd"
      elif defined(netbsd): "netbsd"
      elif defined(openbsd): "openbsd"
      elif defined(dragonfly): "dragonfly"
      elif defined(solaris): "solaris"
      elif defined(haiku): "haiku"
      elif defined(android): "android"
      elif defined(ios): "ios"
      else: "linux"

type
  TrackMode* = enum
    TrackNone, TrackUsages, TrackDef
  TrackPosition* = object
    mode*: TrackMode
    line*, col*: int32
    filename*: string

  AppType* = enum
    appConsole = "console"   # executable with console
    appGui = "gui"           # executable with GUI (no console on Windows)
    appLib = "lib"           # dynamic library (dll/so/dylib)
    appStaticLib = "staticlib" # static library (.a/.lib)

  Backend* = enum
    backendC = "c"
    backendLLVM = "llvm"

  NifConfig* = object
    defines*: seq[string]
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
    appType*: AppType
    backend*: Backend
    noValidate*: bool # skip running the validator on plugin sources
    verbose*: bool    # --verbose: dump NJ IR on contract/init failures
    outFile*: string  # filename portion set by `--out:PATH` / `-o:PATH`
                      # (empty = derive from module basename).
    outDir*: string   # directory portion set by `--out:DIR/NAME` (its
                      # dir half) and/or `--outdir:DIR`. Empty = cwd.

proc addDefine*(config: var NifConfig; symbol: string) =
  config.defines.addUnique symbol

proc initNifConfig*(baseDir: sink string): NifConfig =
  result = NifConfig(
    baseDir: baseDir,
    nifcachePath: "nimcache",
    defines: @["nimony"],
    bits: sizeof(int)*8,
    targetCPU: platform.nameToCPU(hostCPU),
    targetOS: platform.nameToOS(hostOS),
    cc: "gcc",
    linker: "",
    appType: appConsole # console is the default
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
            result.defines.addUnique pool.strings[c.litId]
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
    of "executable": result = config.appType in {appConsole, appGui}
    of "library": result = config.appType in {appLib, appStaticLib}
    of "dll": result = config.appType == appLib
    of "staticlib": result = config.appType == appStaticLib
    of "consoleapp": result = config.appType == appConsole
    of "guiapp": result = config.appType == appGui
    else: result = false

when isMainModule:
  var conf = default(NifConfig)
  parseNifConfig "src/nifler/nifler.cfg.nif", conf
  echo $conf.bits

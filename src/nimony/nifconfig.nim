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
# only `parse` — a full import would re-export nifcore and shadow the global
# `pool` / `createTokenBuf` the shim provides.
from ".." / lib / nifcoreparse import parse

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

const
  DefaultMM* = "atomicarc"
    ## `--mm:atomicArc`: the default strategy. Reference counting with atomic
    ## increments/decrements, so a `ref` may be shared between threads.
  MmPlaceholder* = "$MM"
    ## What `system.nim` writes instead of naming a strategy: `include "$MM"`.
    ## Expanded by `expandMM` to the module `--mm` selected. A new strategy is
    ## then a new file under `MmDir` and nothing else -- no `when` chain in
    ## `system.nim` that every strategy has to be added to.
  MmDir* = "system/"
    ## Where the strategy modules live, relative to `system.nim`.

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
    backendNative = "native"  # C-free: Leng -> arkham -> nifasm (static, libc-free)

  OptLevel* = enum
    optDebug   # default: -O1 (debug-friendly but avoids dumb codegen)
    optNone    # --opt:none: -O0
    optSize    # --opt:size: -Os
    optSpeed   # --opt:speed: -O3

  NifConfig* = object
    defines*: seq[string]
    mm*: string  ## `--mm:NAME`: the memory management strategy. Stored
                 ## `normalize`d, because it is used as a FILENAME (`MmDir & mm`)
                 ## and filenames stay all-lowercase, while the option is spelled
                 ## in camelCase (`--mm:atomicArc` -> `system/atomicarc`).
    paths*, nimblePaths*: seq[string]
    baseDir*: string # base directory for the configuration system
    nifcachePath*: string
    bits*: int
    bitsExplicit*: bool  ## `--bits:N` (or an `intbits` config row) was given, so
                         ## `--cpu` must NOT overwrite it. Without this the two
                         ## flags would resolve by ORDER, and `--bits:32 --cpu:arm32`
                         ## and `--cpu:arm32 --bits:32` would not mean the same thing.
    compat*: bool
    targetCPU*: TSystemCPU
    targetOS*: TSystemOS
    toTrack*: TrackPosition
    cc*: string
    linker*: string
    layoutFile*: string  # `--layout:FILE` — the BOARD, for a bare-metal target:
                         # its memory regions, stack slots and heap. Forwarded to
                         # arkham verbatim; empty on a hosted target, which has an
                         # OS to ask for memory instead of a file that says what
                         # the part has.
    ccKey*: string
    appType*: AppType
    backend*: Backend
    optLevel*: OptLevel
    noValidate*: bool # skip running the validator on plugin sources
    verbose*: bool    # --verbose: dump NJ IR on contract/init failures
    outFile*: string  # filename portion set by `--out:PATH` / `-o:PATH`
                      # (empty = derive from module basename).
    outDir*: string   # directory portion set by `--out:DIR/NAME` (its
                      # dir half) and/or `--outdir:DIR`. Empty = cwd.
    checkFlags*: string  # active check modes as a `genFlags` string (e.g. "br"),
                         # forwarded to `hexer c` so nifcgen injects only the
                         # requested runtime checks (empty = none).
    inlineFrames*: bool  # --inlineframes:on: record which template an expansion
                         # came from, so a debug backend can emit DWARF inlined
                         # frames for it (#1987). Off by default: it costs work
                         # in every template expansion and only a debug build
                         # reads it.

proc addDefine*(config: var NifConfig; symbol: string) =
  config.defines.addUnique symbol

proc initNifConfig*(baseDir: sink string): NifConfig =
  result = NifConfig(
    baseDir: baseDir,
    nifcachePath: "nimcache",
    defines: @["nimony"],
    mm: DefaultMM,
    bits: sizeof(int)*8,
    targetCPU: platform.nameToCPU(hostCPU),
    targetOS: platform.nameToOS(hostOS),
    cc: "gcc",
    linker: "",
    appType: appConsole, # console is the default
    checkFlags: "br"     # = genFlags(DefaultSettings) (BoundCheck + RangeCheck);
                         # the normal compile path overrides from `--boundchecks` etc.
  )

proc setTargetCPU*(config: var NifConfig; symbol: string): bool =
  ## The CPU also decides how wide `int` is. It is not a separate question — a
  ## target whose `int` is not its word is a target nobody asked for — so naming
  ## the CPU answers it, and `--bits` stays for the rare case of contradicting
  ## the table on purpose. Before this, `--cpu:arm32` left `bits` at the HOST's
  ## width: the whole 32-bit target was type-checked with a 64-bit `int`.
  result = platform.findCPU(symbol, config.targetCPU)
  if result and not config.bitsExplicit:
    config.bits = platform.CPU[config.targetCPU].intSize

proc setTargetOS*(config: var NifConfig; symbol: string): bool =
  # `findOS`, not `nameToOS`: the first enum value is the bare-metal target now
  # (`osEmbedded`), so a typo must still be rejected rather than quietly
  # selecting it.
  result = platform.findOS(symbol, config.targetOS)

proc parseConfig(c: Cursor; result: var NifConfig) =
  ## Interprets the single tree at `c`; unknown tags are searched
  ## recursively for known sections.
  var c = c
  if c.isTagLit:
    case globalTags.tags[c.cursorTagId]
    of "defines":
      c.into:
        while c.hasMore:
          if c.isStringLit:
            result.defines.addUnique pool.strings[c.strId]
          skip c
    of "paths":
      c.into:
        while c.hasMore:
          if c.isStringLit:
            result.paths.add pool.strings[c.strId]
          skip c
    of "nimblepaths":
      c.into:
        while c.hasMore:
          if c.isStringLit:
            result.nimblePaths.add pool.strings[c.strId]
          skip c
    of "intbits":
      c.into:
        if c.isIntLit:
          result.bits = int c.intVal
          result.bitsExplicit = true
        while c.hasMore: skip c
    of "compat":
      c.into:
        if c.isIntLit:
          result.compat = bool(c.intVal)
        while c.hasMore: skip c
    of "mm":
      c.into:
        if c.isStringLit:
          result.mm = normalize(pool.strings[c.strId])
        while c.hasMore: skip c
    else:
      c.into:
        while c.hasMore:
          parseConfig(c, result)
          skip c

proc parseNifConfig*(configFile: string; result: var NifConfig) =
  var r = nifreader.open(configFile)
  var buf = createTokenBuf()
  nifcoreparse.parse(r, buf)   # reads directives + the tree into `buf`
  nifreader.close(r)
  var c = beginRead(buf)
  parseConfig(c, result)
proc getOptionsAsOneString*(config: NifConfig): string =
  ## Returns the concatenation of options that affects generated files.
  result = "--base:" & config.baseDir

  for i in config.defines:
    result.add(" -d:" & i)

  result.add " --mm:" & config.mm
  result.add " --bits:" & $config.bits
  result.add " --cpu:" & platform.CPU[config.targetCPU].name
  result.add " --os:" & platform.OS[config.targetOS].name

proc expandMM*(config: NifConfig; path: string): string =
  ## Expands the `$MM` placeholder in an `include` path to the module implementing
  ## the selected memory management strategy: `"$MM"` -> `"system/atomicarc"`.
  ## Applied before the path is resolved, so `resolveFile`'s own `$VAR` (environment
  ## variable) rule never sees it.
  result = path
  if result.endsWith(MmPlaceholder):
    when defined(nimony):
      result.shrink result.len - MmPlaceholder.len
    else:
      result.setLen result.len - MmPlaceholder.len
    result.add MmDir
    result.add config.mm

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
    # `arm` is what code that predates the `arm32` rename says, and what code
    # shared with Nim says. The canonical name moved; the predicate did not.
    of "arm": result = config.targetCPU == cpuArm
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
    # `--mm:arc` makes `defined(gcArc)` true, `--mm:atomicArc` `defined(gcAtomicArc)`,
    # and so on for a strategy this compiler has never heard of: `mm` is already
    # normalized, and so is `symbol` here, so the two spellings meet.
    else: result = config.mm.len > 0 and symbol.normalize == "gc" & config.mm

when isMainModule:
  var conf = default(NifConfig)
  parseNifConfig "src/nifler/nifler.cfg.nif", conf
  echo $conf.bits

#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Common command-line option parsing for nimony and nimsem.

import std / [strutils, sets]
import nifconfig, langmodes
import ".." / lib / argsfinder

proc parseTrack(s: string; mode: TrackMode): TrackPosition =
  ## Parse file,line,col format for --usages and --def options
  var i = 0
  var line = 0'i32
  var col = 0'i32
  while i < s.len and s[i] != ',':
    inc i
  let filenameEnd = i
  if i < s.len and s[i] == ',': inc i

  while i < s.len and s[i] in {'0'..'9'}:
    line = line * 10'i32 + (ord(s[i]) - ord('0')).int32
    inc i
  if i < s.len and s[i] == ',': inc i
  while i < s.len and s[i] in {'0'..'9'}:
    col = col * 10'i32 + (ord(s[i]) - ord('0')).int32
    inc i
  result = TrackPosition(mode: mode, line: line, col: col, filename: s.substr(0, filenameEnd-1))

proc parseCommonOption*(key, val: string; config: var NifConfig;
                        moduleFlags: var set[ModuleFlag];
                        forwardArg: var bool; forwardArgNifc: var bool;
                        helpMsg = ""; versionMsg = ""): bool =
  ## Parses common command-line options shared between nimony and nimsem.
  ## Returns true if the option was recognized and handled.
  ## Sets forwardArg to true if the option should be forwarded to sub-tools.
  ## Sets forwardArgNifc to true if the option should be forwarded to nifc.
  ## Optional helpMsg and versionMsg provide custom help/version text.
  forwardArg = true
  forwardArgNifc = false
  result = true

  case normalize(key)
  of "help", "h":
    quit(if helpMsg.len > 0: helpMsg else: "No help available", QuitSuccess)
  of "version", "v":
    quit(if versionMsg.len > 0: versionMsg else: "No version available", QuitSuccess)
  of "compat":
    config.compat = true
  of "path", "p":
    config.paths.add val
  of "define", "d":
    config.defines.incl val
  of "nosystem":
    moduleFlags.incl SkipSystem
  of "issystem":
    moduleFlags.incl IsSystem
    forwardArg = false
  of "ismain":
    moduleFlags.incl IsMain
    forwardArg = false
  of "bits":
    case val
    of "64": config.bits = 64
    of "32": config.bits = 32
    of "16": config.bits = 16
    else: quit "invalid value for --bits"
  of "cpu":
    if not config.setTargetCPU(val):
      quit "unknown CPU: " & val
  of "os":
    if not config.setTargetOS(val):
      quit "unknown OS: " & val
  of "app":
    forwardArg = true  # Must forward to nimsem for defines to work!
    forwardArgNifc = true
    case normalize(val)
    of "console":
      config.appType = appConsole
    of "gui":
      config.appType = appGui
    of "lib":
      config.appType = appLib
    of "staticlib":
      config.appType = appStaticLib
    else:
      quit "invalid value for --app; expected console, gui, lib, or staticlib"
  of "cc":
    config.cc = val
    config.ccKey = extractCCKey(val)
  of "linker":
    config.linker = val
  of "base":
    config.baseDir = val
  of "nimcache":
    config.nifcachePath = val
    forwardArgNifc = true
  of "usages":
    if config.toTrack.mode == TrackNone:
      config.toTrack = parseTrack(val, TrackUsages)
    else:
      quit "only one --usages or --def can be used"
  of "def":
    if config.toTrack.mode == TrackNone:
      config.toTrack = parseTrack(val, TrackDef)
    else:
      quit "only one --usages or --def can be used"
  of "flags":
    # Flags are forwarded but not processed here
    discard
  else:
    result = false

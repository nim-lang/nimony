#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains a few procedures to control the *terminal*
## (also called *console*). It uses ANSI escape sequences. This is a
## trimmed port for Nimony of Nim's `std/terminal`. Windows support is
## intentionally limited to ANSI output on modern terminals; the legacy
## console API is not wrapped.

import std/[syncio]

const
  fgPrefix* = "\e[38;2;"
  bgPrefix* = "\e[48;2;"
  ansiResetCode* = "\e[0m"
  stylePrefix* = "\e["

type
  Style* = enum        ## Different styles for text output.
    styleBright = 1,   ## bright text
    styleDim,          ## dim text
    styleItalic,       ## italic
    styleUnderscore,   ## underscored text
    styleBlink,        ## blinking/bold text
    styleBlinkRapid,   ## rapid blinking/bold text
    styleReverse,      ## reverse
    styleHidden,       ## hidden text
    styleStrikethrough ## strikethrough

  ForegroundColor* = enum ## Terminal's foreground colors.
    fgBlack = 30,         ## black
    fgRed,                ## red
    fgGreen,              ## green
    fgYellow,             ## yellow
    fgBlue,               ## blue
    fgMagenta,            ## magenta
    fgCyan,               ## cyan
    fgWhite,              ## white
    fg8Bit,               ## 256-color (not supported)
    fgDefault             ## default terminal foreground color

  BackgroundColor* = enum ## Terminal's background colors.
    bgBlack = 40,         ## black
    bgRed,                ## red
    bgGreen,              ## green
    bgYellow,             ## yellow
    bgBlue,               ## blue
    bgMagenta,            ## magenta
    bgCyan,               ## cyan
    bgWhite,              ## white
    bg8Bit,               ## 256-color (not supported)
    bgDefault             ## default terminal background color

  TerminalCmd* = enum ## Commands that can be expressed as arguments.
    resetStyle,       ## reset attributes
    fgColor,          ## set foreground's true color (unsupported here)
    bgColor           ## set background's true color (unsupported here)

proc ansiStyleCode*(style: int): string =
  result = stylePrefix & $style & "m"

proc ansiStyleCode*(style: Style): string =
  ansiStyleCode(ord(style))

proc ansiForegroundColorCode*(fg: ForegroundColor; bright = false): string =
  var code = ord(fg)
  if bright: code = code + 60
  ansiStyleCode(code)

proc ansiBackgroundColorCode*(bg: BackgroundColor; bright = false): string =
  var code = ord(bg)
  if bright: code = code + 60
  ansiStyleCode(code)

proc resetAttributes*(f: File) =
  ## Resets all attributes.
  write f, ansiResetCode

proc setStyle*(f: File; style: Style) =
  ## Sets a single terminal style attribute.
  write f, ansiStyleCode(style)

proc setStyle*(f: File; style: set[Style]) =
  ## Sets the terminal style.
  for s in style:
    write f, ansiStyleCode(s)

proc setForegroundColor*(f: File; fg: ForegroundColor; bright = false) =
  ## Sets the terminal's foreground color.
  write f, ansiForegroundColorCode(fg, bright)

proc setBackgroundColor*(f: File; bg: BackgroundColor; bright = false) =
  ## Sets the terminal's background color.
  write f, ansiBackgroundColorCode(bg, bright)

when defined(posix):
  proc c_isatty(fildes: cint): cint {.importc: "isatty", header: "<unistd.h>".}
  proc c_fileno(f: File): cint {.importc: "fileno", header: "<stdio.h>".}
elif defined(windows):
  proc c_isatty(fildes: cint): cint {.importc: "_isatty", header: "<io.h>".}
  proc c_fileno(f: File): cint {.importc: "_fileno", header: "<stdio.h>".}

proc isatty*(f: File): bool =
  ## Returns true if `f` is associated with a terminal device.
  when defined(posix) or defined(windows):
    result = c_isatty(c_fileno(f)) != 0'i32
  else:
    result = false

# Overloads used by the `styledWrite` and `styledWriteLine` templates.
# These dispatch on argument type through Nimony's `unpack()` loop.

proc styledEchoProcessArg*(f: File; s: string) = write f, s
proc styledEchoProcessArg*(f: File; c: char) = write f, c
proc styledEchoProcessArg*(f: File; style: Style) = setStyle(f, style)
proc styledEchoProcessArg*(f: File; style: set[Style]) = setStyle(f, style)
proc styledEchoProcessArg*(f: File; color: ForegroundColor) =
  setForegroundColor(f, color)
proc styledEchoProcessArg*(f: File; color: BackgroundColor) =
  setBackgroundColor(f, color)
proc styledEchoProcessArg*(f: File; cmd: TerminalCmd) =
  case cmd
  of resetStyle: resetAttributes(f)
  of fgColor, bgColor: discard

template styledWrite*(f: File) {.varargs.} =
  ## Similar to `write`, but treating terminal style arguments specially.
  ## When an argument is `Style`, `set[Style]`, `ForegroundColor`,
  ## `BackgroundColor` or `TerminalCmd` the corresponding terminal style
  ## proc is called instead of writing the value directly.
  for x in unpack():
    styledEchoProcessArg(f, x)
  resetAttributes(f)

template styledWriteLine*(f: File) {.varargs.} =
  ## Calls `styledWrite` and appends a newline at the end.
  for x in unpack():
    styledEchoProcessArg(f, x)
  write f, '\n'
  resetAttributes(f)

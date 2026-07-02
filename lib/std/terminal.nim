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

when defined(nimNativeIo) and defined(posix):
  # Libc-free: a `File` carries a raw fd (no `FILE*`, so no `fileno`), and `isatty`
  # is itself a libc helper — implement it as `ioctl(fd, TCGETS)` (== 0 ⇒ a tty),
  # the same probe glibc's `isatty` uses. TCGETS is 0x5401 on x86-64 and AArch64.
  proc nativeIoctl(fd: cint; request: uint; arg: pointer): cint {.
    importc: "ioctl", sideEffect.}
  proc c_fileno(f: File): cint = getFileHandle(f)
  proc c_isatty(fildes: cint): cint =
    var termbuf {.noinit.}: array[64, byte]   # holds a `struct termios` (~60B)
    if nativeIoctl(fildes, 0x5401'u, addr termbuf) == 0'i32: 1'i32 else: 0'i32
elif defined(posix):
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

# ------------------------------------------------------------------------
# Fluent string styling, in the spirit of the npm `colors` package.
#
# In addition to the `File`-oriented procs above, these `func`s return the
# styled *string*: each wraps its argument in the matching ANSI SGR escape
# sequence, so they compose through method-call syntax. `"err".red.bold` is
# just `bold(red("err"))`, and because every styler emits its own reset code
# the wrappers nest correctly:
#
#   ```nim
#   echo "this is red and bold".red.bold
#   echo "warning".yellow
#   echo "selected".black.onWhite
#   ```
#
# Backgrounds are named `on<Colour>` (e.g. `onRed`) rather than `bg<Colour>`
# to avoid clashing with the `BackgroundColor` enum values above, and because
# `"x".black.onWhite` reads naturally in a chain. Escape codes are always
# emitted; pass a result through `stripAnsi` to recover the plain text.

func sgrWrap(s: string; on, off: int): string {.inline.} =
  stylePrefix & $on & "m" & s & stylePrefix & $off & "m"

# --- foreground colours (reset with 39) ---

func black*(s: string): string = sgrWrap(s, 30, 39)
  ## Colours `s` black.
func red*(s: string): string = sgrWrap(s, 31, 39)
  ## Colours `s` red.
func green*(s: string): string = sgrWrap(s, 32, 39)
  ## Colours `s` green.
func yellow*(s: string): string = sgrWrap(s, 33, 39)
  ## Colours `s` yellow.
func blue*(s: string): string = sgrWrap(s, 34, 39)
  ## Colours `s` blue.
func magenta*(s: string): string = sgrWrap(s, 35, 39)
  ## Colours `s` magenta.
func cyan*(s: string): string = sgrWrap(s, 36, 39)
  ## Colours `s` cyan.
func white*(s: string): string = sgrWrap(s, 37, 39)
  ## Colours `s` white.
func gray*(s: string): string = sgrWrap(s, 90, 39)
  ## Colours `s` gray (bright black).
func grey*(s: string): string = sgrWrap(s, 90, 39)
  ## Alias for `gray`.

# --- bright foreground colours ---

func brightRed*(s: string): string = sgrWrap(s, 91, 39)
func brightGreen*(s: string): string = sgrWrap(s, 92, 39)
func brightYellow*(s: string): string = sgrWrap(s, 93, 39)
func brightBlue*(s: string): string = sgrWrap(s, 94, 39)
func brightMagenta*(s: string): string = sgrWrap(s, 95, 39)
func brightCyan*(s: string): string = sgrWrap(s, 96, 39)
func brightWhite*(s: string): string = sgrWrap(s, 97, 39)

# --- background colours (reset with 49) ---

func onBlack*(s: string): string = sgrWrap(s, 40, 49)
  ## Renders `s` on a black background.
func onRed*(s: string): string = sgrWrap(s, 41, 49)
  ## Renders `s` on a red background.
func onGreen*(s: string): string = sgrWrap(s, 42, 49)
  ## Renders `s` on a green background.
func onYellow*(s: string): string = sgrWrap(s, 43, 49)
  ## Renders `s` on a yellow background.
func onBlue*(s: string): string = sgrWrap(s, 44, 49)
  ## Renders `s` on a blue background.
func onMagenta*(s: string): string = sgrWrap(s, 45, 49)
  ## Renders `s` on a magenta background.
func onCyan*(s: string): string = sgrWrap(s, 46, 49)
  ## Renders `s` on a cyan background.
func onWhite*(s: string): string = sgrWrap(s, 47, 49)
  ## Renders `s` on a white background.

# --- styles (each with its own reset) ---

func bold*(s: string): string = sgrWrap(s, 1, 22)
  ## Renders `s` bold.
func dim*(s: string): string = sgrWrap(s, 2, 22)
  ## Renders `s` dim (faint).
func italic*(s: string): string = sgrWrap(s, 3, 23)
  ## Renders `s` italic.
func underline*(s: string): string = sgrWrap(s, 4, 24)
  ## Underlines `s`.
func inverse*(s: string): string = sgrWrap(s, 7, 27)
  ## Swaps the foreground and background of `s`.
func hidden*(s: string): string = sgrWrap(s, 8, 28)
  ## Renders `s` hidden (not displayed).
func strikethrough*(s: string): string = sgrWrap(s, 9, 29)
  ## Renders `s` with a line through it.

# --- ANSI parsing ---

func stripAnsi*(s: string): string =
  ## Removes ANSI SGR escape sequences (`\e[ ... m`) from `s`, yielding the
  ## plain text.
  result = ""
  var i = 0
  let n = s.len
  while i < n:
    if s[i] == '\e' and i + 1 < n and s[i+1] == '[':
      i += 2
      while i < n and s[i] != 'm':
        inc i
      if i < n: inc i          # skip the terminating 'm'
    else:
      result.add s[i]
      inc i

func visibleLen*(s: string): int =
  ## The number of visible characters in `s`, ignoring ANSI escape sequences.
  stripAnsi(s).len

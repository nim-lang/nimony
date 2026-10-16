#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains a few procedures to control the *terminal*
## (also called *console*). Styles and colors are written as ANSI escape
## sequences on every platform, so on Windows they need a terminal with
## virtual terminal processing (Windows Terminal, or Windows 10+ conhost after
## `enableTrueColors`). Cursor movement, erasing and the terminal size use the
## console API on Windows, like Nim's `std/terminal`.
##
## True color (`Color` from `std/colors`) is off until `enableTrueColors` is
## called, as in Nim; `ansiForegroundColorCode(Color)` and
## `ansiBackgroundColorCode(Color)` always return the escape sequence.
##
## Not ported: `getch`, `readPasswordFromStdin`, `getCursorPos`,
## `setCursorYPos` and `writeStyled`.

import std/[syncio, colors, envvars]
from std/strutils import toLowerAscii

when defined(windows):
  import std/windows/winlean

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
    fgColor,          ## following `Color` arguments set the foreground
    bgColor           ## following `Color` arguments set the background

var
  trueColorSupported {.threadvar.}: bool
  trueColorEnabled {.threadvar.}: bool
  trueColorSetsFg {.threadvar.}: bool  # which side a `Color` arg of `styledWrite` sets

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

proc ansiForegroundColorCode*(color: Color): string =
  ## The escape sequence that sets the foreground to the true color `color`:
  ## `\e[38;2;R;G;Bm`.
  let rgb = extractRGB(color)
  result = fgPrefix & $rgb.r & ";" & $rgb.g & ";" & $rgb.b & "m"

proc ansiBackgroundColorCode*(color: Color): string =
  ## The escape sequence that sets the background to the true color `color`:
  ## `\e[48;2;R;G;Bm`.
  let rgb = extractRGB(color)
  result = bgPrefix & $rgb.r & ";" & $rgb.g & ";" & $rgb.b & "m"

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
elif defined(nimNativeIo) and defined(windows):
  # Libc-free: a `File` wraps a Win32 `HANDLE`, not an msvcrt `FILE*`, so there is
  # no `_fileno`/`_isatty` to call. `GetConsoleMode` succeeds only for console
  # handles, which is exactly the "is this a terminal?" probe we need.
  proc c_fileno(f: File): Handle = getFileHandle(f)
  proc c_isatty(h: Handle): cint =
    var mode: DWORD = 0'u32
    if isSuccess(getConsoleMode(h, addr mode)): 1'i32 else: 0'i32
elif defined(posix):
  proc c_isatty(fildes: cint): cint {.importc: "isatty", header: "<unistd.h>".}
  proc c_fileno(f: File): cint {.importc: "fileno", header: "<stdio.h>".}
  proc nativeIoctl(fd: cint; request: uint; arg: pointer): cint {.
    importc: "ioctl", header: "<sys/ioctl.h>", sideEffect.}
elif defined(windows):
  proc c_isatty(fildes: cint): cint {.importc: "_isatty", header: "<io.h>".}
  proc c_fileno(f: File): cint {.importc: "_fileno", header: "<stdio.h>".}
  proc c_get_osfhandle(fd: cint): int {.importc: "_get_osfhandle", header: "<io.h>".}

proc isatty*(f: File): bool =
  ## Returns true if `f` is associated with a terminal device.
  when defined(posix) or defined(windows):
    result = c_isatty(c_fileno(f)) != 0'i32
  else:
    result = false

# --- terminal size --------------------------------------------------------

proc envDimension(name: string): int =
  ## A positive decimal from the environment variable `name`, or 0.
  let s = getEnv(name)
  result = 0
  for c in s:
    if c notin {'0'..'9'} or result > 100_000: return 0
    result = result * 10 + (ord(c) - ord('0'))

when defined(posix):
  type
    WinSize = object   # `struct winsize`
      wsRow, wsCol, wsXpixel, wsYpixel: uint16

  when defined(linux):
    const TIOCGWINSZ = 0x5413'u
  elif defined(solaris) or defined(illumos):
    const TIOCGWINSZ = 0x5468'u
  else: # the BSDs and macOS: _IOR('t', 104, struct winsize)
    const TIOCGWINSZ = 0x40087468'u

  proc ioctlSize(fd: cint; ws: var WinSize): bool =
    # `== 0`, not `!= -1`: the libc-free build returns `-errno` from the syscall.
    result = nativeIoctl(fd, TIOCGWINSZ, addr ws) == 0'i32 and ws.wsCol > 0'u16

  proc ttySize(): tuple[w, h: int] =
    ## The window size of the first standard stream that is a terminal, else
    ## of the controlling terminal; `(0, 0)` if there is none.
    result = (0, 0)
    var ws = default(WinSize)
    var found = false
    for fd in [0'i32, 1'i32, 2'i32]:
      if ioctlSize(fd, ws):
        found = true
        break
    if not found:
      var tty: File
      if open(tty, "/dev/tty"):
        found = ioctlSize(getFileHandle(tty), ws)
        close(tty)
    if found: result = (int(ws.wsCol), int(ws.wsRow))

elif defined(windows):
  proc conHandle(f: File): Handle =
    when defined(nimNativeIo): getFileHandle(f)
    else: Handle(c_get_osfhandle(c_fileno(f)))

  proc ttySize(): tuple[w, h: int] =
    result = (0, 0)
    var csbi = default(CONSOLE_SCREEN_BUFFER_INFO)
    for std in [STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE]:
      if isSuccess(getConsoleScreenBufferInfo(getStdHandle(std), addr csbi)):
        result = (int(csbi.srWindow.right) - int(csbi.srWindow.left) + 1,
                  int(csbi.srWindow.bottom) - int(csbi.srWindow.top) + 1)
        break

else:
  proc ttySize(): tuple[w, h: int] = (0, 0)

proc terminalWidth*(): int =
  ## Returns the terminal width in columns: the `COLUMNS` environment variable
  ## if set, else the size of the terminal attached to a standard stream (or,
  ## on POSIX, of the controlling terminal), else 80.
  result = envDimension("COLUMNS")
  if result <= 0: result = ttySize().w
  if result <= 0: result = 80

proc terminalHeight*(): int =
  ## Returns the terminal height in rows: the `LINES` environment variable if
  ## set, else the size of the terminal attached to a standard stream (or, on
  ## POSIX, of the controlling terminal), else 24. (Nim returns 0 instead.)
  result = envDimension("LINES")
  if result <= 0: result = ttySize().h
  if result <= 0: result = 24

proc terminalSize*(): tuple[w, h: int] =
  ## Returns the terminal width and height as a tuple. Internally calls
  ## `terminalWidth` and `terminalHeight`, so the same assumptions apply.
  result = (terminalWidth(), terminalHeight())

# --- cursor and erasing ---------------------------------------------------
# On Windows these go through the console API; failures (e.g. `f` is not a
# console) are ignored, so the procs have the same signature everywhere.

when defined(windows):
  proc screenInfo(f: File; csbi: var CONSOLE_SCREEN_BUFFER_INFO): bool =
    # Text still buffered in `f` must reach the console before the cursor moves.
    flushFile(f)
    result = isSuccess(getConsoleScreenBufferInfo(conHandle(f), addr csbi))

  proc winSetCursorPos(f: File; x, y: int) =
    discard setConsoleCursorPosition(conHandle(f), COORD(x: int16(x), y: int16(y)))

  proc winMoveCursor(f: File; dx, dy: int) =
    var csbi = default(CONSOLE_SCREEN_BUFFER_INFO)
    if screenInfo(f, csbi):
      winSetCursorPos(f, int(csbi.dwCursorPosition.x) + dx,
                      int(csbi.dwCursorPosition.y) + dy)

  proc winFill(f: File; csbi: CONSOLE_SCREEN_BUFFER_INFO; origin: COORD; n: int) =
    let h = conHandle(f)
    var written: DWORD = 0'u32
    discard fillConsoleOutputCharacterW(h, uint16(ord(' ')), DWORD(n), origin, addr written)
    discard fillConsoleOutputAttribute(h, csbi.wAttributes, DWORD(n), origin, addr written)

  proc setCursorVisibility(f: File; visible: bool) =
    flushFile(f)
    let h = conHandle(f)
    var ccsi = default(CONSOLE_CURSOR_INFO)
    if isSuccess(getConsoleCursorInfo(h, addr ccsi)):
      ccsi.bVisible = WINBOOL(if visible: 1'i32 else: 0'i32)
      discard setConsoleCursorInfo(h, addr ccsi)

proc hideCursor*(f: File) =
  ## Hides the cursor.
  when defined(windows): setCursorVisibility(f, false)
  else: write f, "\e[?25l"

proc showCursor*(f: File) =
  ## Shows the cursor.
  when defined(windows): setCursorVisibility(f, true)
  else: write f, "\e[?25h"

proc setCursorPos*(f: File; x, y: int) =
  ## Sets the terminal's cursor to the (x,y) position.
  ## (0,0) is the upper left of the screen.
  when defined(windows):
    flushFile(f)
    winSetCursorPos(f, x, y)
  else:
    write f, stylePrefix & $(y + 1) & ";" & $(x + 1) & "f"

proc setCursorXPos*(f: File; x: int) =
  ## Sets the terminal's cursor to the x position.
  ## The y position is not changed.
  when defined(windows):
    var csbi = default(CONSOLE_SCREEN_BUFFER_INFO)
    if screenInfo(f, csbi):
      winSetCursorPos(f, x, int(csbi.dwCursorPosition.y))
  else:
    write f, stylePrefix & $(x + 1) & "G"

proc cursorUp*(f: File; count = 1) =
  ## Moves the cursor up by `count` rows.
  when defined(windows): winMoveCursor(f, 0, -count)
  else: write f, stylePrefix & $count & "A"

proc cursorDown*(f: File; count = 1) =
  ## Moves the cursor down by `count` rows.
  when defined(windows): winMoveCursor(f, 0, count)
  else: write f, stylePrefix & $count & "B"

proc cursorForward*(f: File; count = 1) =
  ## Moves the cursor forward by `count` columns.
  when defined(windows): winMoveCursor(f, count, 0)
  else: write f, stylePrefix & $count & "C"

proc cursorBackward*(f: File; count = 1) =
  ## Moves the cursor backward by `count` columns.
  when defined(windows): winMoveCursor(f, -count, 0)
  else: write f, stylePrefix & $count & "D"

proc eraseLine*(f: File) =
  ## Erases the entire current line and moves the cursor to its start.
  when defined(windows):
    var csbi = default(CONSOLE_SCREEN_BUFFER_INFO)
    if screenInfo(f, csbi):
      let origin = COORD(x: 0'i16, y: csbi.dwCursorPosition.y)
      winSetCursorPos(f, 0, int(origin.y))
      winFill(f, csbi, origin, int(csbi.dwSize.x))
  else:
    write f, "\e[2K"
    setCursorXPos(f, 0)

proc eraseScreen*(f: File) =
  ## Erases the screen with the background colour and moves the cursor to home.
  when defined(windows):
    var csbi = default(CONSOLE_SCREEN_BUFFER_INFO)
    if screenInfo(f, csbi):
      let origin = COORD(x: 0'i16, y: 0'i16)
      winFill(f, csbi, origin, int(csbi.dwSize.x) * int(csbi.dwSize.y))
      winSetCursorPos(f, 0, 0)
  else:
    write f, "\e[2J"

# --- true color -----------------------------------------------------------

proc isTrueColorSupported*(): bool =
  ## Returns true if a terminal supports true color. Only known after
  ## `enableTrueColors` was called (on the current thread).
  result = trueColorSupported

proc enableTrueColors*() =
  ## Enables true color if the terminal supports it: on POSIX when `COLORTERM`
  ## is `truecolor` or `24bit`; on Windows when ANSICON is active or the console
  ## accepts virtual terminal processing, which this turns on for stdout.
  when defined(windows):
    if getEnv("ANSICON_DEF").len > 0:
      trueColorSupported = true
    else:
      let h = getStdHandle(STD_OUTPUT_HANDLE)
      var mode: DWORD = 0'u32
      trueColorSupported = isSuccess(getConsoleMode(h, addr mode)) and
        isSuccess(setConsoleMode(h, mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING))
  else:
    let ct = toLowerAscii(getEnv("COLORTERM"))
    trueColorSupported = ct == "truecolor" or ct == "24bit"
  trueColorEnabled = trueColorSupported

proc disableTrueColors*() =
  ## Disables true color: `setForegroundColor(f, Color)` and friends write
  ## nothing afterwards. (Unlike Nim, the Windows console mode is left alone,
  ## since the styles of this module are ANSI sequences there too.)
  trueColorEnabled = false

proc setForegroundColor*(f: File; color: Color) =
  ## Sets the terminal's foreground true color, if true color is enabled.
  if trueColorEnabled: write f, ansiForegroundColorCode(color)

proc setBackgroundColor*(f: File; color: Color) =
  ## Sets the terminal's background true color, if true color is enabled.
  if trueColorEnabled: write f, ansiBackgroundColorCode(color)

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
proc styledEchoProcessArg*(f: File; color: Color) =
  ## Sets the foreground or background true color, whichever the last
  ## `fgColor`/`bgColor` argument selected (background by default, as in Nim).
  if trueColorSetsFg: setForegroundColor(f, color)
  else: setBackgroundColor(f, color)
proc styledEchoProcessArg*(f: File; cmd: TerminalCmd) =
  case cmd
  of resetStyle: resetAttributes(f)
  of fgColor: trueColorSetsFg = true
  of bgColor: trueColorSetsFg = false

template styledWrite*(f: File) {.varargs.} =
  ## Similar to `write`, but treating terminal style arguments specially.
  ## When an argument is `Style`, `set[Style]`, `ForegroundColor`,
  ## `BackgroundColor`, `Color` or `TerminalCmd` the corresponding terminal style
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

# Wrappers assuming output to stdout:
template hideCursor*() = hideCursor(stdout)
template showCursor*() = showCursor(stdout)
template setCursorPos*(x, y: int) = setCursorPos(stdout, x, y)
template setCursorXPos*(x: int) = setCursorXPos(stdout, x)
template cursorUp*(count = 1) = cursorUp(stdout, count)
template cursorDown*(count = 1) = cursorDown(stdout, count)
template cursorForward*(count = 1) = cursorForward(stdout, count)
template cursorBackward*(count = 1) = cursorBackward(stdout, count)
template eraseLine*() = eraseLine(stdout)
template eraseScreen*() = eraseScreen(stdout)
template setStyle*(style: set[Style]) = setStyle(stdout, style)
template setForegroundColor*(fg: ForegroundColor; bright = false) =
  setForegroundColor(stdout, fg, bright)
template setBackgroundColor*(bg: BackgroundColor; bright = false) =
  setBackgroundColor(stdout, bg, bright)
template setForegroundColor*(color: Color) = setForegroundColor(stdout, color)
template setBackgroundColor*(color: Color) = setBackgroundColor(stdout, color)

proc resetAttributes*() =
  ## Resets all attributes on stdout.
  resetAttributes(stdout)

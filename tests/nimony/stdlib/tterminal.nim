# std/terminal: the escape sequences of styles, colors, true color, cursor
# movement and erasing (written to a file and read back), and the terminal
# size fallbacks.

import std/[assertions, syncio, os, envvars, colors, terminal]

proc main() {.raises.} =
  assert ansiStyleCode(styleBright) == "\e[1m"
  assert ansiForegroundColorCode(fgRed) == "\e[31m"
  assert ansiForegroundColorCode(fgRed, true) == "\e[91m"
  assert ansiBackgroundColorCode(bgBlue) == "\e[44m"
  assert ansiForegroundColorCode(rgb(240, 236, 228)) == "\e[38;2;240;236;228m"
  assert ansiBackgroundColorCode(colGold) == "\e[48;2;255;215;0m"

  # true color stays off until enabled, like Nim
  disableTrueColors()
  let path = getTempDir() / "nimony_tterminal.txt"
  var f = open(path, fmWrite)
  setForegroundColor(f, colRed)
  when not defined(windows):  # the Windows branches use the console API
    hideCursor(f)
    showCursor(f)
    setCursorPos(f, 4, 2)
    setCursorXPos(f, 0)
    cursorUp(f, 3)
    cursorDown(f)
    cursorForward(f, 2)
    cursorBackward(f, 5)
    eraseLine(f)
    eraseScreen(f)
  styledWrite(f, fgColor, colRed, "x", bgColor, colBlue, styleBright, "y")
  close(f)
  let got = readFile(path)
  when defined(windows):
    assert got == "x\e[1my\e[0m"
  else:
    assert got == "\e[?25l\e[?25h\e[3;5f\e[1G\e[3A\e[1B\e[2C\e[5D\e[2K\e[1G\e[2J" &
                  "x\e[1my\e[0m"

  when not defined(nimNoLibc):  # putEnv needs libc's setenv
    # with true color on, `Color` arguments follow fgColor/bgColor
    let oldColorTerm = getEnv("COLORTERM")
    putEnv("COLORTERM", "truecolor")
    enableTrueColors()
    when not defined(windows): assert isTrueColorSupported()
    if isTrueColorSupported():
      f = open(path, fmWrite)
      setBackgroundColor(f, rgb(1, 2, 3))
      styledWrite(f, fgColor, colRed, "x", bgColor, colBlue, "y")
      close(f)
      assert readFile(path) ==
        "\e[48;2;1;2;3m\e[38;2;255;0;0mx\e[48;2;0;0;255my\e[0m"
    disableTrueColors()
    if oldColorTerm.len > 0: putEnv("COLORTERM", oldColorTerm)
    else: delEnv("COLORTERM")

  # size: COLUMNS/LINES win; otherwise a terminal or the 80x24 fallback
  when not defined(nimNoLibc):
    let oldCols = getEnv("COLUMNS")
    let oldLines = getEnv("LINES")
    putEnv("COLUMNS", "77")
    putEnv("LINES", "33")
    assert terminalSize() == (77, 33)
    delEnv("COLUMNS")
    delEnv("LINES")
  let (w, h) = terminalSize()
  assert w > 0 and h > 0
  assert terminalWidth() == w and terminalHeight() == h
  when not defined(nimNoLibc):
    if oldCols.len > 0: putEnv("COLUMNS", oldCols)
    if oldLines.len > 0: putEnv("LINES", oldLines)
  echo "terminal ok"

try:
  main()
except:
  quit "tterminal: unexpected error"

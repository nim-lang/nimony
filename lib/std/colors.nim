#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Terminal string colouring and styling, with the fluent, chainable feel of
## the npm `colors` package: every colour and style is a `func (s: string):
## string` that wraps `s` in the matching ANSI SGR escape sequence, so they
## compose through method-call syntax.
##
##   ```nim
##   echo "this is red and bold".red.bold
##   echo "warning".yellow
##   echo "selected".black.bgWhite
##   echo "brand".fg(rgb(255, 128, 0))     # truecolor from an RGB value
##   ```
##
## `"x".red.bold` is simply `bold(red("x"))`; because each styler emits its own
## reset code, wrappers nest correctly. Escape codes are always emitted — pass a
## result through `stripAnsi` (or guard the call) to get plain text.
##
## **Naming.** This module is `std/colors`; the RGB `Color` *value* type lives in
## `std/rgb`, which this module imports for its truecolor bridge (`fg`/`bg`).
## That is a deliberate swap from Nim 2, where `std/colors` was the value type —
## see the pull request for the argument. It is also a small homage to the npm
## `colors` package, whose name every other ecosystem reads as "style my
## terminal output".

import std/rgb
export rgb

func wrap(s: string; on, off: int): string {.inline.} =
  "\e[" & $on & "m" & s & "\e[" & $off & "m"

# --- foreground colours (reset with 39) ---

func black*(s: string): string = wrap(s, 30, 39)
  ## Colours `s` black.
func red*(s: string): string = wrap(s, 31, 39)
  ## Colours `s` red.
func green*(s: string): string = wrap(s, 32, 39)
  ## Colours `s` green.
func yellow*(s: string): string = wrap(s, 33, 39)
  ## Colours `s` yellow.
func blue*(s: string): string = wrap(s, 34, 39)
  ## Colours `s` blue.
func magenta*(s: string): string = wrap(s, 35, 39)
  ## Colours `s` magenta.
func cyan*(s: string): string = wrap(s, 36, 39)
  ## Colours `s` cyan.
func white*(s: string): string = wrap(s, 37, 39)
  ## Colours `s` white.
func gray*(s: string): string = wrap(s, 90, 39)
  ## Colours `s` gray (bright black).
func grey*(s: string): string = wrap(s, 90, 39)
  ## Alias for `gray`.

# --- bright foreground colours ---

func brightRed*(s: string): string = wrap(s, 91, 39)
func brightGreen*(s: string): string = wrap(s, 92, 39)
func brightYellow*(s: string): string = wrap(s, 93, 39)
func brightBlue*(s: string): string = wrap(s, 94, 39)
func brightMagenta*(s: string): string = wrap(s, 95, 39)
func brightCyan*(s: string): string = wrap(s, 96, 39)
func brightWhite*(s: string): string = wrap(s, 97, 39)

# --- background colours (reset with 49) ---

func bgBlack*(s: string): string = wrap(s, 40, 49)
func bgRed*(s: string): string = wrap(s, 41, 49)
func bgGreen*(s: string): string = wrap(s, 42, 49)
func bgYellow*(s: string): string = wrap(s, 43, 49)
func bgBlue*(s: string): string = wrap(s, 44, 49)
func bgMagenta*(s: string): string = wrap(s, 45, 49)
func bgCyan*(s: string): string = wrap(s, 46, 49)
func bgWhite*(s: string): string = wrap(s, 47, 49)

# --- styles (each with its own reset) ---

func bold*(s: string): string = wrap(s, 1, 22)
  ## Renders `s` bold.
func dim*(s: string): string = wrap(s, 2, 22)
  ## Renders `s` dim (faint).
func italic*(s: string): string = wrap(s, 3, 23)
  ## Renders `s` italic.
func underline*(s: string): string = wrap(s, 4, 24)
  ## Underlines `s`.
func inverse*(s: string): string = wrap(s, 7, 27)
  ## Swaps foreground and background of `s`.
func hidden*(s: string): string = wrap(s, 8, 28)
  ## Renders `s` hidden (not displayed).
func strikethrough*(s: string): string = wrap(s, 9, 29)
  ## Renders `s` with a line through it.

# --- official ANSI generation + truecolor bridge to std/rgb ---

const ansiReset* = "\e[0m"
  ## The SGR sequence that resets all colours and styles.

func ansiSgr*(code: int): string {.inline.} =
  ## The raw SGR escape sequence for `code`, e.g. `ansiSgr(31) == "\e[31m"`.
  "\e[" & $code & "m"

func ansiFg*(c: Color): string =
  ## The 24-bit truecolor foreground SGR sequence for the RGB value `c`.
  let (r, g, b) = extractRGB(c)
  "\e[38;2;" & $r & ";" & $g & ";" & $b & "m"

func ansiBg*(c: Color): string =
  ## The 24-bit truecolor background SGR sequence for the RGB value `c`.
  let (r, g, b) = extractRGB(c)
  "\e[48;2;" & $r & ";" & $g & ";" & $b & "m"

func fg*(s: string; c: Color): string =
  ## Renders `s` in the truecolor foreground `c` (an `std/rgb` `Color`).
  ansiFg(c) & s & "\e[39m"

func bg*(s: string; c: Color): string =
  ## Renders `s` on the truecolor background `c` (an `std/rgb` `Color`).
  ansiBg(c) & s & "\e[49m"

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

# --- homage: npm `colors` novelty ---

func rainbow*(s: string): string =
  ## Paints each non-space character in the next terminal colour, cycling
  ## red -> yellow -> green -> cyan -> blue -> magenta. A wink at npm `colors`
  ## (the sabotage not included).
  const cycle = [31, 33, 32, 36, 34, 35]
  result = ""
  var k = 0
  var i = 0
  let n = s.len
  while i < n:
    let ch = s[i]
    if ch == ' ':
      result.add ch
    else:
      result.add "\e["
      result.add $cycle[k mod 6]
      result.add "m"
      result.add ch
      result.add "\e[39m"
      inc k
    inc i

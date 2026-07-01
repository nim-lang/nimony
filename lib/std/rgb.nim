#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements an RGB color *value* type — the 24-bit `Color`
## (`0xRRGGBB`) plus construction, decomposition, blending and named constants.
##
## It is the value-oriented half of "colors": pure data and colour math, with no
## dependency on any output device. Terminal rendering of these values (ANSI
## truecolor) lives in `std/colors`, which imports this module — so a `Color`
## can be turned into styled terminal text with `"text".fg(rgb(255, 128, 0))`.
##
## (In Nim 2 this value type was itself named `std/colors`; see that module's
## documentation for the rationale behind the split.)

type
  Color* = distinct int
    ## A 24-bit RGB color, stored as `0xRRGGBB`.

func `==`*(a, b: Color): bool {.inline.} =
  ## Two colors are equal when their packed RGB values are equal.
  int(a) == int(b)

func rgb*(r, g, b: int): Color {.inline.} =
  ## Constructs a `Color` from its components (each masked to `0..255`).
  runnableExamples:
    doAssert rgb(255, 128, 0) == Color(0xFF8000)
  Color(((r and 0xFF) shl 16) or ((g and 0xFF) shl 8) or (b and 0xFF))

func extractRGB*(c: Color): tuple[r, g, b: int] {.inline.} =
  ## Splits `c` into its red, green and blue components.
  let v = int(c)
  result = (r: (v shr 16) and 0xFF, g: (v shr 8) and 0xFF, b: v and 0xFF)

func `$`*(c: Color): string =
  ## Renders `c` as a lowercase hex string, e.g. `"#ff8000"`.
  const hex = "0123456789abcdef"
  let v = int(c)
  result = "#"
  for shift in [20, 16, 12, 8, 4, 0]:
    result.add hex[(v shr shift) and 0xF]

func mix*(a, b: Color): Color =
  ## Returns the component-wise average of `a` and `b`.
  let (ar, ag, ab) = extractRGB(a)
  let (br, bg, bb) = extractRGB(b)
  rgb((ar + br) div 2, (ag + bg) div 2, (ab + bb) div 2)

const
  colBlack*   = Color(0x000000)
  colWhite*   = Color(0xFFFFFF)
  colRed*     = Color(0xFF0000)
  colLime*    = Color(0x00FF00)
  colGreen*   = Color(0x008000)
  colBlue*    = Color(0x0000FF)
  colYellow*  = Color(0xFFFF00)
  colCyan*    = Color(0x00FFFF)
  colMagenta* = Color(0xFF00FF)
  colSilver*  = Color(0xC0C0C0)
  colGray*    = Color(0x808080)
  colMaroon*  = Color(0x800000)
  colOlive*   = Color(0x808000)
  colNavy*    = Color(0x000080)
  colPurple*  = Color(0x800080)
  colTeal*    = Color(0x008080)
  colOrange*  = Color(0xFFA500)

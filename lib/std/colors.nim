#
#
#            Nim's Runtime Library
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements color handling for Nimony,
## namely color mixing and parsing the CSS color names.

import std/strutils

type
  Color* = distinct int ## A color stored as RGB, e.g. `0xff00cc`.

proc `==`*(a, b: Color): bool {.borrow.}
  ## Compares two colors.

proc red(a: Color): int {.inline.} = a.int shr 16 and 0xff
proc green(a: Color): int {.inline.} = a.int shr 8 and 0xff
proc blue(a: Color): int {.inline.} = a.int and 0xff

template rawRGB(r, g, b: int): Color =
  Color(r shl 16 or g shl 8 or b)

proc clampComponent(x: int): int {.inline.} =
  ## Saturates a color component to `0..255`.
  if x < 0: 0
  elif x > 255: 255
  else: x

proc `+`*(a, b: Color): Color =
  ## Adds two colors.
  ##
  ## This uses saturated arithmetic, so that each color
  ## component cannot overflow (255 is used as a maximum).
  runnableExamples:
    assert Color(0xaa_00_ff) + Color(0x11_cc_cc) == Color(0xbb_cc_ff)
  rawRGB(clampComponent(a.red + b.red), clampComponent(a.green + b.green),
         clampComponent(a.blue + b.blue))

proc `-`*(a, b: Color): Color =
  ## Subtracts two colors.
  ##
  ## This uses saturated arithmetic, so that each color
  ## component cannot underflow (0 is used as a minimum).
  runnableExamples:
    assert Color(0xff_33_ff) - Color(0x11_ff_cc) == Color(0xee_00_33)
  rawRGB(clampComponent(a.red - b.red), clampComponent(a.green - b.green),
         clampComponent(a.blue - b.blue))

proc extractRGB*(a: Color): tuple[r, g, b: range[0..255]] =
  ## Extracts the red/green/blue components of the color `a`.
  runnableExamples:
    let c = extractRGB(Color(0x00_ff_cc))
    assert c.r == 0 and c.g == 255 and c.b == 204
  result = (r: range[0..255](a.red), g: range[0..255](a.green),
            b: range[0..255](a.blue))

proc mixColors(a, b: Color; fn: proc (x, y: int): int): Color =
  ## The body of `mix`: `fn` must be a proc value, not an expression.
  rawRGB(clampComponent(fn(a.red, b.red)), clampComponent(fn(a.green, b.green)),
         clampComponent(fn(a.blue, b.blue)))

proc scaleComponent(x: int; f: float): int =
  # Rounds half away from zero (like Nim's `toInt`) and saturates to 0..255.
  let y = float(x) * f
  clampComponent(if y >= 0.0: int(y + 0.5) else: -int(0.5 - y))

proc intensity*(a: Color; f: float): Color =
  ## Returns `a` with intensity `f`. `f` should be a float from 0.0 (completely
  ## dark) to 1.0 (full color intensity).
  runnableExamples:
    assert Color(0xff_00_ff).intensity(0.5) == Color(0x80_00_80)
    assert Color(0x00_42_cc).intensity(0.5) == Color(0x00_21_66)
  rawRGB(scaleComponent(a.red, f), scaleComponent(a.green, f),
         scaleComponent(a.blue, f))

template mix*(a, b: Color; fn: untyped): Color {.untyped.} =
  ## Uses `fn` to mix the colors `a` and `b`.
  ##
  ## `fn` is invoked for each component R, G, and B.
  ## If `fn`'s result is not in the `range[0..255]`,
  ## it will be saturated to be so.
  mixColors(a, b, fn)

const
  colAliceBlue* = Color(0xF0F8FF)
  colAntiqueWhite* = Color(0xFAEBD7)
  colAqua* = Color(0x00FFFF)
  colAquamarine* = Color(0x7FFFD4)
  colAzure* = Color(0xF0FFFF)
  colBeige* = Color(0xF5F5DC)
  colBisque* = Color(0xFFE4C4)
  colBlack* = Color(0x000000)
  colBlanchedAlmond* = Color(0xFFEBCD)
  colBlue* = Color(0x0000FF)
  colBlueViolet* = Color(0x8A2BE2)
  colBrown* = Color(0xA52A2A)
  colBurlyWood* = Color(0xDEB887)
  colCadetBlue* = Color(0x5F9EA0)
  colChartreuse* = Color(0x7FFF00)
  colChocolate* = Color(0xD2691E)
  colCoral* = Color(0xFF7F50)
  colCornflowerBlue* = Color(0x6495ED)
  colCornsilk* = Color(0xFFF8DC)
  colCrimson* = Color(0xDC143C)
  colCyan* = Color(0x00FFFF)
  colDarkBlue* = Color(0x00008B)
  colDarkCyan* = Color(0x008B8B)
  colDarkGoldenRod* = Color(0xB8860B)
  colDarkGray* = Color(0xA9A9A9)
  colDarkGreen* = Color(0x006400)
  colDarkGrey* = Color(0xA9A9A9)
  colDarkKhaki* = Color(0xBDB76B)
  colDarkMagenta* = Color(0x8B008B)
  colDarkOliveGreen* = Color(0x556B2F)
  colDarkorange* = Color(0xFF8C00)
  colDarkOrchid* = Color(0x9932CC)
  colDarkRed* = Color(0x8B0000)
  colDarkSalmon* = Color(0xE9967A)
  colDarkSeaGreen* = Color(0x8FBC8F)
  colDarkSlateBlue* = Color(0x483D8B)
  colDarkSlateGray* = Color(0x2F4F4F)
  colDarkSlateGrey* = Color(0x2F4F4F)
  colDarkTurquoise* = Color(0x00CED1)
  colDarkViolet* = Color(0x9400D3)
  colDeepPink* = Color(0xFF1493)
  colDeepSkyBlue* = Color(0x00BFFF)
  colDimGray* = Color(0x696969)
  colDimGrey* = Color(0x696969)
  colDodgerBlue* = Color(0x1E90FF)
  colFireBrick* = Color(0xB22222)
  colFloralWhite* = Color(0xFFFAF0)
  colForestGreen* = Color(0x228B22)
  colFuchsia* = Color(0xFF00FF)
  colGainsboro* = Color(0xDCDCDC)
  colGhostWhite* = Color(0xF8F8FF)
  colGold* = Color(0xFFD700)
  colGoldenRod* = Color(0xDAA520)
  colGray* = Color(0x808080)
  colGreen* = Color(0x008000)
  colGreenYellow* = Color(0xADFF2F)
  colGrey* = Color(0x808080)
  colHoneyDew* = Color(0xF0FFF0)
  colHotPink* = Color(0xFF69B4)
  colIndianRed* = Color(0xCD5C5C)
  colIndigo* = Color(0x4B0082)
  colIvory* = Color(0xFFFFF0)
  colKhaki* = Color(0xF0E68C)
  colLavender* = Color(0xE6E6FA)
  colLavenderBlush* = Color(0xFFF0F5)
  colLawnGreen* = Color(0x7CFC00)
  colLemonChiffon* = Color(0xFFFACD)
  colLightBlue* = Color(0xADD8E6)
  colLightCoral* = Color(0xF08080)
  colLightCyan* = Color(0xE0FFFF)
  colLightGoldenRodYellow* = Color(0xFAFAD2)
  colLightGray* = Color(0xD3D3D3)
  colLightGreen* = Color(0x90EE90)
  colLightGrey* = Color(0xD3D3D3)
  colLightPink* = Color(0xFFB6C1)
  colLightSalmon* = Color(0xFFA07A)
  colLightSeaGreen* = Color(0x20B2AA)
  colLightSkyBlue* = Color(0x87CEFA)
  colLightSlateGray* = Color(0x778899)
  colLightSlateGrey* = Color(0x778899)
  colLightSteelBlue* = Color(0xB0C4DE)
  colLightYellow* = Color(0xFFFFE0)
  colLime* = Color(0x00FF00)
  colLimeGreen* = Color(0x32CD32)
  colLinen* = Color(0xFAF0E6)
  colMagenta* = Color(0xFF00FF)
  colMaroon* = Color(0x800000)
  colMediumAquaMarine* = Color(0x66CDAA)
  colMediumBlue* = Color(0x0000CD)
  colMediumOrchid* = Color(0xBA55D3)
  colMediumPurple* = Color(0x9370DB)
  colMediumSeaGreen* = Color(0x3CB371)
  colMediumSlateBlue* = Color(0x7B68EE)
  colMediumSpringGreen* = Color(0x00FA9A)
  colMediumTurquoise* = Color(0x48D1CC)
  colMediumVioletRed* = Color(0xC71585)
  colMidnightBlue* = Color(0x191970)
  colMintCream* = Color(0xF5FFFA)
  colMistyRose* = Color(0xFFE4E1)
  colMoccasin* = Color(0xFFE4B5)
  colNavajoWhite* = Color(0xFFDEAD)
  colNavy* = Color(0x000080)
  colOldLace* = Color(0xFDF5E6)
  colOlive* = Color(0x808000)
  colOliveDrab* = Color(0x6B8E23)
  colOrange* = Color(0xFFA500)
  colOrangeRed* = Color(0xFF4500)
  colOrchid* = Color(0xDA70D6)
  colPaleGoldenRod* = Color(0xEEE8AA)
  colPaleGreen* = Color(0x98FB98)
  colPaleTurquoise* = Color(0xAFEEEE)
  colPaleVioletRed* = Color(0xDB7093)
  colPapayaWhip* = Color(0xFFEFD5)
  colPeachPuff* = Color(0xFFDAB9)
  colPeru* = Color(0xCD853F)
  colPink* = Color(0xFFC0CB)
  colPlum* = Color(0xDDA0DD)
  colPowderBlue* = Color(0xB0E0E6)
  colPurple* = Color(0x800080)
  colRebeccaPurple* = Color(0x663399)
  colRed* = Color(0xFF0000)
  colRosyBrown* = Color(0xBC8F8F)
  colRoyalBlue* = Color(0x4169E1)
  colSaddleBrown* = Color(0x8B4513)
  colSalmon* = Color(0xFA8072)
  colSandyBrown* = Color(0xF4A460)
  colSeaGreen* = Color(0x2E8B57)
  colSeaShell* = Color(0xFFF5EE)
  colSienna* = Color(0xA0522D)
  colSilver* = Color(0xC0C0C0)
  colSkyBlue* = Color(0x87CEEB)
  colSlateBlue* = Color(0x6A5ACD)
  colSlateGray* = Color(0x708090)
  colSlateGrey* = Color(0x708090)
  colSnow* = Color(0xFFFAFA)
  colSpringGreen* = Color(0x00FF7F)
  colSteelBlue* = Color(0x4682B4)
  colTan* = Color(0xD2B48C)
  colTeal* = Color(0x008080)
  colThistle* = Color(0xD8BFD8)
  colTomato* = Color(0xFF6347)
  colTurquoise* = Color(0x40E0D0)
  colViolet* = Color(0xEE82EE)
  colWheat* = Color(0xF5DEB3)
  colWhite* = Color(0xFFFFFF)
  colWhiteSmoke* = Color(0xF5F5F5)
  colYellow* = Color(0xFFFF00)
  colYellowGreen* = Color(0x9ACD32)

  colorNames = [
    ("aliceblue", colAliceBlue),
    ("antiquewhite", colAntiqueWhite),
    ("aqua", colAqua),
    ("aquamarine", colAquamarine),
    ("azure", colAzure),
    ("beige", colBeige),
    ("bisque", colBisque),
    ("black", colBlack),
    ("blanchedalmond", colBlanchedAlmond),
    ("blue", colBlue),
    ("blueviolet", colBlueViolet),
    ("brown", colBrown),
    ("burlywood", colBurlyWood),
    ("cadetblue", colCadetBlue),
    ("chartreuse", colChartreuse),
    ("chocolate", colChocolate),
    ("coral", colCoral),
    ("cornflowerblue", colCornflowerBlue),
    ("cornsilk", colCornsilk),
    ("crimson", colCrimson),
    ("cyan", colCyan),
    ("darkblue", colDarkBlue),
    ("darkcyan", colDarkCyan),
    ("darkgoldenrod", colDarkGoldenRod),
    ("darkgray", colDarkGray),
    ("darkgreen", colDarkGreen),
    ("darkgrey", colDarkGrey),
    ("darkkhaki", colDarkKhaki),
    ("darkmagenta", colDarkMagenta),
    ("darkolivegreen", colDarkOliveGreen),
    ("darkorange", colDarkorange),
    ("darkorchid", colDarkOrchid),
    ("darkred", colDarkRed),
    ("darksalmon", colDarkSalmon),
    ("darkseagreen", colDarkSeaGreen),
    ("darkslateblue", colDarkSlateBlue),
    ("darkslategray", colDarkSlateGray),
    ("darkslategrey", colDarkSlateGrey),
    ("darkturquoise", colDarkTurquoise),
    ("darkviolet", colDarkViolet),
    ("deeppink", colDeepPink),
    ("deepskyblue", colDeepSkyBlue),
    ("dimgray", colDimGray),
    ("dimgrey", colDimGrey),
    ("dodgerblue", colDodgerBlue),
    ("firebrick", colFireBrick),
    ("floralwhite", colFloralWhite),
    ("forestgreen", colForestGreen),
    ("fuchsia", colFuchsia),
    ("gainsboro", colGainsboro),
    ("ghostwhite", colGhostWhite),
    ("gold", colGold),
    ("goldenrod", colGoldenRod),
    ("gray", colGray),
    ("green", colGreen),
    ("greenyellow", colGreenYellow),
    ("grey", colGrey),
    ("honeydew", colHoneyDew),
    ("hotpink", colHotPink),
    ("indianred", colIndianRed),
    ("indigo", colIndigo),
    ("ivory", colIvory),
    ("khaki", colKhaki),
    ("lavender", colLavender),
    ("lavenderblush", colLavenderBlush),
    ("lawngreen", colLawnGreen),
    ("lemonchiffon", colLemonChiffon),
    ("lightblue", colLightBlue),
    ("lightcoral", colLightCoral),
    ("lightcyan", colLightCyan),
    ("lightgoldenrodyellow", colLightGoldenRodYellow),
    ("lightgray", colLightGray),
    ("lightgreen", colLightGreen),
    ("lightgrey", colLightGrey),
    ("lightpink", colLightPink),
    ("lightsalmon", colLightSalmon),
    ("lightseagreen", colLightSeaGreen),
    ("lightskyblue", colLightSkyBlue),
    ("lightslategray", colLightSlateGray),
    ("lightslategrey", colLightSlateGrey),
    ("lightsteelblue", colLightSteelBlue),
    ("lightyellow", colLightYellow),
    ("lime", colLime),
    ("limegreen", colLimeGreen),
    ("linen", colLinen),
    ("magenta", colMagenta),
    ("maroon", colMaroon),
    ("mediumaquamarine", colMediumAquaMarine),
    ("mediumblue", colMediumBlue),
    ("mediumorchid", colMediumOrchid),
    ("mediumpurple", colMediumPurple),
    ("mediumseagreen", colMediumSeaGreen),
    ("mediumslateblue", colMediumSlateBlue),
    ("mediumspringgreen", colMediumSpringGreen),
    ("mediumturquoise", colMediumTurquoise),
    ("mediumvioletred", colMediumVioletRed),
    ("midnightblue", colMidnightBlue),
    ("mintcream", colMintCream),
    ("mistyrose", colMistyRose),
    ("moccasin", colMoccasin),
    ("navajowhite", colNavajoWhite),
    ("navy", colNavy),
    ("oldlace", colOldLace),
    ("olive", colOlive),
    ("olivedrab", colOliveDrab),
    ("orange", colOrange),
    ("orangered", colOrangeRed),
    ("orchid", colOrchid),
    ("palegoldenrod", colPaleGoldenRod),
    ("palegreen", colPaleGreen),
    ("paleturquoise", colPaleTurquoise),
    ("palevioletred", colPaleVioletRed),
    ("papayawhip", colPapayaWhip),
    ("peachpuff", colPeachPuff),
    ("peru", colPeru),
    ("pink", colPink),
    ("plum", colPlum),
    ("powderblue", colPowderBlue),
    ("purple", colPurple),
    ("rebeccapurple", colRebeccaPurple),
    ("red", colRed),
    ("rosybrown", colRosyBrown),
    ("royalblue", colRoyalBlue),
    ("saddlebrown", colSaddleBrown),
    ("salmon", colSalmon),
    ("sandybrown", colSandyBrown),
    ("seagreen", colSeaGreen),
    ("seashell", colSeaShell),
    ("sienna", colSienna),
    ("silver", colSilver),
    ("skyblue", colSkyBlue),
    ("slateblue", colSlateBlue),
    ("slategray", colSlateGray),
    ("slategrey", colSlateGrey),
    ("snow", colSnow),
    ("springgreen", colSpringGreen),
    ("steelblue", colSteelBlue),
    ("tan", colTan),
    ("teal", colTeal),
    ("thistle", colThistle),
    ("tomato", colTomato),
    ("turquoise", colTurquoise),
    ("violet", colViolet),
    ("wheat", colWheat),
    ("white", colWhite),
    ("whitesmoke", colWhiteSmoke),
    ("yellow", colYellow),
    ("yellowgreen", colYellowGreen)
  ] ## sorted by name for the binary search in `lookupColor`

proc `$`*(c: Color): string =
  ## Converts a color into its textual representation.
  runnableExamples:
    assert $colFuchsia == "#FF00FF"
  result = "#" & toHex(BiggestInt(int(c)), 6)

proc lookupColor(name: string): int =
  ## Index of `name` (case insensitive) in `colorNames`, or -1.
  var lo = 0
  var hi = colorNames.len - 1
  result = -1
  while lo <= hi:
    let mid = (lo + hi) div 2
    let c = cmpIgnoreCase(colorNames[mid][0], name)
    if c == 0:
      result = mid
      break
    elif c < 0:
      lo = mid + 1
    else:
      hi = mid - 1

proc isHexColor(name: string): bool =
  ## `#` followed by one or more hexadecimal digits.
  result = name.len > 1 and name[0] == '#'
  if result:
    for i in 1 ..< name.len:
      if name[i] notin HexDigits: return false

proc parseColor*(name: string): Color {.raises.} =
  ## Parses `name` to a color value.
  ##
  ## If no valid color could be parsed `ValueError` is raised.
  ## Case insensitive.
  runnableExamples:
    assert parseColor("silver") == Color(0xc0_c0_c0)
    assert parseColor("#0179fc") == Color(0x01_79_fc)
  if name.len > 0 and name[0] == '#':
    if not isHexColor(name): raise ValueError
    var v = 0
    for i in 1 ..< name.len:
      let c = name[i]
      let d = if c in {'0'..'9'}: ord(c) - ord('0')
              elif c in {'a'..'f'}: ord(c) - ord('a') + 10
              else: ord(c) - ord('A') + 10
      v = (v shl 4) or d
    result = Color(v)
  else:
    let idx = lookupColor(name)
    if idx < 0: raise ValueError
    result = colorNames[idx][1]

proc isColor*(name: string): bool =
  ## Returns true if `name` is a known color name or a hexadecimal color
  ## prefixed with `#`. Case insensitive.
  runnableExamples:
    assert "silver".isColor
    assert "#0179fc".isColor
    assert not "#zzmmtt".isColor
  if name.len == 0: result = false
  elif name[0] == '#': result = isHexColor(name)
  else: result = lookupColor(name) >= 0

proc rgb*(r, g, b: range[0..255]): Color =
  ## Constructs a color from RGB values.
  runnableExamples:
    assert rgb(0, 255, 128) == Color(0x00_ff_80)
  result = rawRGB(r, g, b)

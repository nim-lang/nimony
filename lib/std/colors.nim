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

import std/[strutils, math]

type Color* = distinct uint32 ## A color stored as RGBA, e.g. `0xff00ccff`.

proc `==`*(a, b: Color): bool {.borrow.}
  ## Compares two colors.

proc r*(a: Color): uint8 {.inline.} = (uint32(a) shr 24).uint8

proc g*(a: Color): uint8 {.inline.} = (uint32(a) shr 16).uint8

proc b*(a: Color): uint8 {.inline.} = (uint32(a) shr 8).uint8

proc a*(a: Color): uint8 {.inline.} = a.uint8

proc `r=`*(a: var Color, value: uint8) {.inline.} =
  a = Color((uint32(a) and 0x00ffffff'u32) or (value.uint32 shl 24))

proc `g=`*(a: var Color, value: uint8) {.inline.} =
  a = Color((uint32(a) and 0xff00ffff'u32) or (value.uint32 shl 16))

proc `b=`*(a: var Color, value: uint8) {.inline.} =
  a = Color((uint32(a) and 0xffff00ff'u32) or (value.uint32 shl 8))

proc `a=`*(a: var Color, value: uint8) {.inline.} =
  a = Color((uint32(a) and 0xffffff00'u32) or value.uint32)

template rawRgba(r, g, b, a: uint8): Color =
  Color(r.uint32 shl 24 or g.uint32 shl 16 or b.uint32 shl 8 or a.uint32)

proc clampComponent(x: int): uint8 {.inline.} =
  ## Saturates a color component to `0..255`.
  if x < 0: 0'u8
  elif x > 255: 255'u8
  else: x.uint8

proc `+`*(a, b: Color): Color =
  ## Adds two colors.
  ##
  ## This uses saturated arithmetic, so that each color
  ## component cannot overflow (255 is used as a maximum).
  runnableExamples:
    assert Color(0xaa_00_ff) + Color(0x11_cc_cc) == Color(0xbb_cc_ff)
  rawRgba(clampComponent(a.r.int + b.r.int), clampComponent(a.g.int + b.g.int),
         clampComponent(a.b.int + b.b.int), clampComponent(a.a.int + b.a.int))

proc `-`*(a, b: Color): Color =
  ## Subtracts two colors.
  ##
  ## This uses saturated arithmetic, so that each color
  ## component cannot underflow (0 is used as a minimum).
  runnableExamples:
    assert Color(0xff_33_ff) - Color(0x11_ff_cc) == Color(0xee_00_33)
  rawRgba(clampComponent(a.r.int - b.r.int), clampComponent(a.g.int - b.g.int),
         clampComponent(a.b.int - b.b.int), clampComponent(a.a.int - b.a.int))

proc extractRgba*(a: Color): tuple[r, g, b, a: uint8] =
  ## Extracts the red/green/blue/alpha components of the color `a`.
  runnableExamples:
    let a = extractRgba(Color(0x00_ff_cc_ff'u32))
    assert a.r == 0 and a.g == 255 and a.b == 204 and a.a == 255
  result = (r: a.r, g: a.g, b: a.b, a: a.a)

proc extractRgb*(a: Color): tuple[r, g, b: uint8] =
  ## Extracts the red/green/blue components of the color `a`.
  runnableExamples:
    let a = extractRgb(Color(0x00_ff_cc_ff'u32))
    assert a.r == 0 and a.g == 255 and a.b == 204
  result = (r: a.r, g: a.g, b: a.b)

proc mixColors(a, b: Color; fn: proc (x, y: int): int): Color =
  ## The body of `mix`: `fn` must be a proc value, not an expression.
  rawRgba(clampComponent(fn(a.r.int, b.r.int)), clampComponent(fn(a.g.int, b.g.int)),
         clampComponent(fn(a.b.int, b.b.int)), clampComponent(fn(a.a.int, b.a.int)))

proc scaleComponent(x: uint8; f: float): uint8 =
  # Rounds half away from zero (like Nim's `toInt`) and saturates to 0..255.
  let y = float(x) * f
  clampComponent(if y >= 0.0: int(y + 0.5) else: -int(0.5 - y))

proc intensity*(a: Color; f: float): Color =
  ## Returns `a` with intensity `f`. `f` should be a float from 0.0 (completely
  ## dark) to 1.0 (full color intensity).
  runnableExamples:
    assert Color(0xff_00_ff).intensity(0.5) == Color(0x80_00_80)
    assert Color(0x00_42_cc).intensity(0.5) == Color(0x00_21_66)
  rawRgba(scaleComponent(a.r, f), scaleComponent(a.g, f),
         scaleComponent(a.b, f), a.a)

template mix*(a, b: Color; fn: untyped): Color {.untyped.} =
  ## Uses `fn` to mix the colors `a` and `b`.
  ##
  ## `fn` is invoked for each component R, G, B, and A.
  ## If `fn`'s result is not in the `range[0..255]`,
  ## it will be saturated to be so.
  mixColors(a, b, fn)

const
  colAliceBlue* = Color(0xF0F8FFFF)
  colAntiqueWhite* = Color(0xFAEBD7FF)
  colAqua* = Color(0x00FFFFFF)
  colAquamarine* = Color(0x7FFFD4FF)
  colAzure* = Color(0xF0FFFFFF)
  colBeige* = Color(0xF5F5DCFF)
  colBisque* = Color(0xFFE4C4FF)
  colBlack* = Color(0x000000FF)
  colBlanchedAlmond* = Color(0xFFEBCDFF)
  colBlue* = Color(0x0000FFFF)
  colBlueViolet* = Color(0x8A2BE2FF)
  colBrown* = Color(0xA52A2AFF)
  colBurlyWood* = Color(0xDEB887FF)
  colCadetBlue* = Color(0x5F9EA0FF)
  colChartreuse* = Color(0x7FFF00FF)
  colChocolate* = Color(0xD2691EFF)
  colCoral* = Color(0xFF7F50FF)
  colCornflowerBlue* = Color(0x6495EDFF)
  colCornsilk* = Color(0xFFF8DCFF)
  colCrimson* = Color(0xDC143CFF)
  colCyan* = Color(0x00FFFFFF)
  colDarkBlue* = Color(0x00008BFF)
  colDarkCyan* = Color(0x008B8BFF)
  colDarkGoldenRod* = Color(0xB8860BFF)
  colDarkGray* = Color(0xA9A9A9FF)
  colDarkGreen* = Color(0x006400FF)
  colDarkGrey* = Color(0xA9A9A9FF)
  colDarkKhaki* = Color(0xBDB76BFF)
  colDarkMagenta* = Color(0x8B008BFF)
  colDarkOliveGreen* = Color(0x556B2FFF)
  colDarkorange* = Color(0xFF8C00FF)
  colDarkOrchid* = Color(0x9932CCFF)
  colDarkRed* = Color(0x8B0000FF)
  colDarkSalmon* = Color(0xE9967AFF)
  colDarkSeaGreen* = Color(0x8FBC8FFF)
  colDarkSlateBlue* = Color(0x483D8BFF)
  colDarkSlateGray* = Color(0x2F4F4FFF)
  colDarkSlateGrey* = Color(0x2F4F4FFF)
  colDarkTurquoise* = Color(0x00CED1FF)
  colDarkViolet* = Color(0x9400D3FF)
  colDeepPink* = Color(0xFF1493FF)
  colDeepSkyBlue* = Color(0x00BFFFFF)
  colDimGray* = Color(0x696969FF)
  colDimGrey* = Color(0x696969FF)
  colDodgerBlue* = Color(0x1E90FFFF)
  colFireBrick* = Color(0xB22222FF)
  colFloralWhite* = Color(0xFFFAF0FF)
  colForestGreen* = Color(0x228B22FF)
  colFuchsia* = Color(0xFF00FFFF)
  colGainsboro* = Color(0xDCDCDCFF)
  colGhostWhite* = Color(0xF8F8FFFF)
  colGold* = Color(0xFFD700FF)
  colGoldenRod* = Color(0xDAA520FF)
  colGray* = Color(0x808080FF)
  colGreen* = Color(0x008000FF)
  colGreenYellow* = Color(0xADFF2FFF)
  colGrey* = Color(0x808080FF)
  colHoneyDew* = Color(0xF0FFF0FF)
  colHotPink* = Color(0xFF69B4FF)
  colIndianRed* = Color(0xCD5C5CFF)
  colIndigo* = Color(0x4B0082FF)
  colIvory* = Color(0xFFFFF0FF)
  colKhaki* = Color(0xF0E68CFF)
  colLavender* = Color(0xE6E6FAFF)
  colLavenderBlush* = Color(0xFFF0F5FF)
  colLawnGreen* = Color(0x7CFC00FF)
  colLemonChiffon* = Color(0xFFFACDFF)
  colLightBlue* = Color(0xADD8E6FF)
  colLightCoral* = Color(0xF08080FF)
  colLightCyan* = Color(0xE0FFFFFF)
  colLightGoldenRodYellow* = Color(0xFAFAD2FF)
  colLightGray* = Color(0xD3D3D3FF)
  colLightGreen* = Color(0x90EE90FF)
  colLightGrey* = Color(0xD3D3D3FF)
  colLightPink* = Color(0xFFB6C1FF)
  colLightSalmon* = Color(0xFFA07AFF)
  colLightSeaGreen* = Color(0x20B2AAFF)
  colLightSkyBlue* = Color(0x87CEFAFF)
  colLightSlateGray* = Color(0x778899FF)
  colLightSlateGrey* = Color(0x778899FF)
  colLightSteelBlue* = Color(0xB0C4DEFF)
  colLightYellow* = Color(0xFFFFE0FF)
  colLime* = Color(0x00FF00FF)
  colLimeGreen* = Color(0x32CD32FF)
  colLinen* = Color(0xFAF0E6FF)
  colMagenta* = Color(0xFF00FFFF)
  colMaroon* = Color(0x800000FF)
  colMediumAquaMarine* = Color(0x66CDAAFF)
  colMediumBlue* = Color(0x0000CDFF)
  colMediumOrchid* = Color(0xBA55D3FF)
  colMediumPurple* = Color(0x9370DBFF)
  colMediumSeaGreen* = Color(0x3CB371FF)
  colMediumSlateBlue* = Color(0x7B68EEFF)
  colMediumSpringGreen* = Color(0x00FA9AFF)
  colMediumTurquoise* = Color(0x48D1CCFF)
  colMediumVioletRed* = Color(0xC71585FF)
  colMidnightBlue* = Color(0x191970FF)
  colMintCream* = Color(0xF5FFFAFF)
  colMistyRose* = Color(0xFFE4E1FF)
  colMoccasin* = Color(0xFFE4B5FF)
  colNavajoWhite* = Color(0xFFDEADFF)
  colNavy* = Color(0x000080FF)
  colOldLace* = Color(0xFDF5E6FF)
  colOlive* = Color(0x808000FF)
  colOliveDrab* = Color(0x6B8E23FF)
  colOrange* = Color(0xFFA500FF)
  colOrangeRed* = Color(0xFF4500FF)
  colOrchid* = Color(0xDA70D6FF)
  colPaleGoldenRod* = Color(0xEEE8AAFF)
  colPaleGreen* = Color(0x98FB98FF)
  colPaleTurquoise* = Color(0xAFEEEEFF)
  colPaleVioletRed* = Color(0xDB7093FF)
  colPapayaWhip* = Color(0xFFEFD5FF)
  colPeachPuff* = Color(0xFFDAB9FF)
  colPeru* = Color(0xCD853FFF)
  colPink* = Color(0xFFC0CBFF)
  colPlum* = Color(0xDDA0DDFF)
  colPowderBlue* = Color(0xB0E0E6FF)
  colPurple* = Color(0x800080FF)
  colRebeccaPurple* = Color(0x663399FF)
  colRed* = Color(0xFF0000FF)
  colRosyBrown* = Color(0xBC8F8FFF)
  colRoyalBlue* = Color(0x4169E1FF)
  colSaddleBrown* = Color(0x8B4513FF)
  colSalmon* = Color(0xFA8072FF)
  colSandyBrown* = Color(0xF4A460FF)
  colSeaGreen* = Color(0x2E8B57FF)
  colSeaShell* = Color(0xFFF5EEFF)
  colSienna* = Color(0xA0522DFF)
  colSilver* = Color(0xC0C0C0FF)
  colSkyBlue* = Color(0x87CEEBFF)
  colSlateBlue* = Color(0x6A5ACDFF)
  colSlateGray* = Color(0x708090FF)
  colSlateGrey* = Color(0x708090FF)
  colSnow* = Color(0xFFFAFAFF)
  colSpringGreen* = Color(0x00FF7FFF)
  colSteelBlue* = Color(0x4682B4FF)
  colTan* = Color(0xD2B48CFF)
  colTeal* = Color(0x008080FF)
  colThistle* = Color(0xD8BFD8FF)
  colTomato* = Color(0xFF6347FF)
  colTurquoise* = Color(0x40E0D0FF)
  colViolet* = Color(0xEE82EEFF)
  colWheat* = Color(0xF5DEB3FF)
  colWhite* = Color(0xFFFFFFFF)
  colWhiteSmoke* = Color(0xF5F5F5FF)
  colYellow* = Color(0xFFFF00FF)
  colYellowGreen* = Color(0x9ACD32FF)

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
    assert $colFuchsia == "#FF00FFFF"
  result = "#" & toHex(int(c), 8)

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
  ## `#` followed by 3, 4, 6, or 8 hexadecimal digits.
  result = name.len in [4, 5, 7, 9] and name[0] == '#'
  if result:
    for i in 1 ..< name.len:
      if name[i] notin HexDigits: return false

proc parseColor*(name: string): Color {.raises.} =
  ## Parses `name` to a color value.
  ##
  ## If no valid color could be parsed `ValueError` is raised.
  ## Case insensitive.
  runnableExamples:
    assert parseColor("silver") == Color(0xc0_c0_c0_ff)
    assert parseColor("#0179fc") == Color(0x01_79_fc_ff)
  var name = name
  if name.len > 0 and name[0] == '#':
    if not isHexColor(name): raise ValueError
    elif name.len == 7:
      name = name & "ff"
    elif name.len == 4 or name.len == 5:
      var temp = newStringOfCap(9)
      temp.add('#')
      for c in name[1 .. ^1]:
        temp.add(c)
        temp.add(c)
      if name.len == 4:
        temp.add("ff")
      name = temp
    var v = 0'u32
    for i in 1 ..< name.len:
      let c = name[i]
      let d = if c in {'0'..'9'}: ord(c) - ord('0')
                   elif c in {'a'..'f'}: ord(c) - ord('a') + 10
                   else: ord(c) - ord('A') + 10
      v = (v shl 4) or uint32(d)
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

proc rgba*(r, g, b, a: uint8): Color =
  ## Constructs a color from RGBA values.
  runnableExamples:
    assert rgba(0, 255, 128, 00) == Color(0x00_ff_80_00)
  result = rawRgba(r, g, b, a)

proc rgb*(r, g, b: uint8): Color =
  ## Constructs a color from RGB values.
  runnableExamples:
    assert rgb(0, 255, 128) == Color(0x00_ff_80_ff)
  result = rawRgba(r, g, b, 255)

proc opacity*(c: Color, f: float): Color {.raises.} =
  ## Returns `c` with opacity `f`. `f` must be a float from 0.0 (completely
  ## transparent) to 1.0 (completely opaque).
  ##
  runnableExamples:
    var
      a = Color(0xff_00_ff_ff)
      b = Color(0x00_42_cc_80)
    assert a.opacity(0.5) == Color(0xff_00_ff_80)
    assert b.opacity(0.5) == Color(0x00_42_cc_80)
    
  if f < 0.0 or f > 1.0:
    raise ErrorCode.ValueError
  let alpha = uint32(round(255.0 * f))
  result = Color((c.uint32 and 0xffffff00'u32) or alpha)

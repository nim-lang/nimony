import std/assertions
import std/rgb

# construction / packing
assert int(rgb(255, 128, 0)) == 0xFF8000
assert rgb(255, 0, 0) == colRed
assert rgb(0, 0, 0) == colBlack
assert colWhite == Color(0xFFFFFF)
assert colOrange == Color(0xFFA500)

# components (named-tuple + destructuring)
let e = extractRGB(rgb(255, 128, 0))
assert e.r == 255
assert e.g == 128
assert e.b == 0
let (r, g, b) = extractRGB(colOrange)
assert r == 255 and g == 165 and b == 0

# hex rendering
assert $rgb(255, 128, 0) == "#ff8000"
assert $colRed == "#ff0000"
assert $colBlack == "#000000"
assert $colWhite == "#ffffff"

# blend
assert mix(colBlack, colWhite) == rgb(127, 127, 127)
assert mix(colRed, colBlue) == rgb(127, 0, 127)

# component masking (out-of-range wraps to a byte)
assert rgb(256, -1, 300) == rgb(0, 255, 44)

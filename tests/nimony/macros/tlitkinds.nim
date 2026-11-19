# Raw, triple-quoted and typed literals reach a macro with their own kinds (#3044 item 11).
import std / [syncio, macros]

macro describe(s: untyped): untyped =
  var d = $s.kind & " len=" & $s.len
  if s.len > 0: d.add " child0=" & $s[0].kind
  result = newStrLitNode(d)

macro show(s: untyped): untyped =
  result = newStrLitNode($s.kind & " " & s.strVal)

macro same(s: untyped): untyped =
  result = s

echo describe("plain")
echo describe("""triple""")
echo describe(r"raw")
echo describe(3'u8)
echo describe(1.5'f32)
echo show("plain")
echo show("""triple""")
echo show(r"r\aw")
let x = same(200'u8)
let y = same(2.5'f32)
let z = same(-7'i16)
echo x, " ", y, " ", z, " ", same("""a
b""")

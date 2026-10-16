# std/colors: construction, component extraction, saturated arithmetic,
# `$` and CSS name / hex parsing.

import std/[assertions, syncio, colors]

proc main() =
  assert rgb(0, 255, 128) == Color(0x00_ff_80)
  let gold = extractRGB(colGold)
  assert gold.r == 255 and gold.g == 215 and gold.b == 0
  echo $colFuchsia, " ", $rgb(1, 2, 3), " ", $colBlack

  assert Color(0xaa_00_ff) + Color(0x11_cc_cc) == Color(0xbb_cc_ff)
  assert Color(0xff_33_ff) - Color(0x11_ff_cc) == Color(0xee_00_33)
  assert Color(0xff_00_ff).intensity(0.5) == Color(0x80_00_80)
  assert Color(0x00_42_cc).intensity(0.5) == Color(0x00_21_66)
  proc myMix(x, y: int): int = 2 * x - 3 * y
  assert mix(Color(0x0a2814), Color(0x050a03), myMix) == Color(0x05_32_1f)

  assert isColor("silver") and isColor("SILVER") and isColor("aliceblue")
  assert isColor("yellowgreen") and isColor("#0179fc")
  assert not isColor("#zzmmtt") and not isColor("#") and not isColor("")
  assert not isColor("nosuchcolor")
  try:
    assert parseColor("silver") == colSilver
    assert parseColor("RebeccaPurple") == colRebeccaPurple
    assert parseColor("#0179fc") == Color(0x01_79_fc)
  except:
    assert false
  var raised = 0
  for bad in ["#zzmmtt", "nosuchcolor", ""]:
    try: discard parseColor(bad)
    except: inc raised
  assert raised == 3
  echo "colors ok"

main()

# std/colors: construction, component extraction, saturated arithmetic,
# `$` and CSS name / hex parsing.

import std/[assertions, syncio, colors]

proc main() =
  assert rgba(0, 255, 128, 255) == Color(0x00_ff_80_ff)
  let gold = extractRgba(colGold)
  assert gold.r == 255 and gold.g == 215 and gold.b == 0 and gold.a == 255
  echo $colFuchsia, " ", $rgb(1, 2, 3), " ", $colBlack

  assert Color(0xaa_00_ff_ff) + Color(0x11_cc_cc_ff) == Color(0xbb_cc_ff_ff)
  assert Color(0xff_33_ff_ff) - Color(0x11_ff_cc_ff) == Color(0xee_00_33_00)
  assert Color(0xff_00_ff_ff).intensity(0.5) == Color(0x80_00_80_ff)
  assert Color(0x00_42_cc_ff).intensity(0.5) == Color(0x00_21_66_ff)
  proc myMix(x, y: int): int = 2 * x - 3 * y
  assert mix(Color(0x0a2814ff), Color(0x050a03ff), myMix) == Color(0x05_32_1f_00)

  assert isColor("silver") and isColor("SILVER") and isColor("aliceblue")
  assert isColor("yellowgreen") and isColor("#0179fc")
  assert not isColor("#zzmmtt") and not isColor("#") and not isColor("")
  assert not isColor("nosuchcolor")
  try:
    assert parseColor("silver") == colSilver
    assert parseColor("RebeccaPurple") == colRebeccaPurple
    assert parseColor("#0179fc") == Color(0x01_79_fc_ff)
  except:
    assert false
  var raised = 0
  for bad in ["#zzmmtt", "nosuchcolor", ""]:
    try: discard parseColor(bad)
    except: inc raised
  assert raised == 3
  echo "colors ok"

main()

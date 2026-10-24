# Accuracy of the native std/math (#2585) against correctly rounded results
# (mpmath, 2200 bits). sqrt must be correctly rounded (IEEE 754); sin/cos/tan
# are within 1 ulp in general (fdlibm) and exact on every input below — each
# of which the previous implementation got wrong:
# sqrt was a Newton iteration (0.75 ulp), sin/cos dropped the reduction tail
# (1.4 ulp), tan was sin/cos (2.6 ulp), and the large-argument reducer kept a
# fixed window of fraction bits (134 ulp for cos near a multiple of pi/2).
import std/[syncio, math]

const Cases = [
  ("sqrt", 0x40EBFF19D1A89EBA'u64, 0x406DEE6F06F70F1E'u64),
  ("sqrt", 0x4128EF78E869A8B5'u64, 0x408C3F6AFC6EDDBA'u64),
  ("sqrt", 0x410F22B3412CDE2A'u64, 0x407F9097B21E4AAC'u64),
  ("sqrt", 0x412138B2A45B66A6'u64, 0x408779A9CBA88B04'u64),
  ("sqrt", 0x410B06C1AB0EBE60'u64, 0x407D68826648057A'u64),
  ("sqrt", 0x412C4C0718D00AE4'u64, 0x408E1772170CE00C'u64),
  ("sin", 0x40012481EBD41A66'u64, 0x3FEAE7DDCBDFBFB7'u64),
  ("sin", 0xC0024C8CB808E338'u64, 0xBFE82138F12FBB0E'u64),
  ("sin", 0xBFE930D9B1A18360'u64, 0xBFE6AB1F766F1EEB'u64),
  ("sin", 0x3FEB3E4A0BEB2084'u64, 0x3FE811CD59D9C686'u64),
  ("sin", 0x40027A9069F73646'u64, 0x3FE7A6C84BD5AB74'u64),
  ("sin", 0xC000DEA8438B90B9'u64, 0xBFEB7B150236C59A'u64),
  ("cos", 0x3FF12162332E9A34'u64, 0x3FDEB101F78E24DF'u64),
  ("cos", 0x3FF0C2970D022B0C'u64, 0x3FDFFB9AA6881C53'u64),
  ("cos", 0x3FE82BA048CBF94C'u64, 0x3FE74C2C50F51A15'u64),
  ("cos", 0xC003488B2CE94D8D'u64, 0xBFE7D21150C7A3B7'u64),
  ("cos", 0x4000B8AEDA2EF120'u64, 0xBFDFC414970396FD'u64),
  ("cos", 0xBFF1000EAAFD7156'u64, 0x3FDF25B92A68943E'u64),
  ("cos", 0x7506AC5B262CA1FF'u64, 0xBC214AE72E6BA22F'u64),
  ("tan", 0x3FF07D102282D3F4'u64, 0x3FFAAD1556C1B8DE'u64),
  ("tan", 0x3FE6CB0D7B559880'u64, 0x3FEBA1E52AFCF2A1'u64),
  ("tan", 0x3FEF539A8D9D0F48'u64, 0x3FF7CD32BFAF65BC'u64),
  ("tan", 0xBFEB43C41556FF74'u64, 0xBFF249ABB6F526E9'u64),
  ("tan", 0x3FD6EBADFD737F18'u64, 0x3FD7F41E5F83EFF8'u64),
  ("tan", 0x3FEFC7C0659C65E8'u64, 0x3FF88BD1DF7B0701'u64),
]

proc ulpDist(a, b: float64): int64 =
  var ia = cast[int64](a)
  var ib = cast[int64](b)
  if ia < 0: ia = low(int64) - ia
  if ib < 0: ib = low(int64) - ib
  abs(ia - ib)

var bad = 0
for c in Cases:
  let x = cast[float64](c[1])
  let want = cast[float64](c[2])
  let got = case c[0]
    of "sqrt": sqrt(x)
    of "sin": sin(x)
    of "cos": cos(x)
    else: tan(x)
  if ulpDist(got, want) != 0:
    inc bad
    echo "FAIL ", c[0], " ulp distance ", ulpDist(got, want)
echo "math accuracy: ", Cases.len, " cases, ", bad, " failures"

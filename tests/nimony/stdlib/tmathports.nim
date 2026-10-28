# Lines the native std/math (#2585) dropped from its musl / FreeBSD originals, found by
# a differential audit against glibc (expected values below are glibc's):
#   log2 (f64): the lo * ivln2hi term (errors up to 4.7e14 ulp, worst near sqrt(2));
#   gamma: `z = -z` in the reflection branch (inf for large negative x);
#   lgamma: `t = -t` for t <= 0 (the log of a negative: NaN for about half of x < 0);
#   abs (system): the generic `x < 0` test kept -0.0, so splitDecimal(-0.0) lost its sign;
#   splitDecimal: inf - inf for x = ±inf (modf gives (±inf, ±0)).
import std/[syncio, math]

proc near(got, want: float64; ulps = 2.0): bool =
  if want == 0.0: got == 0.0
  else: abs(got - want) <= ulps * abs(want) * 2.220446049250313e-16

proc bits(x: float64): uint64 = cast[uint64](x)

var bad = 0
template check(cond: bool; what: string) =
  if not cond:
    inc bad
    echo "FAIL ", what

check near(log2(1.4142119875102148), 0.49999839341935837), "log2 near sqrt(2)"
check near(log2(0.7), -0.5145731728297583), "log2(0.7)"
check near(log2(3.0), 1.584962500721156), "log2(3)"
check gamma(-183.99987792968753) == 0.0, "gamma(-183.9998...) is 0"
check near(gamma(-1.5), 2.3632718012073544, 16.0), "gamma(-1.5)"
check near(gamma(-2.5), -0.9453087204829417, 16.0), "gamma(-2.5)"
check gamma(-63.999996'f32) == 0'f32, "gamma(f32 -63.999996) is 0"
check near(lgamma(-1.5), 0.8600470153764812, 16.0), "lgamma(-1.5)"
check near(lgamma(-49.48008429403923), -145.29465994449347, 16.0), "lgamma(-49.48...)"
check near(float64(lgamma(-1.5'f32)), 0.8600470153764812, 1e9), "lgamma(f32 -1.5) is not NaN"
check bits(abs(-0.0)) == 0'u64, "abs(-0.0) is +0.0"
check cast[uint32](abs(-0.0'f32)) == 0'u32, "abs(-0.0'f32) is +0.0"
let sz = splitDecimal(-0.0)
check bits(sz.intpart) == 0x8000000000000000'u64 and bits(sz.floatpart) == 0x8000000000000000'u64, "splitDecimal(-0.0) is (-0, -0)"
let si = splitDecimal(-Inf)
check si.intpart == -Inf and bits(si.floatpart) == 0x8000000000000000'u64, "splitDecimal(-inf) is (-inf, -0)"
let sp = splitDecimal(Inf)
check sp.intpart == Inf and bits(sp.floatpart) == 0'u64, "splitDecimal(inf) is (inf, +0)"
echo "math ports: ", bad, " failures"

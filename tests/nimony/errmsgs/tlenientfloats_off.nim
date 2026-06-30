# nim-lang/nimony#1899: without `.feature: "lenientFloats".`, narrowing a float
# constant to a smaller float type stays rejected (auto-narrowing of floats can
# lose precision, so it must be opted into explicitly).
const M_PI = 3.14
proc takeF32(f: float32) = discard
takeF32 M_PI

import std/[vectors, math, assertions]

# float32 throughout: scalar params and results stay in the vector's width
let a = vec2(3.0'f32, 4.0'f32)
let b = vec2(1.0'f32, 0.0'f32)
let len32: float32 = a.length
assert len32 == 5.0'f32
let d32: float32 = distance(a, b)
assert almostEqual(d32, sqrt(20.0'f32))
assert lerp(b, a, 0.5'f32) == vec2(2.0'f32, 2.0'f32)
assert lerpClamped(b, a, 2.0'f32) == a
assert approxEq(normalize(a), vec2(0.6'f32, 0.8'f32))
assert approxEq(rotate(b, float32(PI / 2.0)), vec2(0.0'f32, 1.0'f32))
assert approxEq(slerp(b, vec2(0.0'f32, 1.0'f32), 0.5'f32),
                vec2(float32(sqrt(0.5)), float32(sqrt(0.5))))
assert approxEq(moveTowards(b, a, 1.0'f32), vec2(1.4472136'f32, 0.8944272'f32))
assert almostEqual(angle(b, vec2(0.0'f32, 2.0'f32)), float32(PI / 2.0))

# int vectors for the algebraic subset
let i3 = vec3(1, 2, 3)
assert dot(i3, i3) == 14
assert cross(vec3(1, 0, 0), vec3(0, 1, 0)) == vec3(0, 0, 1)
assert sum(i3) == 6
assert lerp(vec2(0, 0), vec2(10, 20), 2) == vec2(20, 40)
assert sign(vec3(-2.0, 0.0, 5.0)) == vec3(-1.0, 0.0, 1.0)

# Vec1: the `1..<len` loops are empty and must not trip `[]`'s contract
let v1 = vec1(3.0)
assert dot(v1, v1) == 9.0
assert sum(v1) == 3.0
assert lengthSq(v1) == 9.0
assert minComponent(v1) == 3.0 and maxComponent(v1) == 3.0
assert distanceSq(v1, vec1(1.0)) == 4.0
assert $v1 == "(x=3.0)"

# dimension promotion and demotion
let v4 = vec4(1.0, 2.0, 3.0, 4.0)
assert xy(v4) == vec2(1.0, 2.0)
assert xyz(v4) == vec3(1.0, 2.0, 3.0)
assert to4(vec2(1.0, 2.0)) == vec4(1.0, 2.0, 0.0, 0.0)
assert to1(v4) == vec1(1.0)
assert vec3(vec2(1, 2), 3) == vec3(1, 2, 3)
assert vec3(0, vec2(1, 2)) == vec3(0, 1, 2)
assert vec4(vec3(1, 2, 3), 4) == vec4(1, 2, 3, 4)
assert vec4(vec2(1, 2), 3, 4) == vec4(1, 2, 3, 4)

func pureDot(a, b: Vec3[float32]): float32 = dot(a, b)
assert pureDot(vec3(1.0'f32, 2.0'f32, 3.0'f32), vec3(1.0'f32, 1.0'f32, 1.0'f32)) == 6.0'f32

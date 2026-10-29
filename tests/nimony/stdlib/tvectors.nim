import std/vectors
import std/[assertions, math, syncio]

proc vec2(x, y: float): Vec2[float] =
  result = default(Vec2[float])
  result.x = x
  result.y = y

proc vec3(x, y, z: float): Vec3[float] =
  result = default(Vec3[float])
  result.x = x
  result.y = y
  result.z = z

proc close(a, b: float): bool = abs(a - b) < 1e-5

proc main =
  block: # dimensions, accessors, indexing, equality, and stringification
    var a = vec2(3.0, 4.0)
    assert a.len == 2
    assert a.x == 3.0 and a.y == 4.0
    assert a[0] == 3.0 and a[1] == 4.0
    a[0] = 5.0
    a.y = 6.0
    assert a == vec2(5.0, 6.0)
    assert $vec2(1.0, 2.0) == "(x=1.0, y=2.0)"
    let b = vec3(1.0, 2.0, 3.0)
    assert b.len == 3 and b.z == 3.0
    assert $b == "(x=1.0, y=2.0, z=3.0)"
  
  block: # dot, cross, and lengths
    let a = vec3(1.0, 2.0, 3.0)
    let b = vec3(4.0, -5.0, 6.0)
    assert dot(a, b) == 12.0
    assert lengthSq(a) == 14.0
    assert close(length(a), sqrt(14.0))
  
  block: # normalization, distance, and interpolation
    let a = vec2(3.0, 4.0)
    assert isNormalized(normalize(a))
    assert safeNormalize(vec2(0.0, 0.0), vec2(1.0, 0.0), 1e-8f) == vec2(1.0, 0.0)
    assert distanceSq(a, vec2(0.0, 0.0)) == 25.0
    assert distance(a, vec2(0.0, 0.0)) == 5.0
    assert lerp(vec2(0.0, 2.0), vec2(10.0, 4.0), 0.25) == vec2(2.5, 2.5)
    assert lerpClamped(vec2(0.0, 0.0), vec2(10.0, 10.0), 2.0) == vec2(10.0, 10.0)
    assert moveTowards(vec2(0.0, 0.0), vec2(3.0, 4.0), 2.5) == vec2(1.5, 2.0)
    assert moveTowards(vec2(0.0, 0.0), vec2(3.0, 4.0), 8.0) == vec2(3.0, 4.0)
  
  block: # component math
    assert min(vec3(1.0, 5.0, -2.0), vec3(2.0, 4.0, -3.0)) == vec3(1.0, 4.0, -3.0)
    assert max(vec3(1.0, 5.0, -2.0), vec3(2.0, 4.0, -3.0)) == vec3(2.0, 5.0, -2.0)
    assert clamp(vec3(-1.0, 0.5, 2.0)) == vec3(0.0, 0.5, 1.0)
    assert sum(vec3(1.0, 2.0, 3.0)) == 6.0
    assert minComponent(vec3(1.0, 2.0, 3.0)) == 1.0
    assert maxComponent(vec3(1.0, 2.0, 3.0)) == 3.0
  
  block: # 2D geometry and angles
    let right = vec2(1.0, 0.0)
    let up = vec2(0.0, 1.0)
    assert perpendicular(right) == up
    assert close(angle(right, up), PI / 2.0)
    assert close(toAngle(up), PI / 2.0)
    assert close(rotate(right, PI / 2.0).y, 1.0)
    assert rotateAround(vec2(2.0, 0.0), vec2(1.0, 0.0), PI / 2.0).approxEq(vec2(1.0, 1.0))
  
  block: # approximate equality and predicates
    assert vec2(1.0, 2.0).approxEq(vec2(1.0 + 1e-7, 2.0))
    assert not vec2(1.0, 2.0).approxEq(vec2(1.1, 2.0))
    assert vec2(0.0, 0.0).isZero

main()
echo "ok"
#############################################################################################################################
#################################################### CVECTORS ################################################################
#############################################################################################################################

import std/[broadcast, math]

type
  AbstractVec2[T] = concept of Broadcastable
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)
    proc z(v: Self): T {.error.}
    proc `z=`(v: var Self, val: T) {.error.}
    proc w(v: Self): T {.error.}
    proc `w=`(v: var Self, val: T) {.error.}

  AbstractVec3[T] = concept of Broadcastable
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)
    proc z(v: Self): T
    proc `z=`(v: var Self, val: T)
    proc w(v: Self): T {.error.}
    proc `w=`(v: var Self, val: T) {.error.}

  AbstractVec4[T] = concept of Broadcastable
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)
    proc z(v: Self): T
    proc `z=`(v: var Self, val: T)
    proc w(v: Self): T
    proc `w=`(v: var Self, val: T)

  HasFloor = concept
    proc floor(x: Self): Self

  HasCeil = concept
    proc ceil(x: Self): Self

  HasRound = concept
    proc round(x: Self): Self

  HasAbs = concept
    proc abs(x: Self): Self

  HasSign = concept
    proc sgn(x: Self): Self

  AbstractVector[T] = AbstractVec2[T] | AbstractVec3[T] | AbstractVec4[T]

  Vec[N, T] = object
    components: array[N, T]

  Vec2[T] = Vec[2, T]
  Vec3[T] = Vec[3, T]
  Vec4[T] = Vec[4, T]

func `[]`*[N: static[int], T](v: Vec[N,T], i: int): T {.untyped, requires: i < N.} = 
  return v.components[i]
func `[]=`*[N: static[int], T](v: Vec[N,T], i: int, val: T) {.untyped, requires: i < N.} = 
  v.components[i] = val
func len*[N: static[int], T](v: Vec[N,T]): int {.untyped, inline.} = N
func similar*[N: static[int], T](v: Vec[N,T]): Vec[N, T] {.untyped, inline.} =
  return default(Vec[N, T])

func x[T](v: Vec2[T]):T {.untyped.} = v.components[0]
func y[T](v: Vec2[T]):T {.untyped.} = v.components[1]
func z[T](v: Vec2[T]):T {.untyped, error.}
func w[T](v: Vec2[T]):T {.untyped, error.}
func `x=`[T](v: var Vec2[T], val: T) {.untyped.} = 
  v.components[0] = val
func `y=`[T](v: var Vec2[T], val: T) {.untyped.} = 
  v.components[1] = val
func `z=`[T](v: var Vec2[T], val: T) {.untyped, error.}
func `w=`[T](v: var Vec2[T], val: T) {.untyped, error.}


func x[T](v: Vec3[T]):T {.untyped.} = v.components[0]
func y[T](v: Vec3[T]):T {.untyped.} = v.components[1]
func z[T](v: Vec3[T]):T {.untyped.} = v.components[2]
func w[T](v: Vec3[T]):T {.untyped, error.}
func `x=`[T](v: var Vec3[T], val: T) {.untyped.} = 
  v.components[0] = val
func `y=`[T](v: var Vec3[T], val: T) {.untyped.} = 
  v.components[1] = val
func `z=`[T](v: var Vec3[T], val: T) {.untyped.} = 
  v.components[2] = val
func `w=`[T](v: var Vec3[T], val: T) {.untyped, error.}


func x[T](v: Vec4[T]):T {.untyped.} = v.components[0]
func y[T](v: Vec4[T]):T {.untyped.} = v.components[1]
func z[T](v: Vec4[T]):T {.untyped.} = v.components[2]
func w[T](v: Vec4[T]):T {.untyped.} = v.components[3]
func `x=`[T](v: var Vec4[T], val: T) {.untyped.} = 
  v.components[0] = val
func `y=`[T](v: var Vec4[T], val: T) {.untyped.} = 
  v.components[1] = val
func `z=`[T](v: var Vec4[T], val: T) {.untyped.} = 
  v.components[2] = val
func `w=`[T](v: var Vec4[T], val: T) {.untyped.} =
  v.components[3] = val

# ########################################################################################################################### #
# ################################################# ARITHMETIC OPERATORS #################################################### #
# ########################################################################################################################### #

func `==`*[T; C: Broadcastable[T]](a, b: C): bool {.untyped.} =
  ## Exact component-wise equality. Prefer `approxEq` for float comparisons.
  result = true
  var i = 0

  while result and i < a.len:
    result = result and (a[i] == b[i])
    inc i

#############################################################################################################################
################################################## DOT PRODUCT ##############################################################
#############################################################################################################################

func dot*[T: SomeNumber, C: Broadcastable[T]](a, b: C): T =
  ## Dot product: |a|·|b|·cos(θ).
  ## Positive when Cvectors point the same way, 0 when perpendicular,
  ## negative when opposing. Core to lighting, projection and culling.
  assert a.len == b.len, "Vectors must be of the same len"
  result = 0

  for i in 0..<a.len:
    result += a[i] * b[i]

#############################################################################################################################
################################################## CROSS PRODUCT ############################################################
#############################################################################################################################

func cross*[T: SomeNumber, N: AbstractVec3[T]](a, b: N): N =
  ## Cross product of two AbstractVec3 (right-hand rule).
  ## Returns a new vector perpendicular to both inputs.
  ## Magnitude = |a|·|b|·sin(θ) = area of parallelogram spanned by a and b.
  ## Used for: surface normals, torque, "up" vector in look-at matrices.
  result = similar(a)
  result.x = a.y * b.z - a.z * b.y 
  result.y = a.z * b.x - a.x * b.z
  result.z = a.x * b.y - a.y * b.x

func cross*[T: SomeNumber, N: AbstractVec2[T]](a, b: N): T =
  ## 2D "cross product" (perp-dot product).
  ## Returns the Z component of the 3D cross product when z = 0.
  ## Positive → b is CCW from a; negative → CW.
  ## Used for: winding order tests, signed triangle area, 2D collisions.
  a.x * b.y - a.y * b.x


#############################################################################################################################
################################################## LENGTH / MAGNITUDE #######################################################
#############################################################################################################################

func lengthSq*[T: SomeNumber, C: Broadcastable[T]](v: C): T =
  ## Squared Euclidean length. No sqrt — cheaper for comparisons.
  result = 0
  for i in 0..<v.len:
    result += v[i] * v[i]

func length*[T: SomeNumber, C: Broadcastable[T]](v: C): float =
  ## Euclidean length (magnitude) of a AbstractVec2.
  sqrt(v.lengthSq)

#############################################################################################################################
################################################## NORMALIZATION ############################################################
#############################################################################################################################

func normalize*[T: SomeFloat, C: Broadcastable[T]](v: C): C =
  ## Return a unit-length (length = 1) copy of v.
  ## Result is NaN/Inf when v is the zero Cvector — use safeNormalize if unsure.
  v /! T(v.length)

func safeNormalize*[T: SomeFloat, C: Broadcastable[T]](v, fallback: C): C =
  ## Normalize v, returning fallback when v is nearly zero.
  ## Prevents NaN in degenerate cases (zero-velocity objects, null axes…).
  assert v.len == fallback.len, "Vectors must be of the same len"
  let l = T(v.length)
  if l < 1e-8f: fallback else: v /! l

#############################################################################################################################
################################################## DISTANCE #################################################################
#############################################################################################################################

func distanceSq*[T: SomeNumber, C: Broadcastable[T]](a, b: C): T =
  ## Squared Euclidean distance between two points.
  ## Cheaper than distance() — ideal for radius/proximity checks.
  assert a.len == b.len, "Vectors must be of the same len."
  result = 0
  for i in 0..<a.len:
    let d = a[i] - b[i]
    result += d * d

func distance*[T: SomeNumber, C: Broadcastable[T]](a, b: C): float =
  ## Euclidean distance between two points.
  sqrt(distanceSq(a, b))

#############################################################################################################################
################################################## INTERPOLATION ############################################################
#############################################################################################################################

func lerp*[T: SomeNumber, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Linear interpolation: t=0 → a, t=1 → b.
  ## t is NOT clamped — values outside [0,1] extrapolate beyond the segment.
  @. a +. (b -. a) *! t

func lerpClamped*[T: SomeNumber, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Lerp with t clamped to [0, 1]. Safe version that never extrapolates.
  lerp(a, b, clamp(t, 0.float, 1.float))

func slerp*[T: SomeFloat, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Spherical linear interpolation between two unit CVec3 Cvectors.
  ## Produces constant angular velocity — ideal for camera / orientation blending.
  ## Both a and b must be normalized. t in [0, 1].
  let cosA = clamp(dot(a, b), -1.float, 1.float)
  let angle = arccos(cosA)
  if angle < 1e-6f:
    return lerp(a, b, t)    # nearly identical — fall back to lerp
  let sinA = sin(angle)
  let wa   = sin((1.float - t) * angle) / sinA
  let wb   = sin(t * angle) / sinA

  return @. a *! wa +. b *! wb

func moveTowards*[T: SomeFloat, C: Broadcastable[T]](current, target: C, maxDelta: T): C =
  ## Move current towards target by at most maxDelta units per call.
  ## Will not overshoot. Common in AI movement and UI easing.
  let dv   = target -. current
  let dist = dv.length
  if dist <= maxDelta or dist < 1e-8f:
    target
  else:
    let inv = maxDelta / dist
    @. current +. dv *! inv

func smoothFunc(a, b, x: float): float =
  let t = clamp((x - a) / (b - a), 0.float, 1.float)
  t * t * (3.float - 2.float * t)

func smoothStep*[T: SomeFloat, C: Broadcastable[T]](edge0, edge1, v: C): C =
  ## Per-component smoothstep — smooth Hermite interpolation in [edge0, edge1].
  ## Derivative is zero at the endpoints; useful for smooth procedural shaping.
  assert edge0.len == v.len and edge1.len == v.len, "Vectors should all have the same length."
  result = similar(v)
  for i in 0..<v.len: 
    result[i] = smoothFunc(edge0[i], edge1[i], v[i])

#############################################################################################################################
################################################## COMPONENT-WISE MATH ######################################################
#############################################################################################################################

func abs*[T: HasAbs, C: Broadcastable[T]](v: C): C =
  ## Component-wise absolute value.
  map(v, abs)

func floor*[T: HasFloor, C: Broadcastable[T]](v: C): C =
  ## Component-wise floor.
  map(v, floor)

func ceil*[T: HasCeil, C: Broadcastable[T]](v: C): C =
  ## Component-wise ceil.
  map(v, ceil)

func round*[T: HasRound, C: Broadcastable[T]](v: C): C =
  ## Component-wise round to nearest integer.
  map(v, round)

func fract*[T: HasFloor and SomeNumber, C: Broadcastable[T]](v: C): C =
  ## Component-wise fractional part: v - floor(v). Each result in [0, 1).
  @. v -. map(v, floor)

func min*[T: SomeNumber, C: Broadcastable[T]](a, b: C): C =
  ## Component-wise minimum.
  assert a.len == b.len, "Vectors must be of the same len."
  result = similar(a)
  for i in 0..<result.len:
    result[i] = min(a[i], b[i])

func max*[T: SomeNumber, C: Broadcastable[T]](a, b: C): C =
  ## Component-wise maximum.
  assert a.len == b.len, "Vectors must be of the same len."
  result = similar(a)
  for i in 0..<result.len:
    result[i] = max(a[i], b[i])

func clamp*[T: SomeNumber, C: Broadcastable[T]](v, lo, hi: C): C =
  ## Component-wise clamp.
  assert v.len == lo.len and v.len == hi.len, "Vectors must be of the same len."
  result = similar(v)
  for i in 0..<result.len:
    result[i] = clamp(v[i], lo[i], hi[i])

func clamp*[T: SomeNumber, C: Broadcastable[T]](v: C): C =
  ## Component-wise clamp to [0, 1].
  result = similar(v)
  for i in 0..<result.len:
    result[i] = clamp(v[i], T(0), T(1))

func sign*[T: HasSign, C: Broadcastable[T]](v: C): C =
  ## Component-wise round to nearest integer.
  map(v, sgn)

func sum*[T: SomeNumber, C: Broadcastable[T]](v: C): T =
  ## Sum of all components. Useful for computing averages.
  result = 0
  for i in 0..<v.len:
    result += v[i]

func minComponent*[T: SomeNumber, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Smallest component value of v.
  result = v[0]
  for i in 0..<v.len:
    result = min(result, v[i])

func maxComponent*[T: SomeNumber, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Largest component value of v.
  result = v[0]
  for i in 0..<v.len:
    result = max(result, v[i])

#############################################################################################################################
################################################## GEOMETRY / PHYSICS #######################################################
#############################################################################################################################

func reflect*[T: SomeNumber, C: Broadcastable[T]](incident, normal: C): C =
  ## Reflect the incident Cvector about a unit surface normal (2D).
  ## Formula: incident − 2·dot(incident, normal)·normal.
  let d = T(2) * dot(incident, normal)
  @. incident -. (normal *! d)
  
func refract*[T: SomeFloat, C: Broadcastable[T]](incident, normal: C, eta: float): C =
  ## Refracted direction via Snell's law.
  ## eta = n₁/n₂ (e.g. 1.0/1.5 for air→glass).
  ## incident must be normalized and point towards the surface.
  ## Returns the zero vector on total internal reflection.
  let ni = dot(normal, incident)
  let k  = T(1) - eta * eta * (T(1) - ni * ni)
  if k < 0:
    fill(similar(normal), T(0))    # total internal reflection
  else:
    let c = eta * ni + sqrt(k)
    @. incident *! eta -. normal *! c

func project*[T: SomeFloat, C: Broadcastable[T]](v, onto: C): C =
  ## Project v onto `onto`. onto need not be normalized.
  ## Returns the component of v along the direction of onto.
  onto *! (dot(v, onto) / onto.lengthSq)

func reject*[T: SomeFloat, C: Broadcastable[T]](v, onto: C): C =
  ## Rejection of v from `onto` — the component of v perpendicular to onto.
  ## Invariant: project(v, onto) + reject(v, onto) == v.
  v -. project(v, onto)

func perpendicular*[T: SomeNumber, C: AbstractVec2[T]](v: C): C =
  ## Return a Cvector perpendicular to v, rotated 90° counter-clockwise.
  result = similar(v)
  result.x = -v.y
  result.y = v.x

func faceForward*[T: SomeNumber, C: AbstractVec3[T]](n, incident, nRef: C): C =
  ## Return n oriented so it faces the same hemisphere as nRef relative to
  ## incident. Useful for ensuring surface normals face the camera/viewer.
  if dot(nRef, incident) < 0: 
    n 
  else:
    map(n, proc(val: T): T = -val) 

#############################################################################################################################
################################################## ANGLE UTILITIES ##########################################################
#############################################################################################################################

func angle*[T: SomeFloat, C: Broadcastable[T]](a, b: C): float =
  ## Unsigned angle in radians between two AbstractVec2 Cvectors (both must be non-zero).
  arccos(clamp(dot(a, b) / (a.length * b.length), T(-1), T(1)))

func signedAngle*[T: SomeFloat, C: Broadcastable[T]](a, b: C): float =
  ## Signed angle in radians from a to b.
  ## Positive = counter-clockwise, negative = clockwise.
  arctan2(cross(a, b), dot(a, b))

func toAngle*[T: SomeFloat, C: AbstractVec2[T]](v: C): float =
  ## Angle of v relative to the +X axis in radians (= atan2(y, x)).
  arctan2(v.y, v.x)

func rotate*[T: SomeFloat, C: AbstractVec2[T]](v: C, angle: float): C =
  ## Rotate a AbstractVec2 by `angle` radians counter-clockwise around the origin.
  let c = T(cos(angle)); let s = T(sin(angle))
  result = similar(v)
  result.x = v.x * c - v.y * s
  result.y = v.x * s + v.y * c

func rotateAround*[T: SomeFloat, C: AbstractVec2[T]](v, pivot: C, angle: float): C =
  ## Rotate v around an arbitrary 2D pivot point by `angle` radians.
  assert v.len == pivot.len, "Vectors must be of same len."
  let c  = cos(angle).T; let s = sin(angle).T
  let dx = v.x - pivot.x;  let dy = v.y - pivot.y
  result = similar(v)
  result.x = pivot.x + dx * c - dy * s
  result.y = pivot.y + dx * s + dy * c

#############################################################################################################################
################################################## APPROXIMATE EQUALITY #####################################################
#############################################################################################################################

func approxEq*[T: SomeFloat, C: Broadcastable[T]](a, b: C, eps: float = 1e-6f): bool =
  ## Component-wise approximate equality within epsilon.
  ## Prefer over `==` when comparing computed floating-point vectors.
  result = true
  var i = 0

  while result and i < a.len:
    result = result and (abs(a[i] - b[i]) <= eps)
    inc i

func isNormalized*[T: SomeFloat, C: Broadcastable[T]](v: C, eps: float = 1e-5f): bool =
  abs(v.lengthSq - 1.float) <= eps

func isZero*[T: SomeFloat, C: Broadcastable[T]](v: C, eps: float = 1e-8f): bool =
  v.lengthSq <= eps * eps


#############################################################################################################################
################################################## STRING REPRESENTATION ####################################################
#############################################################################################################################

template `$`*[T](v: Vec2[T]): string {.untyped.} =
  ## Pretty-print any AbstractVec2-compatible value as "(x, y)".
  "(x=" & $v.x & ", y=" & $v.y & ")"

template `$`*[T](v: Vec3[T]): string {.untyped.} =
  ## Pretty-print any CVec3-compatible value as "(x, y, z)".
  "(x=" & $v.x & ", y=" & $v.y & ", z=" & $v.z & ")"

template `$`*[T](v: Vec4[T]): string {.untyped.} =
  ## Pretty-print any CVec4-compatible value as "(x, y, z, w)".
  "(x=" & $v.x & ", y=" & $v.y & ", z=" & $v.z & ", w=" & $v.w & ")"

import std/broadcast
import std/[math, assertions]

type
  Vec2Concept*[T] = concept of Broadcastable[T]
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)

  Vec3Concept*[T] = concept of Broadcastable[T]
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)
    proc z(v: Self): T
    proc `z=`(v: var Self, val: T)

  Vec4Concept*[T] = concept of Broadcastable[T]
    proc x(v: Self): T
    proc `x=`(v: var Self, val: T)
    proc y(v: Self): T
    proc `y=`(v: var Self, val: T)
    proc z(v: Self): T
    proc `z=`(v: var Self, val: T)
    proc w(v: Self): T
    proc `w=`(v: var Self, val: T)

  AbstractVec2*[T] = Vec2Concept[T] and not Vec3Concept[T] and not Vec4Concept
  AbstractVec3*[T] = Vec3Concept[T] and not Vec4Concept[T]
  AbstractVec4*[T] = Vec4Concept[T]

  HasComp = concept
    proc `<`(a, b: Self): bool
    proc `<=`(a, b: Self): bool

  HasFloor = concept
    proc floor(x: Self): Self

  HasCeil = concept
    proc ceil(x: Self): Self

  HasRound = concept
    proc round(x: Self): Self

  HasAbs = concept
    proc abs(x: Self): Self

  HasMin = concept
    proc min(x, y: Self): Self

  HasMax = concept
    proc max(x, y: Self): Self

  HasSign = concept
    proc sgn(x: Self): Self

  HasSqrt = concept
    proc sqrt(x: Self): Self

  HasMul = concept
    proc `*`(x, y: Self): Self

  HasSub = concept
    proc `-`(x, y: Self): Self

  HasNeg = concept
    proc `-`(x: Self): Self

  HasAdd = concept
    proc `+`(x, y: Self): Self

  HasEq = concept
    proc `==`(x, y: Self): bool

  HasTrig = concept
    proc cos(x: Self): Self
    proc sin(x: Self): Self
    proc tan(x: Self): Self
    proc arccos(x: Self): Self
    proc arcsin(x: Self): Self
    proc arctan2(x, y: Self): Self

  HasClamp = concept
    proc clamp(x, lo, hi: Self): Self

  AbstractVector*[T] = AbstractVec2[T] | AbstractVec3[T] | AbstractVec4[T]

  Arithmetic* = concept
    func `+`(a,b: Self): Self
    func `-`(a,b: Self): Self
    func `*`(a,b: Self): Self

  Vec*[N, T] = object
    components: array[N, T]

  Vec2*[T] = Vec[2, T]
  Vec3*[T] = Vec[3, T]
  Vec4*[T] = Vec[4, T]

func `[]`*[N: static[int], T](v: Vec[N,T], i: int): T {.requires: i < N.} = 
  return v.components[i]
func `[]=`*[N: static[int], T](v: var Vec[N,T], i: int, val: T) {.requires: i < N.} = 
  v.components[i] = val
func len*[N: static[int], T](v: Vec[N,T]): int {.inline.} = N
func similar*[N: static[int], T](v: Vec[N,T]): Vec[N, T] {.inline.} =
  return default(Vec[N, T])

func x*[T](v: Vec2[T]):T = v.components[0]
func y*[T](v: Vec2[T]):T = v.components[1]
func `x=`*[T](v: var Vec2[T], val: T) = 
  v.components[0] = val
func `y=`*[T](v: var Vec2[T], val: T) = 
  v.components[1] = val


func x*[T](v: Vec3[T]):T = v.components[0]
func y*[T](v: Vec3[T]):T = v.components[1]
func z*[T](v: Vec3[T]):T = v.components[2]
func `x=`*[T](v: var Vec3[T], val: T) = 
  v.components[0] = val
func `y=`*[T](v: var Vec3[T], val: T) = 
  v.components[1] = val
func `z=`*[T](v: var Vec3[T], val: T) = 
  v.components[2] = val


func x*[T](v: Vec4[T]):T = v.components[0]
func y*[T](v: Vec4[T]):T = v.components[1]
func z*[T](v: Vec4[T]):T = v.components[2]
func w*[T](v: Vec4[T]):T = v.components[3]
func `x=`*[T](v: var Vec4[T], val: T) = 
  v.components[0] = val
func `y=`*[T](v: var Vec4[T], val: T) = 
  v.components[1] = val
func `z=`*[T](v: var Vec4[T], val: T) = 
  v.components[2] = val
func `w=`*[T](v: var Vec4[T], val: T) =
  v.components[3] = val

# ########################################################################################################################### #
# ################################################# ARITHMETIC OPERATORS #################################################### #
# ########################################################################################################################### #

template `+`*[T: Arithmetic; V: AbstractVector[T]](a, b: V): V = a +. b
template `-`*[T: Arithmetic; V: AbstractVector[T]](a, b: V): V = a -. b
template `*`*[T: Arithmetic; V: AbstractVector[T]](a, b: V): V = a *. b
template `/`*[T: ArithmeticAndDiv; V: AbstractVector[T]](a, b: V): V = a /. b

func `==`*[T: HasEq; C: Broadcastable[T]](a, b: C): bool =
  ## Exact component-wise equality. Prefer `approxEq` for float comparisons.
  result = true
  var i = 0

  while result and i < a.len:
    result = result and (a[i] == b[i])
    inc i

# ############################################################################################################################
# ################################################# DOT PRODUCT ##############################################################
# ############################################################################################################################

proc dot*[T: Arithmetic, C: Broadcastable[T]](a, b: C): T {.requires: a.len > 0 and b.len > 0.} =
  ## Dot product: |a|·|b|·cos(θ).
  ## Positive when vectors point the same way, 0 when perpendicular,
  ## negative when opposing. Core to lighting, projection and culling.
  assert a.len == b.len, "Vectors must be of the same len"
  result = a[0] * b[0]

  for i in 1..<a.len:
    result = result + a[i] * b[i]

# ############################################################################################################################
# ################################################# CROSS PRODUCT ############################################################
# ############################################################################################################################

func cross*[T: Arithmetic, N: AbstractVec3[T]](a, b: N): N =
  ## Cross product of two AbstractVec3 (right-hand rule).
  ## Returns a new vector perpendicular to both inputs.
  ## Magnitude = |a|·|b|·sin(θ) = area of parallelogram spanned by a and b.
  ## Used for: surface normals, torque, "up" vector in look-at matrices.
  result = similar(a)
  result.x = a.y * b.z - a.z * b.y 
  result.y = a.z * b.x - a.x * b.z
  result.z = a.x * b.y - a.y * b.x

func cross*[T: Arithmetic, N: AbstractVec2[T]](a, b: N): T =
  ## 2D "cross product" (perp-dot product).
  ## Returns the Z component of the 3D cross product when z = 0.
  ## Positive → b is CCW from a; negative → CW.
  ## Used for: winding order tests, signed triangle area, 2D collisions.
  a.x * b.y - a.y * b.x


# ############################################################################################################################
# ################################################# LENGTH / MAGNITUDE #######################################################
# ############################################################################################################################

func lengthSq*[T: Arithmetic, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Squared Euclidean length. No sqrt. Cheaper for comparisons.
  result = v[0] * v[0]
  for i in 1..<v.len:
    result = result + v[i] * v[i]

func length*[T: Arithmetic; C: Broadcastable[T]](v: C): float {.requires: v.len > 0.}=
  ## Euclidean length (magnitude) of a AbstractVec2.
  sqrt(v.lengthSq.float)

# ############################################################################################################################
# ################################################# NORMALIZATION ############################################################
# ############################################################################################################################

func normalize*[T: ArithmeticAndDiv and SomeFloat, C: Broadcastable[T]](v: C): C =
  ## Return a unit-length (length = 1) copy of v.
  ## Result is NaN/Inf when v is the zero vector. Use safeNormalize if unsure.
  v /! T(v.length)

proc safeNormalize*[T: ArithmeticAndDiv and HasComp, C: Broadcastable[T]](v, fallback: C, threshold: T): C =
  ## Normalize v, returning fallback when v is nearly zero.
  ## Prevents NaN in degenerate cases (zero-velocity objects, null axes…).
  assert v.len == fallback.len, "Vectors must be of the same len"
  let l = T(v.length)
  if l < threshold: fallback else: v /! l

# ############################################################################################################################
# ################################################# DISTANCE #################################################################
# ############################################################################################################################

proc distanceSq*[T: Arithmetic, C: Broadcastable[T]](a, b: C): T {.requires: a.len > 0 and b.len > 0.} =
  ## Squared Euclidean distance between two points.
  ## Cheaper than distance() — ideal for radius/proximity checks.
  assert a.len == b.len, "Vectors must be of the same len."
  result = a[0] - b[0]
  result = result * result
  for i in 1..<a.len:
    let d = a[i] - b[i]
    result = result + d * d

proc distance*[T: Arithmetic and SomeFloat, C: Broadcastable[T]](a, b: C): float {.requires: a.len > 0 and b.len > 0.} =
  ## Euclidean distance between two points.
  sqrt(float(distanceSq(a, b)))

# ############################################################################################################################
# ################################################# INTERPOLATION ############################################################
# ############################################################################################################################

func lerp*[T: Arithmetic, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Linear interpolation: t=0 → a, t=1 → b.
  ## t is NOT clamped — values outside [0,1] extrapolate beyond the segment.
  @. a +. (b -. a) *! t

func lerpClamped*[T: Arithmetic, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Lerp with t clamped to [0, 1]. Safe version that never extrapolates.
  lerp(a, b, clamp(t, 0.float, 1.float))

func slerp*[T: ArithmeticAndDiv and SomeFloat, C: Broadcastable[T]](a, b: C, t: float): C =
  ## Spherical linear interpolation between two unit Vec3 vectors.
  ## Produces constant angular velocity — ideal for camera / orientation blending.
  ## Both a and b must be normalized. t in [0, 1].
  let cosA = clamp(float(dot(a, b)), -1.float, 1.float)
  let angle = arccos(cosA)
  if angle < 1e-6f:
    return lerp(a, b, t)    # nearly identical — fall back to lerp
  let sinA = sin(angle)
  let wa   = sin((1.float - t) * angle) / sinA
  let wb   = sin(t * angle) / sinA

  return @. a *! wa +. b *! wb

func moveTowards*[T: SomeFloat, C: Broadcastable[T]](current, target: C, maxDelta: float): C =
  ## Move current towards target by at most maxDelta units per call.
  ## Will not overshoot. Common in AI movement and UI easing.
  let dv   = target -. current
  let dist = dv.length
  if dist <= maxDelta or dist < 1e-8f:
    target
  else:
    let inv = T(maxDelta / dist)
    @. current +. dv *! inv

func smoothFunc[T: SomeFloat and ArithmeticAndDiv](a, b, x: T): T =
  let t = T(clamp(((x - a) / (b - a)), 0.T, 1.T))
  t * t * (3.T - 2.T * t)

proc smoothStep*[T: SomeFloat, C: Broadcastable[T]](edge0, edge1, v: C): C =
  ## Per-component smoothstep — smooth Hermite interpolation in [edge0, edge1].
  ## Derivative is zero at the endpoints; useful for smooth procedural shaping.
  assert edge0.len == v.len and edge1.len == v.len, "Vectors should all have the same length."
  result = similar(v)
  for i in 0..<v.len: 
    result[i] = smoothFunc(edge0[i], edge1[i], v[i])


# ############################################################################################################################
# ################################################# COMPONENT-WISE MATH ######################################################
# ############################################################################################################################

func abs*[T: HasAbs, C: Broadcastable[T]](v: C): C =
  ## Component-wise absolute value.
  map(v, (proc (x: T): T)(abs))

func floor*[T: HasFloor, C: Broadcastable[T]](v: C): C =
  ## Component-wise floor.
  map(v, (proc (x: T): T)(floor))

func ceil*[T: HasCeil, C: Broadcastable[T]](v: C): C =
  ## Component-wise ceil.
  map(v, (proc (x: T): T)(ceil))

func round*[T: HasRound, C: Broadcastable[T]](v: C): C =
  ## Component-wise round to nearest integer.
  map(v, (proc (x: T): T)(round))

func fract*[T: HasFloor and SomeNumber, C: Broadcastable[T]](v: C): C =
  ## Component-wise fractional part: v - floor(v). Each result in [0, 1).
  @. v -. map(v, (proc (x: T): T)(floor))

proc min*[T: HasMin, C: Broadcastable[T]](a, b: C): C =
  ## Component-wise minimum.
  assert a.len == b.len, "Vectors must be of the same len."
  result = similar(a)
  for i in 0..<result.len:
    result[i] = min(a[i], b[i])

proc max*[T: HasMax, C: Broadcastable[T]](a, b: C): C =
  ## Component-wise maximum.
  assert a.len == b.len, "Vectors must be of the same len."
  result = similar(a)
  for i in 0..<result.len:
    result[i] = max(a[i], b[i])

proc clamp*[T: SomeNumber, C: Broadcastable[T]](v, lo, hi: C): C =
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
  map(v, (proc (x: T): T)(sgn))

func sum*[T: Arithmetic, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Sum of all components. Useful for computing averages.
  result = v[0]
  for i in 1..<v.len:
    result = result + v[i]

func minComponent*[T: HasMin, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Smallest component value of v.
  result = v[0]
  for i in 1..<v.len:
    result = min(result, v[i])

func maxComponent*[T: HasMax, C: Broadcastable[T]](v: C): T {.requires: v.len > 0.} =
  ## Largest component value of v.
  result = v[0]
  for i in 1..<v.len:
    result = max(result, v[i])

# ############################################################################################################################
# ################################################# GEOMETRY / PHYSICS #######################################################
# ############################################################################################################################

func reflect*[T: SomeNumber and Arithmetic, C: Broadcastable[T]](incident, normal: C): C =
  ## Reflect the incident vector about a unit surface normal (2D).
  ## Formula: incident − 2·dot(incident, normal)·normal.
  let d = T(2) * dot(incident, normal)
  @. incident -. (normal *! d)
  
func refract*[T: SomeFloat and Arithmetic and HasComp and HasSqrt, C: Broadcastable[T]](incident, normal: C, eta: T): C =
  ## Refracted direction via Snell's law.
  ## eta = n₁/n₂ (e.g. 1.0/1.5 for air→glass).
  ## incident must be normalized and point towards the surface.
  ## Returns the zero vector on total internal reflection.
  let ni = dot(normal, incident)
  let k  = T(1.0) - eta * eta * (T(1.0) - ni * ni)
  if k < 0.T:
    result = similar(normal)
    fill(result, T(0))    # total internal reflection
  else:
    let c = eta * ni + sqrt(k)
    result = @. incident *! eta -. normal *! c

func project*[T: SomeFloat and ArithmeticAndDiv, C: Broadcastable[T]](v, onto: C): C =
  ## Project v onto `onto`. onto need not be normalized.
  ## Returns the component of v along the direction of onto.
  onto *! (dot(v, onto) / onto.lengthSq)

func reject*[T: SomeFloat, C: Broadcastable[T]](v, onto: C): C =
  ## Rejection of v from `onto` — the component of v perpendicular to onto.
  ## Invariant: project(v, onto) + reject(v, onto) == v.
  v -. project(v, onto)

func perpendicular*[T: SomeNumber and HasNeg, C: AbstractVec2[T]](v: C): C =
  ## Return a vector perpendicular to v, rotated 90° counter-clockwise.
  result = similar(v)
  result.x = -v.y
  result.y = v.x

func faceForward*[T: SomeNumber and HasNeg and HasComp, C: AbstractVec3[T]](n, incident, nRef: C): C =
  ## Return n oriented so it faces the same hemisphere as nRef relative to
  ## incident. Useful for ensuring surface normals face the camera/viewer.
  if dot(nRef, incident) < T(0): 
    result = n 
  else:
    result = similar(n)
    result.x = -n.x
    result.y = -n.y
    result.z = -n.z

# ############################################################################################################################
# ################################################# ANGLE UTILITIES ##########################################################
# ############################################################################################################################

proc angle*[T: SomeFloat and ArithmeticAndDiv and HasTrig and HasClamp, C: AbstractVec2[T]](a, b: C): T =
  ## Unsigned angle in radians between two AbstractVec2 vectors (both must be non-zero).
  arccos(clamp(dot(a, b) / T(a.length * b.length), T(-1), T(1)))

func signedAngle*[T: SomeFloat and HasTrig and Arithmetic, C: AbstractVec2[T]](a, b: C): T =
  ## Signed angle in radians from a to b.
  ## Positive = counter-clockwise, negative = clockwise.
  arctan2(cross(a, b), dot(a, b))

func toAngle*[T: SomeFloat and HasTrig, C: AbstractVec2[T]](v: C): T =
  ## Angle of v relative to the +X axis in radians (= atan2(y, x)).
  arctan2(v.y, v.x)

func rotate*[T: SomeFloat and Arithmetic and HasTrig, C: AbstractVec2[T]](v: C, angle: float): C =
  ## Rotate a AbstractVec2 by `angle` radians counter-clockwise around the origin.
  let c = T(cos(angle))
  let s = T(sin(angle))
  result = similar(v)
  result.x = v.x * c - v.y * s
  result.y = v.x * s + v.y * c

proc rotateAround*[T: SomeFloat and Arithmetic and HasTrig, C: AbstractVec2[T]](v, pivot: C, angle: float): C =
  ## Rotate v around an arbitrary 2D pivot point by `angle` radians.
  assert v.len == pivot.len, "Vectors must be of same len."
  let c  = T(cos(angle)); let s = T(sin(angle))
  let dx = v.x - pivot.x;  let dy = v.y - pivot.y
  result = similar(v)
  result.x = pivot.x + dx * c - dy * s
  result.y = pivot.y + dx * s + dy * c

# ############################################################################################################################
# ################################################# APPROXIMATE EQUALITY #####################################################
# ############################################################################################################################

func approxEq*[T: SomeFloat and Arithmetic and HasComp, C: Broadcastable[T]](a, b: C, eps: float = 1e-6f): bool =
  ## Component-wise approximate equality within epsilon.
  ## Prefer over `==` when comparing computed floating-point vectors.
  result = true
  var i = 0

  while result and i < a.len:
    result = result and (abs(a[i] - b[i]) <= eps)
    inc i

func isNormalized*[T: SomeFloat and Arithmetic and HasComp, C: Broadcastable[T]](v: C, eps: T = T(1e-5f)): bool =
  abs(v.lengthSq - 1.T) <= eps

func isZero*[T: SomeFloat and Arithmetic and HasComp, C: Broadcastable[T]](v: C, eps: T = T(1e-8f)): bool =
  v.lengthSq <= eps * eps


# ############################################################################################################################
# ################################################# STRING REPRESENTATION ####################################################
# ############################################################################################################################

template `$`*[T: Stringable](v: Vec2[T]): string =
  ## Pretty-print any AbstractVec2-compatible value as "(x, y)".
  "(x=" & $v.x & ", y=" & $v.y & ")"

template `$`*[T: Stringable](v: Vec3[T]): string =
  ## Pretty-print any Vec3-compatible value as "(x, y, z)".
  "(x=" & $v.x & ", y=" & $v.y & ", z=" & $v.z & ")"

template `$`*[T: Stringable](v: Vec4[T]): string =
  ## Pretty-print any Vec4-compatible value as "(x, y, z, w)".
  "(x=" & $v.x & ", y=" & $v.y & ", z=" & $v.z & ", w=" & $v.w & ")"






# Ported from vmath/tests/bench_raytracer.nim under the accompanying MIT license.
# Vec3 color channels use x, y, z and all scalar math uses float64.
import std/[math, syncio]
const
  ImageSize = 16
  TileStart = 0
  TileCount = 16
  FarAway: float64 = 1000000.0
  MaxDepth = 5

type
  Vec3 = object
    x: float64
    y: float64
    z: float64
  Camera = object
    forward: Vec3
    right: Vec3
    up: Vec3
    pos: Vec3
  Ray = object
    start: Vec3
    dir: Vec3
  Thing = object
    kind: int
    surface: int
    center: Vec3
    radius2: float64
    normal: Vec3
    offset: float64
  Intersection = object
    thing: int
    dist: float64
  Light = object
    pos: Vec3
    color: Vec3
  Properties = object
    diffuse: Vec3
    specular: Vec3
    reflect: float64
    roughness: float64

proc vector(x: float64, y: float64, z: float64): Vec3 =
  ## Constructs a position, direction, or RGB color.
  return Vec3(x: x, y: y, z: z)

proc add(a: Vec3, b: Vec3): Vec3 =
  ## Adds vector or color components.
  return vector(a.x + b.x, a.y + b.y, a.z + b.z)

proc subtract(a: Vec3, b: Vec3): Vec3 =
  ## Subtracts vector components.
  return vector(a.x - b.x, a.y - b.y, a.z - b.z)

proc scale(a: Vec3, amount: float64): Vec3 =
  ## Scales vector or color components.
  return vector(a.x * amount, a.y * amount, a.z * amount)

proc multiply(a: Vec3, b: Vec3): Vec3 =
  ## Multiplies color components.
  return vector(a.x * b.x, a.y * b.y, a.z * b.z)

proc dot(a: Vec3, b: Vec3): float64 =
  ## Computes the scalar product.
  return a.x * b.x + a.y * b.y + a.z * b.z

proc cross(a: Vec3, b: Vec3): Vec3 =
  ## Computes the vector product.
  return vector(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x)

proc magnitude(a: Vec3): float64 =
  ## Computes vector length.
  return sqrt(dot(a, a))

proc normalize(a: Vec3): Vec3 =
  ## Divides each component by length as vmath does.
  let length = magnitude(a)
  return vector(a.x / length, a.y / length, a.z / length)

proc newCamera(pos: Vec3, lookAt: Vec3): Camera =
  ## Builds the original camera basis.
  let
    down = vector(0.0, -1.0, 0.0)
    forward = normalize(subtract(lookAt, pos))
    right = cross(forward, down)
    up = cross(forward, right)
  return Camera(pos: pos, forward: forward,
    right: scale(normalize(right), 1.5), up: scale(normalize(up), 1.5))

const
  White = vector(1.0, 1.0, 1.0)
  Grey = vector(0.5, 0.5, 0.5)
  Black = vector(0.0, 0.0, 0.0)
const
  things = [
    Thing(kind: 0, surface: 0, normal: vector(0.0, 1.0, 0.0), offset: 0.0),
    Thing(kind: 1, surface: 1, center: vector(0.0, 1.0, -0.25), radius2: 1.0),
    Thing(kind: 1, surface: 1, center: vector(-1.0, 0.5, 1.5), radius2: 0.25)
  ]
  lights = [
    Light(pos: vector(-2.0, 2.5, 0.0), color: vector(0.49, 0.07, 0.07)),
    Light(pos: vector(1.5, 2.5, 1.5), color: vector(0.07, 0.07, 0.49)),
    Light(pos: vector(1.5, 2.5, -1.5), color: vector(0.07, 0.49, 0.071)),
    Light(pos: vector(0.0, 3.5, 0.0), color: vector(0.21, 0.21, 0.35))
  ]
  camera = newCamera(vector(3.0, 2.0, 4.0), vector(-1.0, 0.5, 0.0))

proc getNormal(thing: int, pos: Vec3): Vec3 =
  ## Computes the original sphere or plane normal.
  let obj = things[thing]
  if obj.kind == 1:
    return normalize(subtract(pos, obj.center))
  return obj.normal

proc objectIntersect(thing: int, ray: Ray): Intersection =
  ## Returns an object index or negative one for a miss.
  let obj = things[thing]
  if obj.kind == 1:
    let
      eo = subtract(obj.center, ray.start)
      projection = dot(eo, ray.dir)
    if projection >= 0.0:
      var distance: float64 = 0.0
      let discriminant = (obj.radius2 -
        (dot(eo, eo) - projection * projection))
      if discriminant >= 0.0:
        distance = projection - sqrt(discriminant)
      if distance != 0.0:
        return Intersection(thing: thing, dist: distance)
  else:
    let denominator = dot(obj.normal, ray.dir)
    if denominator < 0.0:
      return Intersection(thing: thing,
        dist: (dot(obj.normal, ray.start) + obj.offset) / -denominator)
  return Intersection(thing: -1, dist: 0.0)

proc getProperties(thing: int, pos: Vec3): Properties =
  ## Selects shiny or checkerboard material properties.
  if things[thing].surface == 1:
    return Properties(diffuse: White, specular: Grey,
      reflect: 0.7, roughness: 250.0)
  let value = int(floor(pos.z) + floor(pos.x))
  if value mod 2 != 0:
    return Properties(diffuse: White, specular: White,
      reflect: 0.1, roughness: 150.0)
  return Properties(diffuse: Black, specular: White,
    reflect: 0.7, roughness: 150.0)

proc intersections(ray: Ray): Intersection =
  ## Finds the nearest intersection before the original far plane.
  var
    closest = FarAway
    nearest = Intersection(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    let hit = objectIntersect(i, ray)
    if hit.thing != -1 and hit.dist < closest:
      nearest = hit
      closest = hit.dist
  return nearest

proc shade(ray: Ray, hit: Intersection, depth: int): Vec3

proc traceRay(ray: Ray, depth: int): Vec3 =
  ## Traces primary or reflected rays using the original background.
  let hit = intersections(ray)
  if hit.thing != -1:
    return shade(ray, hit, depth)
  return Black

proc getNaturalColor(thing: int, pos: Vec3, normal: Vec3,
    reflectDir: Vec3): Vec3 =
  ## Accumulates unshadowed diffuse and specular light.
  var color = Black
  let
    reflectNorm = normalize(reflectDir)
    properties = getProperties(thing, pos)
  for i in 0 ..< 4:
    let
      light = lights[i]
      lightDist = subtract(light.pos, pos)
      lightVec = normalize(lightDist)
      lightLength = magnitude(lightDist)
      shadowRay = Ray(start: pos, dir: lightVec)
      shadowHit = intersections(shadowRay)
      inShadow = shadowHit.thing != -1 and shadowHit.dist <= lightLength
    if not inShadow:
      let
        illumination = dot(lightVec, normal)
        specular = dot(lightVec, reflectNorm)
      var
        lightColor = Black
        specularColor = Black
      if illumination > 0.0:
        lightColor = scale(light.color, illumination)
      if specular > 0.0:
        specularColor = scale(light.color, pow(specular, properties.roughness))
      lightColor = multiply(lightColor, properties.diffuse)
      specularColor = multiply(specularColor, properties.specular)
      color = add(add(color, lightColor), specularColor)
  return color

proc shade(ray: Ray, hit: Intersection, depth: int): Vec3 =
  ## Combines natural illumination and recursive reflections.
  let
    direction = ray.dir
    pos = add(scale(direction, hit.dist), ray.start)
    normal = getNormal(hit.thing, pos)
    reflectDir = subtract(direction,
      scale(scale(normal, dot(normal, direction)), 2.0))
    natural = add(Black,
      getNaturalColor(hit.thing, pos, normal, reflectDir))
  var reflected = Grey
  if depth < MaxDepth:
    let
      reflectedRay = Ray(start: pos, dir: reflectDir)
      color = traceRay(reflectedRay, depth + 1)
      properties = getProperties(hit.thing, pos)
    reflected = scale(color, properties.reflect)
  return add(natural, reflected)

proc getPoint(x: int, y: int): Vec3 =
  ## Retains vmath's pixel coordinates and camera projection.
  let
    width = float64(ImageSize)
    xf = float64(x)
    yf = float64(y)
    recenterX = (xf - width / 2.0) / 2.0 / width
    recenterY = -(yf - width / 2.0) / 2.0 / width
    horizontal = scale(camera.right, recenterX)
    vertical = scale(camera.up, recenterY)
  return normalize(add(camera.forward, add(horizontal, vertical)))

proc channel(value: float64): int =
  ## Retains the original unchecked byte wrapping for over-bright colors.
  return int(round(value * 255.0)) mod 256

for i in 0 ..< TileCount:
  let
    pixel = TileStart + i
    x = pixel mod ImageSize
    y = pixel div ImageSize
    ray = Ray(start: camera.pos, dir: getPoint(x, y))
    color = traceRay(ray, 0)
  echo channel(color.x), channel(color.y), channel(color.z)

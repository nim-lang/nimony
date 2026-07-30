import deps/mfieldvis
import deps/mfieldvisnest

# Expansion nested inside instantiation, spanning three modules.
var g = createGeneric[int](7)
touchPublic(g)
discard viaOwner(g)

# ...and the same nesting must NOT launder a private access written here.
template outer(body: untyped): untyped = body
template inner(body: untyped): untyped = outer(body)
discard inner(g.private)

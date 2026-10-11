# The foreign half of timportedmacro: a macro DECLARED here and used as a
# proc pragma by the importing test module. Identity, like
# macros/tprocdef_roundtrip — the point is not what it does but WHERE it is.
import std/macros

macro traced*(p: untyped): untyped =
  result = p

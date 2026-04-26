# Helper for `tforward_decl_export.nim`. Exports a public proc that has a
# forward declaration AND a matching implementation. The forward decl must
# not survive into this module's exported symbol set, otherwise importers
# would see two `isEmpty` symbols and report an ambiguous-call error.

proc isEmpty*(x: int): bool

proc useInternally*(x: int): bool =
  result = isEmpty(x)

proc isEmpty*(x: int): bool =
  result = x == 0

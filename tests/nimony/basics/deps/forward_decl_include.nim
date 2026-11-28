# Included by `tforward_decl.nim`: implements one of its forward declarations
# and forward-declares a routine that the includer implements.
proc fromIncluder(): int
proc fromInclude(): int = 21

# The alias is the file name of the module whose `value` the import
# re-exports: that module's name is not in scope, so the alias is the only
# `mreexportsymbol_source`.
import std/syncio
import deps/mreexportsymbol as mreexportsymbol_source

proc main() =
  try:
    let s = mreexportsymbol_source.open()
    echo s.value
  except ErrorCode:
    echo "unreachable"

main()

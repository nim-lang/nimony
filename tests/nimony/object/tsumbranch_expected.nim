# A branch name shared with an imported sum type: the expected type picks the
# branch, else the module's own one wins (#3044 item 13).
import std/syncio
import ./msumbranch

type
  Command = object
    case
    of Help:
      discard
    of Version:
      discard

proc mk(): Command =
  result = Version()

let f: FileVersion = Version(mtime: 3)
case f
of Missing(): echo "missing"
of Version(mtime): echo mtime
case mk()
of Help(): echo "help"
of Version(): echo "version"
let g = Help()
case g
of Help(): echo "own help"
of Version(): echo "own version"

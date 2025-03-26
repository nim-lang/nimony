
import std / [syncio]

import deps / ismain_dep
include deps / ismain_include

echo "include file says: ", isMainInclude()

when isMainModule:
  echo "main, client is: ", ClientIsMain
else:
  echo "not main, client is: ", ClientIsMain

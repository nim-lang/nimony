
import std / [syncio]

import deps / ismain_dep

when isMainModule:
  echo "main, client is: ", ClientIsMain
else:
  echo "not main, client is: ", ClientIsMain

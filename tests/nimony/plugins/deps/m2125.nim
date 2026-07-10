# issue #2125
import plugins

var r = loadReplacer()
loopKeepTag r:
  keep r, Any
saveReplacer(r)

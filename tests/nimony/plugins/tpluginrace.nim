# Regression test for the plugin build race (ETXTBSY / clobbered plugin
# nimcache). Eight independent modules use one plugin, so nifmake sems them in
# parallel. Before the plugin executable became a node of the build graph,
# every one of those nimsem processes reached `runPlugin` for the same,
# not-yet-built executable at the same time and built it, and this failed
# almost every run with `Text file busy` on the exec side, or with
# undefined-symbol link errors from several plugin sub-compiles sharing one
# nimcache. Now the dependency scanner lists the plugin from nifler's deps
# file, nifmake builds it exactly once before the sem runs that have it as an
# input, and the lazy path in `runPlugin` is only a lock-free fallback.
#
# It bites only when the plugin actually needs building — a cold nimcache, or
# after `deps/mraceplugin.nim` is touched. Armed properly it failed 6/6
# before the fix and 0/10 after; `nimony c --report` shows `pluginbuild=1`.
import std / syncio
import deps/mrace1, deps/mrace2, deps/mrace3, deps/mrace4
import deps/mrace5, deps/mrace6, deps/mrace7, deps/mrace8

echo race1() + race2() + race3() + race4() +
     race5() + race6() + race7() + race8()

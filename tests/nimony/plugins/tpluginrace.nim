# Regression test for the plugin build race (ETXTBSY / clobbered plugin
# nimcache). Eight independent modules use one plugin, so nifmake sems them in
# parallel and every one of those nimsem processes reaches `runPlugin` for the
# same, not-yet-built executable at the same time. Before `compilePlugin` took
# a lock and installed the executable atomically, this failed almost every run
# with `Text file busy` on the exec side, or with undefined-symbol link errors
# from several plugin sub-compiles sharing one nimcache.
#
# It bites only when the plugin actually needs building — a cold nimcache, or
# after `deps/mraceplugin.nim` is touched. Deleting just the executable is NOT
# enough to arm it: the plugin SOURCE is what the sem nodes take as an input,
# so with the modules already semmed nimsem never runs and never reaches
# `runPlugin`. Armed properly it failed 6/6 before the fix and 0/8 after.
import std / syncio
import deps/mrace1, deps/mrace2, deps/mrace3, deps/mrace4
import deps/mrace5, deps/mrace6, deps/mrace7, deps/mrace8

echo race1() + race2() + race3() + race4() +
     race5() + race6() + race7() + race8()

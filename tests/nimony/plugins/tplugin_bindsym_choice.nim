# Exercises `bindSym` against an overloaded name in a nimony-compiled plugin.
# `add` has several overloads in `system`; bindSym emits a `(cchoice …)`
# subtree, and the call-site sem resolves it against the actual argument
# types — `add(buf, "from plugin")` matches `add(string, string)`.
import std / syncio

template doAdd(buf: string; lit: string) {.plugin: "deps/mplugin_bindsym_choice".}

var buf = "init "
doAdd(buf, "from plugin")
echo buf

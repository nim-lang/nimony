# Exercises `bindSym` inside a nimony-compiled plugin: the plugin's call to
# `bindSym("echo")` is resolved at *plugin sem time* (against the plugin's
# def-site, which has `import std/syncio`) and the emitted NIF references
# the system-resolved symbol — so the caller can't shadow `echo` with a
# local definition.
import std / syncio

template wrapEcho(s: string) {.plugin: ("deps/mplugin_bindsym", "nimony").}

# Even with a misleading local `echo` proc, the macro should bind to the
# system one via bindSym.
proc echo(x: string) = discard
wrapEcho("hygienic echo from the plugin")

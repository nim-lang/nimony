# Same as tplugin1.nim but uses the new `nim3plugins` API selected via
# `.plugin: ("path", "nimony")`. The plugin source is compiled by Nimony,
# not Nim 2, so it can use bindSym and other Nimony-only macro primitives.
import std / syncio

template generateEcho(s: string) {.plugin: ("deps/mplugin1_nim3", "nimony").}

generateEcho("Hello from a nimony-compiled plugin!")

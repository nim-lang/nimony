# Same shape as tplugin1.nim but uses the new tuple form
# `.plugin: ("path", "v2")`. Explicitly declares the plugin should be
# compiled with Nim 2 — current default behaviour.
import std / syncio

template generateEcho(s: string) {.plugin: ("deps/mplugin1", "v2").}

generateEcho("Hello, plugin v2!")

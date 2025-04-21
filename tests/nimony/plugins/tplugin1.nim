
import std / syncio

template generateEcho(s: string) {.plugin: "deps/tplugin1".}

generateEcho("Hello, world!")

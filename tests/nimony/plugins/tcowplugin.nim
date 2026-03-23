import std / syncio

template generateEcho(s: string) {.plugin: "deps/mcowplugin".}

generateEcho("Hello, world!")

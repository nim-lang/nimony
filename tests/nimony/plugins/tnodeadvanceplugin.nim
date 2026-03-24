import std / syncio

template generateEcho(s: string) {.plugin: "deps/mnodeadvanceplugin".}

generateEcho("Hello, world!")

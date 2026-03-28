import std / syncio

template generateEcho(s: string) {.plugin: "deps/mnifsnippet".}

generateEcho("Hello, world!")

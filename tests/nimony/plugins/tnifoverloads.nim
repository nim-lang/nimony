import std / syncio

template generateEcho(s: string) {.plugin: "deps/mnifoverloads".}

generateEcho("Hello, world!")

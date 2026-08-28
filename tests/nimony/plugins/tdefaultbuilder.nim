import std / syncio

template generateEcho(s: string) {.plugin: "deps/mdefaultbuilder".}

generateEcho("default builder")

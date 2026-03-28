import std / syncio

template stressNodeTree(s: string) {.plugin: "deps/mnodetreestress".}

stressNodeTree("payload")

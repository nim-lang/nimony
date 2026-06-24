import std / syncio

template builderCow(s: string) {.plugin: "deps/mbuildercow".}

builderCow("payload")

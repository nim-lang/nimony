import std / syncio

template echoThroughFreshSymbol(value: untyped) {.
    plugin: "deps/mplugin_gensym".}

let generated = "outer"
echoThroughFreshSymbol("generated")
echoThroughFreshSymbol("generated")
echo generated

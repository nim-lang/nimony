import std / syncio

template echoThroughFreshSymbol(value: untyped) {.
    plugin: "deps/mplugin_gensym".}

proc main =
  let tmp = "outer"
  echoThroughFreshSymbol("generated")
  echoThroughFreshSymbol("generated")
  echo tmp

main()

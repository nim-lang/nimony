import std / syncio

template echoThroughFreshSymbol(value: untyped) {.
    plugin: "deps/mplugin_gensym".}

# The same plugin built with `-d:danger`, which compiles assertions out
# (nim-lang/nimony#2973).
template echoThroughDangerSymbol(value: untyped) {.
    plugin: "deps/danger/mplugin_gensym_danger".}

proc main =
  let tmp = "outer"
  echoThroughFreshSymbol("generated")
  echoThroughFreshSymbol("generated")
  echoThroughDangerSymbol("danger")
  echoThroughDangerSymbol("danger")
  echo tmp

main()

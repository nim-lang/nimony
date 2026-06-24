import std / syncio

template sayConst(): untyped {.plugin: "deps/mshared".}
template sayEcho(): untyped {.plugin: "deps/mshared".}

sayConst()
echo Produced
sayEcho()

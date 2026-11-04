import std/syncio

# A `sink` parameter must not give an imported proc a body to destroy it in.
{.build("C", "sinkimportc.c").}
proc sinkTake(s: sink string): cint {.importc: "sinkTake".}

echo sinkTake("hello")

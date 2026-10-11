# `{.intdefine.}` / `{.booldefine.}` / `{.strdefine.}`: the command line may
# override a constant's initializer. The flags live in `nimony.args`.
import std / [syncio]

const
  tdefInt {.intdefine.}: int = 0
  tdefBool {.booldefine.} = false
  tdefStr {.strdefine.} = "unset"
  tdefColon {.intdefine.}: int32 = 0
  tdefOff {.booldefine.} = true
  custom {.intdefine: "my.key".}: int = 0
  # not given on the command line: the written default stands
  untouchedInt {.intdefine.}: int = 5
  untouchedStr {.strdefine.} = "default"
  untouchedBool {.booldefine.} = true

echo tdefInt
echo tdefBool          # a bare `-d:tdefBool` means true
echo tdefStr
echo tdefColon         # `--define:key:value` spelling, and a narrower type
echo tdefOff           # `=false` means false
echo custom            # `{.intdefine: "my.key".}` reads a key of its own
echo untouchedInt
echo untouchedStr
echo untouchedBool
echo defined(tdefInt)  # `-d:key=value` still defines `key`

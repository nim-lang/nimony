
import std / [syncio]

proc myecho(x: string) {.passive.} =
  echo x

proc passiveProc(x: string) {.passive.} =
  for i in 0..<10:
    myecho x & "xyz"

proc main {.passive.} =
  passiveProc("abc")

main()

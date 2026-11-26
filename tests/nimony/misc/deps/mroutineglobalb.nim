when defined(nimony):
  {.feature: "assumeSync".}
import mroutineglobala

proc fromB*(): int =
  var counter {.global.}: int
  inc counter
  counter

proc b1*(): int = counted(10)

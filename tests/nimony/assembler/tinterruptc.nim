# A C build has no vector table to install a handler in. Emitting the function
# anyway compiles and links and is simply never reached — a device that does not
# respond to the interrupt, with nothing at the failure site to say why. So the
# C backend refuses it by name, the way it refuses an `{.assembler.}` body.

proc sysTick {.interrupt: "SysTick".} =
  discard

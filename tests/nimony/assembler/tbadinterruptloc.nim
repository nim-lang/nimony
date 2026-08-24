# Only routines have an address to put in a vector table.

var notARoutine {.interrupt: "SysTick".}: int

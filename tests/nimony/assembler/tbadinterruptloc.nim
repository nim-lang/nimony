# Only routines have an address to put in a interrupt table.

var notARoutine {.interrupt: "SysTick".}: int

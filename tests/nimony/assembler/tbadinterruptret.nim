# A handler returns to the interrupted code, not to a caller: the hardware
# restores the register a result would have been written into.

proc withResult: int {.interrupt: "SysTick".} = 1

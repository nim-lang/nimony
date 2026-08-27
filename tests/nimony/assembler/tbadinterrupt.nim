# `{.interrupt: "NAME".}` — the vector this routine handles. WHICH names a part
# has is a target question, arkham's, exactly as for `{.register.}`. What sem
# owns is the shape, and this pins the three ways to get it wrong.

proc withParam(x: int) {.interrupt: "SysTick".} = discard

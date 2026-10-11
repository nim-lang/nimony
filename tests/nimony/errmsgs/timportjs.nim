# `{.importjs.}` is the JS backend's extern: the argument is a raw JavaScript
# splice template, which only jorogumo can answer. On the C target there is
# nothing to lower it to, and quietly dropping the pragma would emit a call to a
# symbol that is never declared — a link error with no explanation — so lengc
# refuses it by name instead. The other side — the same declaration compiling
# and running — belongs to the JS suite, which needs jorogumo in `bin/` and so
# lands with the `src/nativenif.commit` pin bump.

proc jsLog(s: cstring) {.importjs: "console.log(#)".}
jsLog("hello")

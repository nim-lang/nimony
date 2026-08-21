# A module-qualified name that the module does not have is a hard error: a
# module qualifier has no `a.b(x)` -> `b(a, x)` fallback. Both the call and the
# non-call form used to lose the name on the way out and report
# "undeclared identifier: ''". https://github.com/nim-lang/nimony/issues/2308
import deps/mmodulemember as mm

proc localOnly(): int = 42

discard mm.nosuchproc()
discard mm.nosuchvalue
discard mm.localOnly()

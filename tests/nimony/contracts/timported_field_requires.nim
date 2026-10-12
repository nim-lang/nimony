# The `.requires` of an imported routine that reads a field of a parameter is
# proven against the argument, not the callee's parameter (#2643).
import std / syncio
import deps / mfieldreq

check(State(value: 0))
var s = State(value: 3)
checkVar(s)
checkRange(s)
checkNested(Outer(s: s))
checkTwo(s, 10)
let m = State(value: 4)
check(m)
echo "ok"

# consumer module declares NO closures itself; it only calls through a
# closure-typed field on a foreign object type (pass-2 trigger on the
# consumer side only)
import deps/mwidgethook

let w = makeWidget()
w.cb()

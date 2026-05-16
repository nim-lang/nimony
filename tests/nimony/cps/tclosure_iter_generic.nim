## Generic closure iterators. Two distinct instantiations must each get their
## own coro frame type, init wrapper, and state procs. Bugs prevented this
## previously:
##  - iterinliner.rewriteClosureIter ran on the generic decl, injecting a
##    `result: T` symbol whose typevar T leaked into the coro frame's field
##    type → nifcgen.trField crash loading the typevar as a typedecl.
##  - cps.trCoroutine called patchParamList + generateCoroutineType/Helpers
##    for non-concrete decls, emitting a coro frame for `gen.0` itself with
##    a typevar-typed field.
##  - cps.coroWrapperProc / coroTypeForProc used extractVersionedBasename,
##    which strips the `.I<hash>` segment used to disambiguate cross-module
##    generic instantiations → distinct instances collided on the same
##    wrapper/type/state-proc names. splitSymName preserves the hash.

import std / syncio

iterator gen[T](x, y, z: T): T {.closure.} =
  yield x
  yield y
  yield z

proc main() =
  for v in gen(1, 2, 3):
    echo v
  for s in gen("alpha", "beta", "gamma"):
    echo s

main()

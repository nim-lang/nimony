#
#
#            Nim's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Volatile loads and stores, for memory-mapped I/O.
##
## The interface is Nim's `std/volatile` — same names, same signatures — so
## source written against that module compiles here unchanged. What differs is
## the implementation: Nim emits a C cast through a `volatile*`, while here the
## access IS the intrinsic and the guarantee is stated in its row rather than
## delegated to a C compiler.
##
## What the guarantee is:
##
## * the access HAPPENS, exactly once, wherever it is written. It is never
##   deleted for producing a value nobody reads, never duplicated, and never
##   merged with another access to the same address.
## * it is ONE access, at exactly the pointee's width. A cell too wide for the
##   target's load is refused by name rather than split into two — two accesses
##   is not what was asked for, and for a device register the difference is
##   observable.
## * it is not reordered against another volatile access.
##
## What it is NOT: a barrier, and not ordered against ORDINARY memory. That is
## the same line C draws, and it is drawn there because a peripheral write does
## not flush a store buffer. Where a device needs more, the barrier instructions
## are their own thing.
##
## `volatile` is deliberately not a type qualifier. A qualifier is viral — it
## reaches every type comparison, every signature, every generic instantiation —
## to express a property that belongs to the ACCESS and not to the memory.

proc volatileLoad*[T](src: ptr T): T {.intrinsic: "VolatileLoad".}
  ## One volatile read of `src[]`, at `T`'s width.

proc volatileStore*[T](dest: ptr T; val: T) {.intrinsic: "VolatileStore".}
  ## One volatile write of `val` into `dest[]`, at `T`'s width.

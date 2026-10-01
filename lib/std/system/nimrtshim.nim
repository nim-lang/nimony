# What Nim's `lib/system/arc.nim` provides to its cycle collectors, for the
# copies of them in `system/upstream/` (see `system/orc` and `system/yrc`):
# the cell header, the type descriptor, a few operators and pragmas.
# Included by both, never on its own.
#
# The compiler protocol (what a runtime whose `nimTraceRef` says
# `{.enableTrace.}` gets, `hexer/lifter.runtimeEnablesTrace`):
#
# * A cell carries a second header word after `rc`, `rootIdx` (`RefHeader`).
# * `=destroy` of a `ref T` whose `T` can form a cycle calls `nimDecRefCyclic`
#   instead of `arcDec`, handing it the cell and `T`'s *cell operation*, a
#   compiler-generated `proc (cell, env: pointer)` that traces `T`'s payload
#   (`env != nil`) or destroys the payload and frees the cell (`env == nil`).
#   The cell operation IS the type descriptor (`PNimTypeV2` to Nim's code):
#   it is all the collector ever needs to know about a type. A class that
#   cannot form a cycle passes `nil`.
# * `=trace` of a `ref T` field calls `nimTraceRef` with the field's address
#   and `T`'s cell operation. `=trace` of every other type visits exactly the
#   `ref`s it owns (never `.cursor` fields, never raw pointers unless the type
#   has a hand-written `=trace`, like `seq`).

const
  rcIncrement = 0b10000 # the lowest 4 bits of `rc` are the collector's flags
  rcMask = 0b1111
  rcShift = 4
  nimRcShift* = rcShift ## for `assertions.assertRc`

  orcLeakDetector = false
  traceCollector = false

type
  RefHeader = object
    ## Must match the cell layout `hexer/lengcgen.trRefBody` emits for a
    ## runtime that traces: `(rc, rootIdx, payload)`.
    rc: int
      ## zero based, counted in `rcIncrement`s above the collector's flags
    rootIdx: int64
  Cell = ptr RefHeader

  CellOp* = proc (cell: pointer; env: pointer) {.nimcall.}
    ## Traces the payload of `cell` into `env` (a `ptr GcEnv`), or, with
    ## `env == nil`, destroys the payload and frees the cell.
  PNimTypeV2 = CellOp

{.pragma: compilerRtl.}
{.pragma: inl, inline.}

when not declared(`+%`): # `system/memory` has them with its own allocator
  template `+%`(x, y: int): int = cast[int](cast[uint](x) + cast[uint](y))
  template `-%`(x, y: int): int = cast[int](cast[uint](x) - cast[uint](y))
  template `*%`(x, y: int): int = cast[int](cast[uint](x) * cast[uint](y))

proc head(p: pointer): Cell {.inline.} = cast[Cell](p) # a `ref` points at its header

template compileOption(option: string): bool = false
  # there is no separate shared heap: `alloc` serves every thread

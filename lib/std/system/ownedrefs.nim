# Release of the owning edge of an `owned ref T` (RFC #575, `.feature:
# "ownedRefs"`). Strategy independent: built on the primitives every strategy
# (`include "$MM"`) supplies, after `system/panics` for the diagnostic.

func arcDecOwned*(memLoc: var int): bool {.inline.} =
  ## Releases the owning edge of a cell of acyclic type. True when it was the
  ## last reference.
  ##
  ## The fast path rests on a non-local invariant, so here it is: every counted
  ## reference is derived either from the owned location, whose accesses
  ## happen-before its destruction (or the program races on it), or from
  ## another counted reference, whose contribution is already in the count. So
  ## observing a zero count proves that no other reference exists and none can
  ## appear; there is nothing to adjudicate and no read-modify-write is needed.
  ## Two participants are outside this argument: `.cursor`/`addr`/`cast` (the
  ## pre-existing cursor hazard) and a cycle collector, which mutates the count
  ## without holding a reference. The compiler only emits this call for cells
  ## that cannot form a cycle, which the collector therefore never holds.
  ##
  ## `arcIsUnique` is an ACQUIRE load where the strategy is atomic: the count
  ## may have reached zero via another thread's decrement. The slow path frees
  ## on the value its own read-modify-write returned, never on the load, which
  ## keeps this out of the nim-lang/threading#45 bug class. Do not "simplify"
  ## this into an unconditional `arcDec`, and do not copy it where the
  ## derivation does not hold.
  {.cast(noSideEffect).}:
    if arcIsUnique(memLoc):
      result = true
    else:
      when defined(nimOwnedStrict):
        # Diagnostic only: an unowned reference outlives its owner. Without
        # `nimOwnedStrict` it keeps the object alive, which is safe.
        panic "[FATAL] dangling references exist\n"
      result = arcDec(memLoc)

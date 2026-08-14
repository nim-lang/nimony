# Guards comparing CASTS of nil/true/false literals inside inlinable procs
# must not be constant-folded by literal tag: `cast[pointer](false)` and
# `nil` are both zero yet carry different tags, so a tag-inequality fold
# deletes the wrong arm. The fold may only look through the `(conv T (nil))`
# wrapper the inliner itself splices for substituted nil arguments — casts
# and true/false stay opaque.
import std/syncio

proc peq(): string =
  if cast[pointer](false) == nil: "p-eq true" else: "p-eq false"

proc ieq(): string =
  if cast[int](nil) == cast[int](false): "i-eq true" else: "i-eq false"

proc ine(): string =
  if cast[int](false) != cast[int](nil): "i-ne true" else: "i-ne false"

proc ueq(): string =
  if cast[uint](nil) == cast[uint](false): "u-eq true" else: "u-eq false"

proc main =
  echo peq()
  echo ieq()
  echo ine()
  echo ueq()

main()

# Fake compiler pass with deliberate NIF construction errors.
# Used by check_tags tests to verify that violations are detected.

import std/assertions
include "../../src/lib/nifprelude"
import "../../src/nimony/nimony_model"

proc correctVar(dest: var TokenBuf; info: PackedLineInfo; s: SymId) =
  # (var D E P T .X) — E and P always allow dots, .X allows dot. All correct.
  dest.copyIntoKind VarS, info:
    dest.addSymDef s, info       # D: correct
    dest.addDotToken()           # E: always allows dot
    dest.addDotToken()           # P: always allows dot
    dest.copyIntoKind RefT, info:  # T: correct (type)
      dest.addDotToken()         # BUG: ref expects a type child, not DotToken
    dest.addDotToken()           # .X: correct (dot allowed)

proc wrongChildCount(dest: var TokenBuf; info: PackedLineInfo) =
  # (add T X X) — 3 children required, only 2 given
  dest.copyIntoKind AddX, info:
    dest.addDotToken()  # T
    dest.addDotToken()  # X — missing second X!

proc wrongKind(dest: var TokenBuf; info: PackedLineInfo; s: SymId) =
  # (asgn X X) — expects expressions, not SymDef or DotToken
  dest.copyIntoKind AsgnS, info:
    dest.addSymDef s, info  # BUG: SymDef where expr expected
    dest.addDotToken()      # BUG: DotToken where expr expected (no dot prefix)


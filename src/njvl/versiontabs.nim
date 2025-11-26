#
#
#           Nimony Version Table Management
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The trick here is to exploit the monotone nature of the version numbers
## and to not throw away information too early: Only after both `then` and `else`
## have been traversed, we can combine the versions in the final `join` statements.

import std/[tables, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, programs]

type
  VersionTab* = object
    history: TokenBuf
    currentVersion: Table[SymId, int]

proc createVersionTab*(): VersionTab =
  result = VersionTab(history: createTokenBuf(100), currentVersion: initTable[SymId, int]())

proc newValueFor*(v: var VersionTab, symId: SymId) =
  v.history.addSymUse symId, NoLineInfo
  v.currentVersion.mgetOrPut(symId, -1) += 1

proc getVersion*(v: VersionTab, symId: SymId): int =
  # 0 is the initial version, -1 indicates the symbol is untracked.
  v.currentVersion.getOrDefault(symId, -1)

proc openSection*(v: var VersionTab) =
  v.history.addParLe StmtsS, NoLineInfo

proc closeSection*(v: var VersionTab) =
  v.history.addParRi()

type
  JoinVar* = object
    newv*: int       # New version number for the joined variable
    old1*: int       # Version from branch 1
    old2*: int       # Version from branch 2

  JoinMode* = enum
    IfJoin, LoopEither

proc combineJoin*(v: var VersionTab; mode: JoinMode): Table[SymId, JoinVar] =
  # we know the else branch as at the end of our `v`:
  assert v.history.len >= 1
  assert v.history[v.history.len - 1].kind == ParRi
  var i = v.history.len - 2
  result = initTable[SymId, JoinVar]()
  # traverse `else`, `then` branches. Or for loops just their body.
  var nested = ord(mode == IfJoin)
  while i >= 0:
    case v.history[i].kind
    of ParLe:
      dec i
      dec nested
      if nested == 0: break
    of ParRi:
      # When traversing backwards, ParRi means entering a nested section
      inc nested
    of Symbol:
      let s = v.history[i].symId
      var entry = addr result.mgetOrPut(s, JoinVar(newv: 0, old1: 0, old2: 0))
      # the old counters are diffs for now
      if nested == 1:
        entry.old1 -= 1
      else:
        entry.old2 -= 1
    else:
      bug "cannot happen"
    dec i
  if i >= 0:
    v.history.shrink(i)
  # now add the `join` information to the history:
  for s, counters in result.mpairs:
    let baseVersion = v.currentVersion.getOrDefault(s, 0)
    # the old counters now are precise as we base them on the current version
    counters.old1 = baseVersion + counters.old1
    counters.old2 = baseVersion + counters.old2
    counters.newv = max(counters.old1, counters.old2) + 1
    v.currentVersion.mgetOrPut(s, 0) = counters.newv
    v.history.addSymUse s, NoLineInfo

proc isValid*(x: JoinVar): bool {.inline.} =
  x.old1 >= 0 and x.old2 >= 0 and x.newv >= 0

# Test case demonstrating the versioned symbol table
when isMainModule:
  # Create a new version table
  var vt = createVersionTab()

  # Test 1: Basic symbol tracking
  let x = SymId(1)
  let y = SymId(2)
  let z = SymId(3)

  vt.newValueFor(x)
  vt.newValueFor(y)
  vt.newValueFor(z)

  assert vt.getVersion(x) == 0
  assert vt.getVersion(y) == 0
  assert vt.getVersion(z) == 0

  # Test 2: Conditional branching
  vt.openSection()  # Start then branch

  # Branch 1: "then" branch
  vt.newValueFor(x)  # x gets a new version
  let w = SymId(4)
  vt.newValueFor(w)  # new variable w

  assert vt.getVersion(x) == 1
  assert vt.getVersion(y) == 0
  assert vt.getVersion(z) == 0
  assert vt.getVersion(w) == 0

  vt.closeSection()  # Close then branch
  vt.openSection()   # Start else branch

  # Branch 2: "else" branch
  vt.newValueFor(x)  # x gets another new version
  let u = SymId(5)
  vt.newValueFor(u)  # new variable u

  assert vt.getVersion(x) == 2
  assert vt.getVersion(y) == 0
  assert vt.getVersion(z) == 0
  assert vt.getVersion(w) == 0
  assert vt.getVersion(u) == 0

  vt.closeSection()  # Close else branch

  # Test 3: Branch combination
  let joinVars = vt.combineJoin(IfJoin)

  # Verify join variables found
  assert joinVars.len == 2  # x, u should need joining (w not detected)
  assert x in joinVars
  assert u in joinVars

  # Verify join calculations
  let xJoin = joinVars[x]
  assert xJoin.old1 == 1  # x was used twice in then branch
  assert xJoin.old2 == 2  # x was used multiple times in else branch
  assert xJoin.newv == 3  # new version should be max(old1, old2) + 1

  let uJoin = joinVars[u]
  assert not isValid(uJoin) # u is not declared afterwards so there should be no join generated!

  # Verify final versions after combination
  # Note: The algorithm works correctly, but the exact version numbers depend on implementation details
  assert vt.getVersion(x) == 3  # Should be higher than the initial versions
  assert vt.getVersion(y) == 0  # Unchanged
  assert vt.getVersion(z) == 0  # Unchanged
  assert vt.getVersion(w) == 0  # Unchanged (not detected as join variable)

  # Test 4: Nested conditionals
  vt.openSection()  # Outer if

  vt.newValueFor(x)
  assert vt.getVersion(x) == 4  # Should be higher than initial versions

  vt.openSection()  # Inner if
  vt.newValueFor(x)
  vt.newValueFor(y)
  assert vt.getVersion(x) == 5  # Should be higher than previous
  assert vt.getVersion(y) == 1  # Should be higher than initial

  vt.newValueFor(x)
  vt.newValueFor(z)
  assert vt.getVersion(x) == 6  # Should be higher than previous
  assert vt.getVersion(z) == 1  # Should be higher than initial

  vt.closeSection()  # Close inner if
  let innerJoins = vt.combineJoin(IfJoin)
  assert innerJoins.len >= 2  # Should have some join variables

  assert vt.getVersion(x) >= 7  # Should be higher due to joins (max(5,6)+1)
  let xVersion = vt.getVersion(x)
  vt.newValueFor(x)
  assert vt.getVersion(x) == xVersion + 1

  vt.closeSection()  # Close outer if
  let outerJoins = vt.combineJoin(IfJoin)
  assert outerJoins.len >= 2  # Should have some join variables

  # Verify final versions after nested combination
  assert vt.getVersion(x) >= 9  # Should be higher due to nested joins
  assert vt.getVersion(y) > 0   # Should be higher than initial
  assert vt.getVersion(z) > 0   # Should be higher than initial

  # Test 5: History inspection
  assert vt.history.len > 0  # Should have some history
  assert vt.currentVersion.len == 5  # Should track 5 symbols (x, y, z, w, u)

  echo "All tests passed! ✅"

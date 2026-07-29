# A loop-local `let` combined with `and`/`or` inside an OBJECT-CONSTRUCTOR
# field initialiser silently evaluated to a constant `false`:
#
#   for k in keys:
#     let kk = k
#     result.add Opt(key: kk, selected: sel.len > 0 and kk == sel)   # always false
#
# BOTH INGREDIENTS ARE REQUIRED — drop either and the answer is correct, which
# is what makes this so easy to mis-diagnose. The two negative controls below
# pin that down: `key: k` (loop variable, no `let`) and `selected: kk == sel`
# (no `and`) were both correct while the combination was wrong.
#
# Root cause: `controlflow.nim` models `and`/`or` as the short-circuit control
# flow they are and emits the operands *before* the sibling field reads the
# enclosing constructor has accumulated. xelim, however, used to leave `and`
# alone until the FINAL run — after the duplifier. So the mover saw
# "`kk == sel` first, `key: kk` last", declared the constructor's `key: kk` the
# last read and let the duplifier sink it (`tmp = kk; =wasMoved kk`), while the
# emitted code ran that `=wasMoved` *before* the `and`. The comparison then ran
# on an emptied string. No warning, no error, no crash — just `false`.
#
# `if`/`case` operands never had this problem because they are complex in every
# xelim goal, so they are already statements by the time the duplifier looks;
# the fix is to treat `and`/`or` the same way.
#
# Found 2026-07-29 in weborders `services/location_prefs.roleLocations`, where
# it silently disabled every ordering-location pre-selection.

import std / [syncio, assertions]

type
  Opt = object
    key: string
    selected: bool

proc build(keys: openArray[string]; sel: string): seq[Opt] =
  # THE BUG: loop-local `let` + `and` in a constructor field.
  result = @[]
  for k in keys:
    let kk = k
    result.add Opt(key: kk, selected: sel.len > 0 and kk == sel)

proc buildNoLet(keys: openArray[string]; sel: string): seq[Opt] =
  # Control 1: the loop variable directly, no `let`.
  result = @[]
  for k in keys:
    result.add Opt(key: k, selected: sel.len > 0 and k == sel)

proc buildNoAnd(keys: openArray[string]; sel: string): seq[Opt] =
  # Control 2: a single comparison, no `and`.
  result = @[]
  for k in keys:
    let kk = k
    result.add Opt(key: kk, selected: kk == sel)

proc buildOr(keys: openArray[string]; sel: string): seq[Opt] =
  # The `or` half of the same lowering.
  result = @[]
  for k in keys:
    let kk = k
    result.add Opt(key: kk, selected: sel.len == 0 or kk == sel)

proc render(s: seq[Opt]): string =
  result = ""
  for it in s:
    if result.len > 0: result.add " "
    result.add it.key
    result.add (if it.selected: "+" else: "-")

proc main =
  let keys = ["a", "b", "c"]
  # The moved-into field must arrive intact in every shape, too.
  for s in [build(keys, "b"), buildNoLet(keys, "b"), buildNoAnd(keys, "b"),
            buildOr(keys, "b")]:
    assert s.len == 3
    assert s[0].key == "a" and s[1].key == "b" and s[2].key == "c"
    assert not s[0].selected
    assert s[1].selected
    assert not s[2].selected

  echo "let+and : ", render(build(keys, "b"))
  echo "no-let  : ", render(buildNoLet(keys, "b"))
  echo "no-and  : ", render(buildNoAnd(keys, "b"))
  echo "let+or  : ", render(buildOr(keys, "b"))

main()

# A branch that contains `jmp L … lab L` is STILL that branch.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_inlined_early_return.nim
# Expected (see .output):
#     /A/x
#     true false
#
# An inlined callee with early `return`s lowers to `… (jmp ret) … (lab ret) …`
# *inside* one arm of the caller's `if`. `trackers.gotoLabel` used to CLOSE the
# enclosing sibling, so when `landLabel` revived the path everything written
# after it — here `res = <the callee's result>` — was no longer attributed to
# any branch. `closeBranch` then found the sibling already closed and returned
# without reverting those writes, and they leaked past the whole `if` as if
# they were unconditional. The join with the else arm's `res = false` was
# therefore computed from the else arm alone, and copyprop's literal-snapshot
# tracker substituted that `false` into the following `if res:` — the branch
# was compiled away.
#
# In `strutils` that callee is `continuesWith`, so `multiReplace` replaced
# nothing, so `semos.replaceSubs` never expanded `${path}` and a `-d:release`
# self-host looked for `lib/vendor/mimalloc/...`. Only `--opt:speed` runs
# shoggoth, which is why the plain `tstrutils` coverage never saw it.

import std / [syncio, assertions]

func continuesAt(s, sub: string; start: int): bool =
  # The early `return`s are load-bearing: written as `result = …; break` this
  # lowers without the `jmp`/`lab` pair and was always correct.
  if sub.len == 0: return true
  if sub.len > s.len - start: return false
  for i in 0 ..< sub.len:
    if s[i + start] != sub[i]: return false
  return true

proc expand(s: string; reps: openArray[(string, string)]): string =
  result = ""
  var i = 0
  while i < s.len:
    var hit = false
    for repl in reps.items:
      if repl[0].len > 0 and continuesAt(s, repl[0], i):
        result.add repl[1]
        inc(i, repl[0].len)
        hit = true
        break
    if not hit:
      result.add s[i]
      inc(i)

proc main =
  echo expand("${p}/x", [("${p}", "/A")])
  # Directly: the guarded call's result must survive the join with the `false`
  # the unguarded arm assigns.
  echo continuesAt("abc", "ab", 0), " ", continuesAt("abc", "bc", 0)

main()

import std / syncio

# More arguments than the ABI has argument registers, in the shapes that give a
# parameter a stack HOME rather than a register one.
#
# AArch64 passes the first eight integer/pointer arguments in x0-x7 and the rest on
# the caller's stack. A `string` is two eightbytes, so four of them fill the argument
# registers outright and everything after lands on the stack. That is
# `nimony/deps.wantTool`'s shape — four `string`s then four `var Table`s — and it
# used to abort arkham's AArch64 backend with ">8 integer params (stack TODO)": the
# prologue addressed incoming arguments off SP, before the frame was carved, so it
# could only ever fill a REGISTER home. A stack-passed parameter that is also
# address-taken or has to survive a call gets a `(s)` slot instead, and filling one
# means storing into a slot that does not exist yet at that point in the prologue.
#
# `wantish` below is that shape with the pressure that forces it: every parameter is
# used after a call, so none of them can stay in a volatile register. `scalars` is
# the pointer-free half of the same rule.

type
  Pair = object
    a, b: int

proc note(s: var seq[string]; v: string) = s.add v
proc tally(s: seq[string]): int = s.len

proc wantish(name, src, builder, cachePath: string; p: Pair;
             exe, build, cmd, names: var seq[string]): int =
  note(exe, cachePath & "/" & name)
  note(build, src)
  note(cmd, builder)
  note(names, builder & $tally(names))
  result = tally(exe) + tally(build) + tally(cmd) + tally(names) +
           name.len + src.len + builder.len + cachePath.len + p.a + p.b

proc scalars(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12: int): int =
  result = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12

var exe: seq[string] = @[]
var build: seq[string] = @[]
var cmd: seq[string] = @[]
var names: seq[string] = @[]

echo wantish("nifler", "src/n.nim", "nim", "cache", Pair(a: 3, b: 5), exe, build, cmd, names)
echo exe[0], " ", build[0], " ", cmd[0], " ", names[0]
echo scalars(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

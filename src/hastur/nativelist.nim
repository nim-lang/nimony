## What the native backend (`nimony n`: arkham + nifasm) is RECORDED as
## compiling correctly, and the two questions the rest of hastur asks that
## list: does `native.nim`'s own suite run this test, and does the Windows
## tree walk compile it with `nimony n` instead of `nimony c`?
##
## Its own module because those two callers sit on opposite sides of the
## layering — `compile`/`runner`/`walk` on one, `native` (via `builders`) on
## the other — and the list has to be the same list for both, or the walk
## would compile natively what the native suite never vouched for.

import std / strutils
import category
when defined(windows):
  # Only the Windows branch of `walkUsesNative` looks at the file system (a
  # test's sidecars) or at the toolchain (arkham, nifasm); importing these
  # unconditionally would be an unused import on every other host.
  import std / os
  import context

# ── native-backend regression set ────────────────────────────────────────────
# The C-FREE native path (`nimony n` → arkham + nifasm, sibling `../nativenif`) is
# still incomplete, so we can't run the whole suite through it. Instead this is an
# explicit whitelist of what is known to run correctly natively — a regression
# guard: a native miscompile that diverges from the (spec-pinned) result is caught.
# Grow it as the backend gains features (the same record-what-works philosophy as
# the rest of hastur). `NativeTestDirs` are directories that pass apart from the
# files `NativeTestSkip` names (negative `.msgs` tests auto-skipped too);
# `NativeTestFiles` are individual passers from otherwise-partial directories.
const
  NativeTestDirs* = [
    "tests/nimony/arc",        # full ARC suite — byte-identical to the C backend
    "tests/nimony/closures"
  ]

  NativeTestDirsWindows* = [
    # Windows grew a native target later than the other hosts, so what it is known
    # to run correctly is recorded separately until a run elsewhere confirms the
    # same — the whole point of this file is that it states OBSERVED results, and
    # these were observed on Windows. Fold an entry into `NativeTestDirs` once it
    # has been seen green on Linux/macOS too.
    #
    # Every entry below was established the same way, under Wine against the real
    # MinGW bundle (`tools/wine_test.sh`, see `doc/wine_testing.md`): each of the
    # directory's tests run through `nimony n` on its own AND the whole directory
    # run as one native joined program, which is the shape the tree walk actually
    # compiles it in and NOT the same question: five of these directories passed
    # test-by-test and failed as a group, on three backend bugs that only a
    # module compiled as an import can reach (see `NativeJoinSkip`).
    #
    # `tests/nimony/stdlib`: 45 of its 53 pass; the rest are in `NativeTestSkip`.
    # This is the suite that covers the process vectors the Windows entry point
    # does not receive (`tcmdline`, `tenvvars`, `tos`) — the reason it is worth
    # running natively at all rather than only through the C backend.
    "tests/nimony/stdlib",
    "tests/nimony/assembler",
    "tests/nimony/basics",
    "tests/nimony/borrows",
    "tests/nimony/casestmt",
    "tests/nimony/consteval",
    "tests/nimony/converter",
    "tests/nimony/cyclic",
    "tests/nimony/enums",
    "tests/nimony/exceptions",
    "tests/nimony/intrinsics",
    "tests/nimony/lastuse",
    "tests/nimony/lookups",
    "tests/nimony/macros",
    "tests/nimony/mut",
    "tests/nimony/overload",
    "tests/nimony/plugins",
    "tests/nimony/sets",
    "tests/nimony/strings",
    "tests/nimony/templates",
    "tests/nimony/threads",
    "tests/nimony/types",
    "tests/nimony/untyped",
    "tests/nimony/when"
  ]

  NativeJoinSkip*: array[0, string] = [
    # Directories whose tests each compile and run correctly through `nimony n`
    # — they are in the lists above — but whose JOINED program does not, so the
    # group keeps the C backend while the same directory's standalone tests
    # stay native.
    #
    # Empty, and worth keeping empty: the tree walk compiles a group's members
    # as IMPORTS of a generated driver, and no other runner does — `hastur
    # native` compiles every test as a MAIN module — so this list is the only
    # place a native gap in that shape can be recorded. Five directories were
    # here when it was written (arc, strings, consteval, templates, untyped)
    # and all five were three backend bugs, not five: a peephole that answered
    # its liveness question across proc boundaries, a template-gensym'd routine
    # named with local layout, and a `dynlib` that only one backend wrote into
    # the shared `.x.nif`. An entry belongs here only while such a bug is open.
  ]

  NativeTestSkip* = [
    # Files a `NativeTestDir` does not cover yet, with the reason each fails. All
    # but the last PASS through the C backend, so each of those is a gap in the
    # native path rather than in the test — which is what makes them worth naming
    # individually instead of dropping the directory.
    #
    # A libc math/stdio extern with a FLOAT parameter. Win64 indexes its SSE
    # argument registers positionally (an xmm slot burns the GPR of the same
    # position), which arkham's ABI planner does not model — `emitWinExtproc`
    # rejects it rather than guess. Moot until the freestanding target has a libc
    # to bind these to at all.
    "tests/nimony/stdlib/tcomplex",     # sqrtf
    "tests/nimony/stdlib/tmath",        # frexp
    "tests/nimony/stdlib/tstrutils",    # snprintf
    # `mov` of a `bool` into an `(i 64)` result: nifasm's widening rule admits
    # int→int only, so a bool source is a type error. Arch-neutral.
    "tests/nimony/stdlib/thashes",
    # Compiles and links, but the result diverges from the spec-pinned output —
    # a native MISCOMPILE (all three are green on the C backend). Undiagnosed.
    "tests/nimony/stdlib/tbitops",
    "tests/nimony/stdlib/tencodings",
    "tests/nimony/stdlib/trandom",
    # The odd one out: NOT a native gap. Its `std/typetraits` compile-time plugin
    # fails to run ("vfs: open failed"), and the C backend fails it identically,
    # so nothing here would fix it.
    "tests/nimony/stdlib/ttypetraits"
  ]
  NativeTestFiles* = [
    # Portable intrinsics: `(instr …)` lowers to the target's own instruction —
    # x86-64 `bsf`, AArch64 `rbit`+`clz` — so this is the one test whose POINT is
    # that the C and native backends agree on results they reach by different
    # instructions.
    "tests/nimony/intrinsics/tintrinsics",
    # The atomics are intrinsic rows too, and the backends reach them from even
    # further apart: C emits the `__atomic_*` builtins, x86-64 a `lock`-prefixed
    # sequence, AArch64 an `ldaxr`/`stlxr` retry loop. Running it here is what says
    # the three agree — including the compare-exchange FAILURE path, which a wrong
    # lowering still "passes" single-threaded unless the test reads back the value
    # the CAS observed.
    "tests/nimony/intrinsics/tatomics",
    # The valgrind client request. Native-only by nature: under the C backend the
    # mechanism is valgrind's own headers around mimalloc, so there is no row to
    # lower and the test compiles to its `echo` alone. What it checks here is the
    # property the C path never has to have — that the request sequence is inert
    # on real hardware, and leaves every live register where it found it.
    "tests/nimony/intrinsics/tvalgrind",
    # `div`/`mod` by a constant power of two, which arkham strength-reduces to a
    # shift (and, for a signed dividend, a round-toward-zero bias). Native-relevant
    # by nature: under `nimony c` the C compiler owns that rewrite, so only a
    # backend doing its own can round the wrong way on a negative dividend.
    "tests/nimony/basicarith/tdivmodpow2",
    # `float32` at every width-sensitive spot. Native-relevant by nature: the C
    # backend gets the widths from C's own type system, so this only ever fails on
    # a backend that has to derive them itself.
    "tests/nimony/types/tfloat32",
    # Indexing an array of AGGREGATES and reading one field of the element: the
    # element stride and the access width are different numbers, which AArch64's
    # register-offset addressing cannot express (`[Xn, Xm, LSL #k]` scales by the
    # ACCESS width, not by an arbitrary amount). Native-relevant by nature — the C
    # compiler owns addressing under `nimony c`. It read `arr[i div 2]` for a 4-byte
    # field, which is what kept `-d:release` off the native self-host.
    "tests/nimony/calls/tindexednarrowfield",
    # More arguments than the ABI has argument registers, with the parameter shapes
    # that give a stack-passed one a stack HOME (a `var seq` written through, an
    # object read after a call). Native-relevant by nature: the C compiler owns
    # argument passing under `nimony c`, so only a backend that marshals arguments
    # itself can fail this — arkham's AArch64 prologue used to abort on it outright
    # (">8 integer params (stack TODO)"), which is what kept `nimony.nim` off the
    # native bootstrap ladder.
    "tests/nimony/calls/tstackparams",
    # cps/* — closures & continuation-passing (indirect calls through fn-ptr values)
    "tests/nimony/cps/tbasicpassive",
    "tests/nimony/cps/tclosure",
    "tests/nimony/cps/tclosure_iter_basic",
    "tests/nimony/cps/tclosure_iter_body_capture",
    "tests/nimony/cps/tclosure_iter_break",
    "tests/nimony/cps/tclosure_iter_envcheck",
    # One frame field of every kind. Native-relevant by nature: this is the
    # backend that depends on `oconstr` being total, since it stores exactly the
    # fields the constructor lists and zeroes nothing.
    "tests/nimony/cps/tclosure_iter_frametypes",
    # Sets in a closure iterator, in both representations (word-sized and the
    # 32-byte array). Worth running natively because the big-set path builds
    # its value through `zeroMem` plus per-element stores rather than a
    # literal, which is a different shape for the back end than the C one.
    "tests/nimony/cps/tclosure_iter_sets",
    "tests/nimony/cps/tclosure_iter_string",
    "tests/nimony/cps/tclosure_iter_var",
    "tests/nimony/cps/tfirstpassive",
    "tests/nimony/cps/tif",
    "tests/nimony/cps/tmethods",
    "tests/nimony/cps/tnestedloops",
    "tests/nimony/cps/trecursive",
    "tests/nimony/cps/tsuspend",
    "tests/nimony/cps/tsuspend_resume",
    "tests/nimony/cps/tparkstate",
    "tests/nimony/cps/ttry",
    # The native backend is the only one that HAS a stack trace: it walks nifasm's
    # own per-proc table, seeded by a `{.naked.}` proc. Running it here is what
    # checks the three halves agree — the table nifasm lays down, the two
    # intrinsics arkham lowers, and the walk in `lib/std/stacktraces`.
    "tests/nimony/stacktraces/tstacktrace",
    # A `const` set is read straight out of read-only data, so its membership
    # test is an indexed load from a GLOBAL — a shape the C backend never sees a
    # register problem in and arkham got wrong twice. Native-only by nature.
    "tests/nimony/sets/tconstsetscan",
    # The one test here whose point is WHERE its code is compiled rather than
    # what it does: everything it checks lives in `deps/mimportshapes`, so this
    # is the suite's only native compile of a non-main module. Every other test
    # arrives as `--isMain`, and three bugs lived in that gap — see the test.
    "tests/nimony/modules/timportshapes"
  ]

# ── native compilation inside the tree walk ──────────────────────────────────
# What a Windows test costs is mostly not nimony: it is the gcc invocation per
# module and the link that follow it, each a process creation the platform
# charges dearly for (see the `joined tests` note below — same cause, different
# lever). `nimony n` has no such tail: arkham and nifasm consume the same front
# end and write the PE themselves. So for a test the native backend is KNOWN to
# compile correctly, the walk takes that path on Windows and pays a fraction of
# the time, asserting exactly what it asserted before — the same program's
# output against the same `.output` file, only the code generator differs.
#
# "Known to compile correctly" is not a new list: it is the `hastur native`
# whitelist directly above, read the same way. A test the native suite does not
# vouch for keeps the C backend, so this can never turn a native gap into a
# tree-walk failure that `hastur native` would not already report.

var walkNative* = true
  ## `--native:off` puts the whole tree walk back on the C backend. Only ever
  ## consulted on Windows (elsewhere `walkUsesNative` is false regardless): the
  ## other hosts run their C toolchain fast enough that the native path would
  ## trade coverage for nothing.

proc nativeWhitelisted(file: string): bool =
  ## Is `file` one of the tests `hastur native` runs — i.e. one the native
  ## backend is recorded as compiling correctly on this host?
  result = false
  let p = file.replace('\\', '/')
  if not p.endsWith(".nim"): return false
  let stem = p[0 ..< p.len - ".nim".len]
  for f in NativeTestSkip:
    if stem == f: return false
  for f in NativeTestFiles:
    if stem == f: return true
  # A whitelisted directory covers its own `.nim` files only; the nested dirs
  # (`deps/`, `imp/`, …) hold import fixtures, which are never tests. Sliced by
  # hand rather than with `parentDir`, which re-spells the result with the host
  # separator — a backslash on the one platform this runs on, and so never
  # equal to a list entry.
  let sep = stem.rfind('/')
  if sep < 0: return false
  let dir = stem[0 ..< sep]
  for d in NativeTestDirs:
    if dir == d: return true
  when defined(windows):
    for d in NativeTestDirsWindows:
      if dir == d: return true

proc nativeJoinable*(dir: string): bool =
  ## May this directory's joined group be compiled natively? Separate from
  ## `walkUsesNative` because it is a different question: a group compiles its
  ## members as imports, and `NativeJoinSkip` records where the native backend
  ## gets that shape wrong for tests it gets right on their own.
  let d = dir.replace('\\', '/').strip(chars = {'/'})
  for x in NativeJoinSkip:
    if d == x: return false
  result = true

proc walkUsesNative*(file: string; cat: Category): bool =
  ## Whether the tree walk compiles this test with `nimony n` rather than
  ## `nimony c`. Windows-only, and only for a plain compile-run-diff test:
  ## a `.msgs` spec, a golden `.nim.c`, a `.nif` dump or a valgrind run each
  ## pin the test to the C pipeline, and every non-`Normal` category carries
  ## flags (`--noSystem`, `--compat`, `--opt:speed`) the native path does not
  ## take.
  when not defined(windows):
    result = false
  else:
    if not walkNative or cat != Normal: return false
    # A plain clone of this repo has no `../nativenif`, so `build all` skips
    # arkham and nifasm and says so rather than failing; the walk has to make
    # the same allowance or every whitelisted test would die on a missing tool.
    # Safe to ask here: `tests/setup.hastur` builds the toolchain on the way
    # down, before any leaf directory is planned.
    if not (fileExists(toolExe("arkham")) and fileExists(toolExe("nifasm"))):
      return false
    for ext in [".msgs", ".nim.c", ".nif", ".valgrind"]:
      if file.changeFileExt(ext).fileExists(): return false
    result = nativeWhitelisted(file)

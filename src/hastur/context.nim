## Hastur's shared configuration: the toolchain directory the tools are
## resolved from, the run-wide flags the command line sets, and the two `exec`
## helpers everything else builds its subprocesses with.

import std / [syncio, os, osproc, strutils]

proc quitWithText*(s: string) =
  stdout.write(s)
  stdout.flushFile()
  quit(0)

proc error*(msg: string) =
  when defined(debug):
    writeStackTrace()
  stdout.write("[Error] ")
  stdout.write msg
  stdout.write "\n"
  quit 1

const
  ErrorKeyword* = "Error:"

  targetIs64bit* = sizeof(int) == 8
    ## The checked-in golden `.nim.c` and `.nif` outputs are generated for a
    ## 64-bit target (e.g. `(i +64)`, `IL64(...)`, `NIM_INTBITS 64`). nimony
    ## defaults its target word size to the host CPU, so on a 32-bit host the
    ## generated code legitimately differs and those diffs would spuriously
    ## fail. Per nim-lang/nimony#1569, only run the golden-file comparisons on
    ## 64-bit hosts. (`sizeof(int)` reflects the host hastur was built for,
    ## which is the same machine that runs nimony for the tests.)

# ---- toolchain resolution -------------------------------------------------

proc defaultToolchainDir(): string =
  ## Where hastur looks for its sibling toolchain binaries by default: the
  ## directory hastur itself lives in. `tester.nim` builds hastur into `bin/`
  ## alongside `nimony`, `lengc`, `hexer`, … so a `bin/hastur` invocation
  ## finds them regardless of the current working directory — no `--bindir`
  ## and no "run from the repo root" requirement. When hastur's own directory
  ## has no toolchain (e.g. a `nim c -r` run whose binary sits in a nimcache
  ## temp dir) fall back to the cwd-relative `bin/`.
  result = getAppFilename().parentDir
  if not fileExists(result / "nimony".addFileExt(ExeExt)):
    result = "bin"

var toolchainDir* = defaultToolchainDir()
  ## Directory the toolchain binaries (`nimony`, `lengc`, `hexer`, …) are
  ## resolved from. Defaults to hastur's own directory (its siblings);
  ## `--bindir:PATH` overrides it to point at any prebuilt/installed
  ## toolchain, and binaries missing there are looked up on `$PATH`.

proc toolExe*(name: string): string =
  ## Resolve a toolchain binary: `toolchainDir/<name>` if present, otherwise
  ## `<name>` on `$PATH`. The `$PATH` fallback lets an installed toolchain
  ## drive the tests. When neither exists we still return the `toolchainDir`
  ## path so the ensuing "not found" failure names the expected location.
  result = toolchainDir / name.addFileExt(ExeExt)
  if fileExists(result): return
  let onPath = findExe(name)
  if onPath.len > 0: result = onPath

proc binDir*(): string =
  ## Where a build writes its binary. The same directory the tools are
  ## resolved from, so `hastur build …` and `--bindir:PATH` cannot disagree
  ## about which toolchain a run is about.
  result = toolchainDir

# ---- subprocess helpers ---------------------------------------------------

proc execLocal*(exe, cmd: string): (string, int) =
  ## Run a toolchain binary, capturing its output. `exe` is a bare tool name;
  ## `toolExe` decides which copy of it runs.
  result = osproc.execCmdEx(toolExe(exe).quoteShell & " " & cmd)

# ---- run-wide flags, set by the command line ------------------------------

var nimcacheDir* = "nimcache"
  ## Directory used for compiler intermediates. Per-test parallel runs
  ## point this at a unique sub-directory so concurrent tests don't
  ## race on the same `nimcache/` artifacts.

var parallelJobs* = 1
  ## How many tests `testDir` runs concurrently. 1 = serial (current
  ## behavior). `--jobs:N` on the command line overrides; `--jobs:auto`
  ## uses `countProcessors()`.

var skipBuild* = false
  ## Set by the parallel test runner on its worker invocations: the
  ## parent has already rebuilt nimony / lengc before kicking off the
  ## pool, so each worker skips the rebuild. Otherwise every worker
  ## spends seconds re-running `nim c` for nothing.

proc normalizeDirKey*(p: string): string =
  ## Compare directories by a single spelling: the tree walk builds paths with
  ## the host separator (`tests\boot` on Windows), while a `--skip:` on the
  ## command line is written portably (`tests/boot`).
  result = p.replace('\\', '/').strip(chars = {'/'})

var skipDirs*: seq[string] = @[]
  ## Directories the tree walk leaves out, from `--skip:<dir>` (repeatable).
  ## Unlike `hastur.mode = skip`, this is a property of *this run*, not of the
  ## suite: it exists so CI can split one sweep across runners (`--skip:tests/boot`
  ## on the job that tests, `hastur tests/boot` on the job that boots) without
  ## changing what a plain local `hastur all` covers.

let hasValgrind* = findExe("valgrind").len > 0
  ## Whether the `valgrind` binary (and, by extension, its dev headers) is
  ## available. mimalloc no longer hard-depends on valgrind, so the suite must
  ## run without it: when absent we neither pass `-DMI_TRACK_VALGRIND=1` (which
  ## would need `<valgrind/valgrind.h>` to compile) nor run the leak checks —
  ## the `.valgrind` tests simply skip rather than failing the whole run.

proc exec*(cmd: string; showProgress = false) =
  if showProgress:
    let exitCode = execShellCmd(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n"
  else:
    let (s, exitCode) = execCmdEx(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n" & s

proc exec*(exe, cmd: string) =
  let (s, exitCode) = execLocal(exe, cmd)
  if exitCode != 0:
    quit "FAILURE " & cmd & "\n" & s

# ---- how the toolchain (and `boot`'s stages) get built --------------------

var bootRelease* = true
  ## `boot` compiles the bootstrapped toolchain with `-d:release` by default:
  ## it has proven to be the wider test. It implies `--opt:speed` (so the
  ## shoggoth tree optimizer and the inter-module inliner run) *and* defines
  ## `release`, which exercises the `when defined(release)` paths in nimony,
  ## nimsem and hexer themselves — a whole class of bugs a debug boot cannot
  ## reach. `--no-release` boots at the default opt level instead.
var bootNative* = false
  ## Compile the stages with the C-free native backend (`nimony n` → arkham +
  ## nifasm)? Decided by `useNativeBoot` at the start of `bootCmd`.

type BootBackend* = enum
  bbAuto,   ## whatever `useNativeBoot` says for this host — the default
  bbC,      ## force `nimony c`
  bbNative  ## force `nimony n`, and fail loudly when it is not available

var bootBackend* = bbAuto
  ## `--boot-backend:`. Exists so the two paths can be MEASURED against each
  ## other on a host where the native one is automatic: without it, timing the
  ## C boot on linux/amd64 means hiding `bin/arkham` from hastur.
var debugBuild* = false
  ## `--debug`: build the front-end tools unoptimized (see `nimcPrefix`).
var nativeToolsDebug* = false
  ## `--native-debug`: build arkham + nifasm unoptimized (see `nativeToolPrefix`).

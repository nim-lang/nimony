## The deterministic self-host bootstrap (`bin0/` -> `bin1/` -> ...) and the
## `selfcheck` sequence built on top of it.

import std / [syncio, os, osproc, strutils, times]
import context, deps, builders, tiers

# ---- deterministic self-host bootstrap ------------------------------------
# `bin0/` is a fresh copy of the host-Nim-built toolchain in `bin/`; `binN/`
# (N >= 1) is `binN-1/`'s nimony recompiling all three self-tools from
# source. Each pass produces a sibling `binN/` directory next to `lib/` and
# nothing is fed back into `bin/` — `bin/` remains the host-Nim-built
# toolchain the rest of hastur drives the test suite with.

## The boot bootstrap rebuilds the *full* toolchain at every stage — not just
## `bin/nimony`. nimony is only the driver; the heavy lifting (semantic
## analysis, hexer lowering) happens in `nimsem` and `hexer`, both reached
## via `findTool` from each `nimony c` invocation. Iterating only over the
## driver therefore exercises self-hosting of nimony alone while the
## stage-0 nimsem and hexer keep doing all the real work — bugs in their
## codegen go undetected indefinitely. `tooldirs.binDir` and
## `semos.nimonyDir` accept any tail starting with `bin`, so stage-N's
## nimony resolves stage-N's tools and the project's stdlib without any
## CLI override.
const BootSelfTools = ["nimsem", "hexer", "nimony"]
  ## Tools rebuilt from source at every stage. The order matters: nimsem and
  ## hexer are needed by every later `nimony c` call, so they go first;
  ## nimony itself goes last because it's the one each *next* stage will
  ## drive with.
const BootCarryTools = ["nifler", "lengc", "niflink", "nifmake", "validator", "shoggoth"]
  ## Tools copied from `bin/` into each stage dir. They're tier-0 for
  ## bootstrap purposes (host-Nim-built throughout) but `nimony c` shells
  ## to them, so each stage dir needs its own copy.

proc bootCarryTools*(): seq[string] =
  result = @BootCarryTools
  if bootNative: result.add BootNativeTools

const NativeBootReady = true
  ## Requires a nativenif checkout whose arkham holds each emit step's demand
  ## inside the transient-register budget (the `semBodyCheckBody` staging
  ## exhaustion: binFold's pick-before-premat, emitCondValue2's early result
  ## hold, and a staging pick blind to a free temp pool). When stage 1 dies
  ## with "no staging register available", rebuild arkham/nifasm from a
  ## nativenif master that carries those fixes — or flip this back to `false`
  ## and the C-backend boot below is the whole self-host gate meanwhile.

proc useNativeBoot*(): bool =
  ## linux/amd64 is the platform the native backend is complete on: x86-64
  ## codegen plus the Linux syscall table the libc-free stdlib runs on. Its
  ## tools come from the sibling `../nativenif` checkout, which `build all`
  ## builds when it is there — so fall back to the C backend when they're
  ## missing, and say so (`bootBackendLine`) rather than let a missing sibling
  ## look like a slow C boot.
  ##
  ## linux/arm64 is IN, as of 2026-08-22. `hastur tiers native --forward:-d:release`
  ## is 27/27 and `--boot-backend:native` reaches a byte-identical
  ## stage1==stage2==stage3 fixed point in ~405s, against ~650s through the C backend.
  ##
  ## What had held it back was never the syscall/ABI layer. In order of discovery, and
  ## each one attributed by the tier ladder rather than by staring at a boot stage:
  ## arkham's ">8 integer params (stack TODO)"; a stack-marshalled by-value aggregate
  ## whose home is a register PAIR, which has no address to marshal from; a spilled
  ## `nil` declared `(s) (nil)` where the slot is storage and wants `(ptr void)`; a
  ## word-sized set constant `desugar` emitted unsuffixed, so `xelim` typed its temp
  ## `u64` and the native backend rejected the narrowing the C one performs silently;
  ## the emitter's last-resort register draws handing out live PARAMETER homes, which
  ## are never `rb`-bound and so were invisible to the liveness test they use; and
  ## finally nifasm encoding an element STRIDE into AArch64's register-offset scale
  ## bit, which can only mean "the access width" — `(dot (at arrayOfTuples i) fld)`
  ## read `arr[i div 2]`.
  ##
  ## Only the last two needed `-d:release`, and only because shoggoth's inter-module
  ## inliner supplies the register pressure that reaches them; neither was the
  ## inliner's fault. `SHOGGOTH_DISABLE=<pass,…>` is what separated those questions.
  ##
  ## windows/amd64 is in as of #2325: arkham's win_x64 target emits a PE that
  ## imports what it needs per dll, so a native boot there needs no MinGW and no
  ## libc — which is what makes the Windows job the slowest in the matrix (601s
  ## of stages against 61s natively).
  when defined(linux) and (defined(amd64) or defined(arm64)):
    result = NativeBootReady and missingNativeTools().len == 0
  elif defined(windows) and defined(amd64):
    result = NativeBootReady and missingNativeTools().len == 0
  else:
    result = false

proc bootBackendLine*(withValgrind: bool): string =
  ## One line naming the backend `boot` is about to use, and — when it is not the
  ## native one — why. The fallback used to be silent, which reads as "the native
  ## path is enabled and slow" instead of "the native path was never reached":
  ## #2325 turned windows/amd64 on and the boot went on emitting `nimony c`
  ## because nothing had built arkham into `bin/`.
  if bootNative:
    return "[boot] backend: native (`nimony n`: arkham + nifasm)" &
           (if bootBackend == bbNative: " — forced by --boot-backend:native" else: "")
  result = "[boot] backend: C (`nimony c`)"
  if bootBackend == bbC:
    return result & " — forced by --boot-backend:c"
  when (defined(linux) and (defined(amd64) or defined(arm64))) or
       (defined(windows) and defined(amd64)):
    if withValgrind:
      result.add " — --valgrind cannot see the native heap"
    elif not NativeBootReady:
      result.add " — NativeBootReady is off"
    else:
      let missing = missingNativeTools()
      if missing.len > 0:
        result.add " — " & missing.join(", ") & " missing from " & binDir() &
                   "; `hastur build all` builds them when " & NativenifDir &
                   " is checked out"
  else:
    result.add " — no native backend target for this host"

proc bootSourceFor*(tool: string): string =
  case tool
  of "nimony", "nimsem": "src/nimony/" & tool & ".nim"
  of "hexer": "src/hexer/hexer.nim"
  else: quit "boot: no source mapping for tool " & tool

proc bootStageDir*(stage: int): string =
  ## Every stage lives in its own `binN/` directory next to `lib/`. `bin0/`
  ## is a fresh copy of the host-Nim-built `bin/` (provisioned by
  ## `provisionStageZero`); `binN/` (N>=1) is built by
  ## `binN-1/nimony`.
  "bin" & $stage

proc carryAuxTool*(stageBin, name: string) =
  let exe = name.addFileExt(ExeExt)
  let src = binDir() / exe
  let dst = stageBin / exe
  if not fileExists(src):
    quit "boot: " & src & " not found; run `hastur build " & name &
         "` (or `hastur build all`) first"
  if fileExists(dst): removeFile(dst)
  copyFile(src, dst)
  when defined(posix):
    inclFilePermissions(dst, {fpUserExec, fpGroupExec, fpOthersExec})

proc provisionStageBin*(stage: int): string =
  ## Create `binN/` (clean) and populate it with the tools that don't get
  ## rebuilt per stage. The self-rebuilt tools are dropped in afterwards by
  ## `compileBootTool`.
  result = bootStageDir(stage)
  removeDir result
  createDir result
  for aux in bootCarryTools():
    carryAuxTool(result, aux)

proc provisionStageZero*(): string =
  ## Seed `bin0/` with a fresh copy of the host-Nim-built toolchain in
  ## `bin/`. Unlike later stages, the self-tools aren't recompiled here —
  ## they're copied across so that `bin1/` has a complete driver to start
  ## from.
  result = bootStageDir(0)
  removeDir result
  createDir result
  for aux in bootCarryTools():
    carryAuxTool(result, aux)
  for tool in BootSelfTools:
    carryAuxTool(result, tool)

proc bootToolCmd*(compiler, source, outBin, cacheBase, args: string;
                 withValgrind: bool): string =
  ## The `compiler <c|n> --out:outBin source` command line for one boot tool.
  ## `compiler` is the previous stage's nimony, so it transitively drives the
  ## previous stage's nimsem/hexer (siblings under the same stage's bin dir)
  ## for this build. The subcommand is `n` for a native boot (arkham + nifasm,
  ## no C compiler), `c` otherwise.
  let cache = cacheBase / outBin.extractFilename
  removeDir cache
  createDir cache
  if fileExists(outBin): removeFile(outBin)
  result = compiler.quoteShell & (if bootNative: " n" else: " c") &
           " --silentMake --nimcache:" &
           cache.quoteShell & " --out:" & outBin.quoteShell
  if withValgrind:
    result.add " --passC:\"-DMI_TRACK_VALGRIND=1\""
  if args.len > 0:
    result.add ' '
    result.add args
  result.add ' '
  result.add source.quoteShell

proc compileBootStage*(stage: int; cacheBase, args: string; withValgrind: bool):
                     string =
  ## Build stage `stage` of the toolchain. Returns the stage's bin
  ## directory. Driver of stage N is the stage-(N-1) nimony.
  ##
  ## The stage's three tools (nimsem, hexer, nimony) are compiled CONCURRENTLY.
  ## They are independent: each gets its own `--nimcache` and its own `--out`,
  ## and the only shared input — the previous stage's `bin/` — is read-only for
  ## the duration. Serially this was the single longest stretch of the Windows
  ## CI run (9 compiles, ~600s, all of it one core at a time while nifmake's
  ## `-j` had nothing left to overlap with across the tool boundary).
  let prev = bootStageDir(stage - 1)
  let prevNimony = prev / "nimony".addFileExt(ExeExt)
  if not fileExists(prevNimony):
    quit "boot: " & prevNimony & " not found (stage " & $(stage - 1) &
         " missing)"
  result = provisionStageBin(stage)

  var outBins: seq[string] = @[]
  var cmds: seq[string] = @[]
  for tool in BootSelfTools:
    let outBin = result / tool.addFileExt(ExeExt)
    outBins.add outBin
    cmds.add bootToolCmd(prevNimony, bootSourceFor(tool), outBin, cacheBase,
                         args, withValgrind)
    echo "[boot] stage ", stage, ": ", cmds[^1]

  let t0 = epochTime()
  var procs: seq[Process] = @[]
  for cmd in cmds:
    # `poEvalCommand` so the already-quoted command line goes through the
    # shell exactly as `execShellCmd` ran it. Output is inherited: the tools
    # are quiet under `--silentMake`, and interleaved progress from three
    # compiles is still more useful than buffering it all to the end.
    procs.add startProcess(cmd, options = {poEvalCommand, poParentStreams})
  var failed = ""
  for i in 0 ..< procs.len:
    let exitCode = waitForExit(procs[i])
    close procs[i]
    if exitCode != 0 and failed.len == 0:
      failed = outBins[i].extractFilename
  let dt = epochTime() - t0
  if failed.len > 0:
    quit "FAILURE: boot stage " & $stage & " (" & failed &
         ") failed after " & formatFloat(dt, ffDecimal, precision=2) & "s"
  for outBin in outBins:
    if not fileExists(outBin):
      quit "FAILURE: boot stage " & $stage & ": " & outBin &
           " was not produced (did `--out` get rejected?)"
  echo "[boot] stage ", stage, " produced ", BootSelfTools.join(", "), " in ",
       formatFloat(dt, ffDecimal, precision=2), "s"

const HeaderSkipBytes = 4096
  ## Bytes at the start of an executable to skip during stage-comparison.
  ## Big enough to cover the volatile header regions of all three common
  ## formats: ELF (.note.gnu.build-id is at ~1 KB), Mach-O (LC_UUID lives
  ## inside the load commands), PE (DOS header + COFF header with
  ## TimeDateStamp). 4 KB also matches the typical page size so the cut
  ## tends to fall on a section boundary.

proc maskBuildStamps*(buf: var string) =
  ## Zero out byte regions whose contents depend on *when* the binary was
  ## linked, not *what* the toolchain produced. Two stages whose code is
  ## identical otherwise still compare equal.
  ##
  ## Approach: skip the first `HeaderSkipBytes` (covers ELF build-id,
  ## Mach-O LC_UUID, PE COFF TimeDateStamp — all live in headers) and
  ## additionally mask `HH:MM:SS` / `Mmm DD YYYY` ASCII strings anywhere
  ## else (mimalloc bakes `__DATE__` / `__TIME__` into .rodata via
  ## `vendor/mimalloc/src/options.c`).
  let skip = min(HeaderSkipBytes, buf.len)
  for k in 0 ..< skip: buf[k] = '\0'

  proc isDigit(c: char): bool {.inline.} = c >= '0' and c <= '9'

  # HH:MM:SS — exact 8 bytes.
  var i = skip
  while i + 8 <= buf.len:
    if buf[i+2] == ':' and buf[i+5] == ':' and
       isDigit(buf[i]) and isDigit(buf[i+1]) and
       isDigit(buf[i+3]) and isDigit(buf[i+4]) and
       isDigit(buf[i+6]) and isDigit(buf[i+7]):
      for k in i ..< i + 8: buf[k] = '\0'
      i += 8
    else:
      inc i

  # `Mmm DD YYYY` — month abbreviation + day (space-padded) + 4-digit year.
  const Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  i = skip
  while i + 11 <= buf.len:
    var matched = false
    if buf[i+3] == ' ' and buf[i+6] == ' ' and
       (buf[i+4] == ' ' or isDigit(buf[i+4])) and isDigit(buf[i+5]) and
       isDigit(buf[i+7]) and isDigit(buf[i+8]) and
       isDigit(buf[i+9]) and isDigit(buf[i+10]):
      let m = buf.substr(i, i + 2)
      for mo in Months:
        if m == mo:
          matched = true
          break
    if matched:
      for k in i ..< i + 11: buf[k] = '\0'
      i += 11
    else:
      inc i

proc bootBinariesEqual*(pa, pb: string): bool =
  ## Equal ignoring linker / preprocessor build-time stamps (build-id and
  ## any embedded `__DATE__` / `__TIME__` strings). Mismatch on size is
  ## always a real difference; same size compares the masked content.
  if getFileSize(pa) != getFileSize(pb): return false
  var a = readFile(pa)
  var b = readFile(pb)
  maskBuildStamps(a)
  maskBuildStamps(b)
  result = a == b

proc stagesEqual*(a, b: string): bool =
  ## Two stages converge when every self-rebuilt binary is byte-identical
  ## after masking build-time stamps (ELF build-id, mimalloc __DATE__/
  ## __TIME__). Auxiliary carried tools are the same file copied twice, so
  ## we don't bother comparing them.
  ##
  ## On macOS the byte comparison is skipped: every link records a fresh
  ## LC_UUID and emits an LC_CODE_SIGNATURE whose body is a SHA-256 hash
  ## chain over the binary's pages. Even a single byte difference (such
  ## as the `__DATE__` / `__TIME__` strings mimalloc bakes into .rodata)
  ## ripples into the signature blob, so two stages whose generated code
  ## is identical still won't compare equal. The masking strategy that
  ## works for ELF doesn't generalize, and Mach-O has no equivalent of
  ## ELF's deterministic `--build-id=none`, so we report stages as
  ## converged once both stages produced binaries of equal size — the
  ## self-compile pass that built `b` having succeeded is what tells us
  ## the previous stage is functional.
  when defined(macosx):
    for tool in BootSelfTools:
      let pa = a / tool.addFileExt(ExeExt)
      let pb = b / tool.addFileExt(ExeExt)
      if getFileSize(pa) != getFileSize(pb):
        echo "[boot] stage size diff: ", tool,
             " (", getFileSize(pa), " vs ", getFileSize(pb), ")"
        return false
    return true
  else:
    for tool in BootSelfTools:
      let pa = a / tool.addFileExt(ExeExt)
      let pb = b / tool.addFileExt(ExeExt)
      if not bootBinariesEqual(pa, pb):
        echo "[boot] stage diff: ", tool
        return false
    return true

proc valgrindSmokeTest*(exe: string) =
  ## Run the bootstrapped binary under valgrind on a trivial command to flush
  ## out the obvious memory corruption bugs (use-after-free, double-free,
  ## invalid reads). `-DMI_TRACK_VALGRIND=1` must already be baked in via
  ## the `--valgrind` boot flag, otherwise mimalloc's arena is opaque to
  ## valgrind and nothing useful comes out.
  echo "[boot] valgrind smoke check on ", exe
  let cmd = "valgrind --leak-check=full --error-exitcode=1 " &
            exe.quoteShell & " --version"
  echo "[boot] ", cmd
  let exitCode = execShellCmd(cmd)
  if exitCode != 0:
    quit "FAILURE: valgrind reported errors in " & exe
  echo "[boot] valgrind smoke check passed"

const BootSelfCompilePasses = 3
  ## Number of self-compile passes the bootstrap runs (`bin1/` … `binN/`).
  ## Fixed so that `boot` is deterministic: every invocation produces the
  ## same set of stage directories regardless of whether earlier stages
  ## happen to converge to a byte-identical binary.

proc specifiesOptLevel*(args: string): bool =
  ## Does the caller already pick a build mode for the bootstrapped compiler?
  ## Then `boot` must not layer its `-d:release` default on top: `-d:danger`
  ## in particular is a deliberate *stronger* choice, and passing both would
  ## read as a contradiction on the command line.
  ##
  ## Matched without the leading dashes on purpose, so that every spelling
  ## counts: getopt strips them off a positional `-d:danger` (it reaches here as
  ## `d:danger` — which is what `--forward:` exists to avoid), while
  ## `--forward:-d:danger` keeps them.
  result = "d:release" in args or "d:danger" in args or "opt:" in args

proc bootCmd*(args: string; withValgrind: bool; release = true) =
  ## `release` ⇒ compile every stage with `-d:release` unless `args` already
  ## names a build mode. Defaulted here rather than at the dispatch site so
  ## `selfcheck`, which calls this directly, gets the same coverage.
  var args = args
  if release and not specifiesOptLevel(args):
    args = if args.len > 0: "-d:release " & args else: "-d:release"
  for tool in BootSelfTools:
    let exe = binDir() / tool.addFileExt(ExeExt)
    if not fileExists(exe):
      quit "boot: " & exe & " not found; run `hastur build all` first"
  # valgrind cannot see the native backend's static, libc-free `mmap` heap, so
  # `--valgrind` (and thus `selfcheck`) always boots through the C backend.
  # `--boot-backend:native` FORCES: the only thing that can stop it is not having
  # arkham and nifasm to run. It used to defer to `useNativeBoot` as well, which
  # made the flag useless on exactly the hosts it is needed on — a host where the
  # native path is off is a host where someone is trying to turn it ON, and
  # "not available here" told them nothing they could act on. `--valgrind` still
  # wins, because memcheck cannot see the native heap at all.
  if bootBackend == bbNative:
    if withValgrind:
      quit "boot: --boot-backend:native and --valgrind are exclusive: " &
           bootBackendLine(withValgrind)
    let missing = missingNativeTools()
    if missing.len > 0:
      quit "boot: --boot-backend:native needs " & missing.join(", ") & " in " &
           binDir() & "; `hastur build native` builds them from " & NativenifDir
    bootNative = true
  else:
    bootNative = not withValgrind and bootBackend != bbC and useNativeBoot()
  echo bootBackendLine(withValgrind)
  for tool in bootCarryTools():
    let exe = binDir() / tool.addFileExt(ExeExt)
    if not fileExists(exe):
      quit "boot: " & exe & " not found; run `hastur build all` first"
  for tool in BootSelfTools:
    let src = bootSourceFor(tool)
    if not fileExists(src):
      quit "boot: " & src & " missing"
  let cacheBase = nimcacheDir / "boot"
  removeDir cacheBase
  createDir cacheBase
  let t0 = epochTime()

  echo "[boot] compiling stages with: ",
       (if args.len > 0: args else: "(no extra flags)")
  var stages = newSeq[string](BootSelfCompilePasses + 1)
  stages[0] = provisionStageZero()
  for n in 1 .. BootSelfCompilePasses:
    stages[n] = compileBootStage(n, cacheBase, args, withValgrind)

  for n in 1 .. BootSelfCompilePasses:
    if stagesEqual(stages[n-1], stages[n]):
      echo "[boot] stages ", n-1, " and ", n, " are byte-identical."
    else:
      echo "[boot] stages ", n-1, " and ", n, " differ."

  if withValgrind:
    valgrindSmokeTest(stages[^1] / "nimony".addFileExt(ExeExt))

  echo "[boot] total ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  echo "SUCCESS."

proc selfcheckCmd*() =
  ## Full compiler self-host regression check. The sequence here mirrors what
  ## a maintainer runs after touching anything in `src/nimony/`, `src/hexer/`
  ## or `src/lib/` that the compiler itself depends on:
  ##
  ##   1. Rebuild nimony + nimsem + hexer from host Nim, so all three reflect
  ##      current source. `boot` copies `bin/` into `bin0/` at every run, so
  ##      a stale `bin/` would still poison `bin0/`.
  ##   2. `bootstrap`: compile every module on the bootstrap list with the
  ##      freshly-built `bin/nimony`. Catches per-module sem/codegen
  ##      regressions and fails fast.
  ##   3. `boot --valgrind`: deterministic self-host (bin0 → bin1 → … →
  ##      binN), then run the last stage's nimony under valgrind. Catches
  ##      whole-program regressions (init order, codegen interactions,
  ##      runtime UAFs) that single-module compiles miss. Boots at
  ##      `-d:release` (boot's default), so the shoggoth optimizer and the
  ##      `when defined(release)` paths are part of what this checks.
  ##
  ##      `--valgrind` keeps this on the C backend (see `bootCmd`); plain
  ##      `hastur boot` is the native self-host.
  ##
  ## Boot's "stages N and N+1 differ" messages are informational — they
  ## normally reflect gcc's `--build-id` non-determinism, not a real
  ## divergence; the valgrind smoke test is what tells us the last stage
  ## actually runs.
  let t0 = epochTime()
  echo "[selfcheck] step 1/3: rebuilding nimony toolchain"
  buildNimonyToolchain(showProgress = true)
  echo "[selfcheck] step 2/3: bootstrap (per-module compile check)"
  tierTests()
  echo "[selfcheck] step 3/3: boot --valgrind (3-stage self-host + valgrind smoke)"
  bootCmd("", withValgrind = true)
  echo "[selfcheck] all checks passed in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."

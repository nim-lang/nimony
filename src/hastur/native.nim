## The curated native-backend (`nimony n`: arkham + nifasm) regression set,
## the same corpus under memcheck, and the `tests/nativecg` machine-code golden
## suite.

import std / [syncio, os, osproc, strutils, times, algorithm, sets]
import context, counters, compile, joined, builders

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
  NativeTestDirs = [
    "tests/nimony/arc",        # full ARC suite — byte-identical to the C backend
    "tests/nimony/closures"
  ]

  NativeTestDirsWindows = [
    # Windows grew a native target later than the other hosts, so what it is known
    # to run correctly is recorded separately until a run elsewhere confirms the
    # same — the whole point of this file is that it states OBSERVED results, and
    # these were observed on Windows. Fold an entry into `NativeTestDirs` once it
    # has been seen green on Linux/macOS too.
    #
    # `tests/nimony/stdlib`: 45 of its 53 pass; the rest are in `NativeTestSkip`.
    # This is the suite that covers the process vectors the Windows entry point
    # does not receive (`tcmdline`, `tenvvars`, `tos`) — the reason it is worth
    # running natively at all rather than only through the C backend.
    "tests/nimony/stdlib"
  ]

  NativeTestSkip = [
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
  NativeTestFiles = [
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
    "tests/nimony/sets/tconstsetscan"
  ]

when defined(linux):
  var nativeElfShapeChecked = false
    ## The shape check below is an invariant of nifasm's ELF writer, not of any one
    ## test, so it runs once per suite — on the first native binary produced.

  proc u16(s: string; at: int): int =
    int(uint8(s[at])) or (int(uint8(s[at+1])) shl 8)

  proc u32(s: string; at: int): int =
    var r = 0
    for i in countdown(3, 0): r = (r shl 8) or int(uint8(s[at+i]))
    result = r

  proc u64(s: string; at: int): uint64 =
    result = 0'u64
    for i in countdown(7, 0): result = (result shl 8) or uint64(uint8(s[at+i]))

  proc checkNativeElfShape(c: var TestCounters; file, exe: string) =
    ## The three properties that make a nifasm image DEBUGGABLE, asserted against the
    ## bytes rather than against the writer that produced them.
    ##
    ## All three were absent once, and the way they failed is why this is a test:
    ## nothing crashed and no test went red. The image merged code and data into one
    ## R+W+X segment, which valgrind's ELF reader — it classifies a mapping as the
    ## text map only when it is R+X and NOT writable — declined to read at all, so
    ## every frame of every report came back as `???` and valgrind looked like it
    ## simply had nothing to say about native binaries. `.eh_frame` sat outside every
    ## PT_LOAD, which valgrind treats as a FATAL debug-info error, taking `.symtab`
    ## down with it. A regression here costs no test and one very confusing week.
    if nativeElfShapeChecked: return
    nativeElfShapeChecked = true
    var img = ""
    try: img = readFile(exe)
    except IOError, OSError: return
    if img.len < 64 or not img.startsWith("\x7FELF"): return
    if uint8(img[4]) != 2'u8 or uint8(img[5]) != 1'u8: return  # ELF64 little-endian only

    const PtLoad = 1
    const (PfX, PfW, PfR) = (1, 2, 4)
    let phoff = int(u64(img, 32))
    let phentsize = u16(img, 54)
    let phnum = u16(img, 56)
    var loads: seq[(uint64, uint64, int)] = @[]   # vaddr, memsz, flags
    var rxText = false
    for i in 0 ..< phnum:
      let at = phoff + i * phentsize
      if at + phentsize > img.len: return
      if u32(img, at) != PtLoad: continue
      let flags = u32(img, at + 4)
      let vaddr = u64(img, at + 16)
      let memsz = u64(img, at + 40)
      if memsz == 0: continue
      loads.add (vaddr, memsz, flags)
      if (flags and PfW) != 0 and (flags and PfX) != 0:
        failure c, file, "no writable+executable PT_LOAD (W^X)",
          "segment at 0x" & toHex(vaddr, 8) & " is R+W+X"
        return
      if (flags and PfX) != 0 and (flags and PfW) == 0 and (flags and PfR) != 0:
        rxText = true
    if not rxText:
      failure c, file, "a readable+executable, non-writable PT_LOAD",
        "none of the " & $loads.len & " PT_LOADs is R+X and not writable"
      return

    # `.eh_frame` must be SHF_ALLOC and land inside one of those PT_LOADs.
    const ShfAlloc = 2'u64
    let shoff = int(u64(img, 40))
    let shentsize = u16(img, 58)
    let shnum = u16(img, 60)
    let shstrndx = u16(img, 62)
    if shoff == 0 or shnum == 0: return          # stripped: nothing to check
    let strBase = int(u64(img, shoff + shstrndx * shentsize + 24))
    for i in 0 ..< shnum:
      let at = shoff + i * shentsize
      if at + shentsize > img.len: return
      let nameOff = strBase + u32(img, at)
      if nameOff >= img.len: return
      var name = ""
      var k = nameOff
      while k < img.len and img[k] != '\0': name.add img[k]; inc k
      if name != ".eh_frame": continue
      let shFlags = u64(img, at + 8)
      let shAddr = u64(img, at + 16)
      let shSize = u64(img, at + 32)
      if shSize == 0: return                     # built with `--no-debug-info`
      if (shFlags and ShfAlloc) == 0 or shAddr == 0:
        failure c, file, ".eh_frame is SHF_ALLOC with a real sh_addr",
          "sh_flags=0x" & toHex(shFlags, 4) & " sh_addr=0x" & toHex(shAddr, 8)
        return
      for (vaddr, memsz, _) in loads:
        if shAddr >= vaddr and shAddr + shSize <= vaddr + memsz: return
      failure c, file, ".eh_frame inside a PT_LOAD",
        "sh_addr=0x" & toHex(shAddr, 8) & " is in none of the loaded segments"
      return

proc nativeTestFile*(c: var TestCounters; file: string; overwrite: bool) =
  let msgs = file.changeFileExt(".msgs")
  if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
    return
  inc c.total
  let (compilerOutput, compilerExitCode) = execNimonyNative(quoteShell(file))
  if compilerExitCode != 0:
    failure c, file, "native compiler exitcode 0",
      removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
    return
  let exe = file.generatedExeFile()
  if not exe.fileExists():
    failure c, file, "native executable", "missing: " & exe
    return
  when defined(linux):
    checkNativeElfShape c, file, exe
  let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
  var output = file.changeFileExt(".output")
  if testProgramExitCode != 0:
    output = file.changeFileExt(".exitcode")
    if not output.fileExists():
      failure c, file, "test program exitcode 0",
        "exitcode " & $testProgramExitCode & "\n" & testProgramOutput
      return
  if output.fileExists():
    let outputSpec = readFile(output).strip
    if outputSpec != testProgramOutput.strip:
      if overwrite:
        writeFile(output, testProgramOutput)
      failure c, file, outputSpec, testProgramOutput

proc nativeValgrindTestFile*(c: var TestCounters; file: string) =
  ## One native test built with `-d:valgrind` and run under memcheck.
  ##
  ## Two things are asserted, and the second is the one worth the runtime: that
  ## the program still prints what it should, and that memcheck has NOTHING to
  ## say about it. The heap instrumentation lives in the allocator, so a mistake
  ## in it is not a wrong answer anywhere — it is a false report, arriving in
  ## whatever unrelated program someone was debugging, looking exactly like the
  ## bug they were hunting. The only way to know the instrumentation is honest is
  ## to run a corpus that is known good and demand silence.
  let msgs = file.changeFileExt(".msgs")
  if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
    return
  inc c.total
  let (compilerOutput, compilerExitCode) = execNimonyNative("-d:valgrind " & quoteShell(file))
  if compilerExitCode != 0:
    failure c, file, "native -d:valgrind compiler exitcode 0",
      removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
    return
  let exe = file.generatedExeFile()
  if not exe.fileExists():
    failure c, file, "native executable", "missing: " & exe
    return
  # `-q` so only real findings appear, and `--error-exitcode` so the verdict is
  # the exit code rather than something to parse out of the log.
  let (vgOutput, vgExitCode) = osproc.execCmdEx(
      "valgrind --error-exitcode=99 --leak-check=no -q " & quoteShell(exe))
  if vgExitCode == 99:
    failure c, file, "no valgrind findings", vgOutput
    return
  let output = file.changeFileExt(".output")
  if output.fileExists():
    let outputSpec = readFile(output).strip
    if outputSpec != vgOutput.strip:
      failure c, file, outputSpec, vgOutput

const NativeValgrindSkip: array[0, string] = [
  # Empty. A test belongs here when it runs correctly under `hastur native` but
  # memcheck reports a real defect in it that is NOT the allocator
  # instrumentation's doing — with the reason spelled out, as
  # `tclosure_iter_string` had before its two causes were fixed (`allocFrame`
  # now uses `alloc0`, and the coroutine frame's `oconstr` is total).
]

proc nativeValgrindTests*() =
  ## The native regression set again, this time with the allocator telling
  ## valgrind what it is doing (`lib/std/system/valgrind.nim`). Same corpus as
  ## `nativetests`, so what it adds is purely the memcheck verdict.
  if not hasValgrind:
    echo "0 / 0 native valgrind tests (valgrind not installed)"
    return
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  proc slashed(p: string): string = p.replace('\\', '/')
  var skip = initHashSet[string]()
  for f in NativeTestSkip: skip.incl slashed(f.addFileExt(".nim"))
  for f in NativeValgrindSkip: skip.incl slashed(f.addFileExt(".nim"))
  var dirs: seq[string] = @NativeTestDirs
  when defined(windows): dirs.add @NativeTestDirsWindows
  for dir in dirs:
    var files: seq[string] = @[]
    for x in walkDir(dir):
      if x.kind == pcFile and x.path.endsWith(".nim") and
         not isGeneratedTestFile(x.path) and slashed(x.path) notin skip:
        files.add x.path
    sort files
    for f in files: nativeValgrindTestFile c, f
  for f in NativeTestFiles:
    if slashed(f.addFileExt(".nim")) in skip: continue
    nativeValgrindTestFile c, f.addFileExt(".nim")
  reportFailures c
  echo c.total - c.failures, " / ", c.total, " native valgrind tests successful in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native valgrind tests failed."

proc nativetests*(overwrite: bool) =
  ## Run the native-backend regression set (`NativeTestDirs` + `NativeTestFiles`,
  ## plus `NativeTestDirsWindows` on Windows, minus `NativeTestSkip`) through
  ## `nimony n`. Requires the sibling `../nativenif` checkout (arkham/nifasm).
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  # `walkDir` joins with the platform separator onto a forward-slashed constant, so
  # its paths come back mixed (`tests/nimony/stdlib\tbitops.nim` on Windows) and can
  # only be compared to the skip list once both sides are spelled one way.
  proc slashed(p: string): string = p.replace('\\', '/')
  var skip = initHashSet[string]()
  for f in NativeTestSkip: skip.incl slashed(f.addFileExt(".nim"))
  var dirs: seq[string] = @NativeTestDirs
  when defined(windows): dirs.add @NativeTestDirsWindows
  for dir in dirs:
    var files: seq[string] = @[]
    for x in walkDir(dir):
      # `_hastur_joined.nim` is the C-backend runner's own build artifact, left
      # in the tree by an earlier `hastur tests/…`. Walking it in as a test made
      # the native result depend on whether that run happened, and reported the
      # joined program's arkham gaps against whichever test was added last.
      if x.kind == pcFile and x.path.endsWith(".nim") and
         not isGeneratedTestFile(x.path) and slashed(x.path) notin skip:
        files.add x.path
    sort files
    for f in files: nativeTestFile c, f, overwrite
  for f in NativeTestFiles:
    nativeTestFile c, f.addFileExt(".nim"), overwrite
  reportFailures c
  echo c.total - c.failures, " / ", c.total, " native tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native tests failed."
  else:
    echo "SUCCESS."

proc asmNifTarget*(asmFile: string): string =
  ## The target arkham compiled for, read off the `(arch …)` its asm-NIF opens
  ## with (`x64`, `win_x64`, `arm64`, `linux_arm64`). Taken from the OUTPUT rather
  ## than re-derived from `hostOS`/`hostCPU` so the golden can never be filed under
  ## a target the file was not actually generated for.
  let s = readFile(asmFile)
  let i = s.find("(arch ")
  if i < 0: return ""
  let j = s.find(')', i)
  if j < 0: return ""
  result = s[i + len("(arch ") .. j - 1].strip

proc runNativeCodegenTests*(dir: string; overwrite: bool) =
  ## Custom runner for `tests/nativecg`: a golden suite over the C-free native
  ## backend's *emitted machine code*. For each `.nim` it
  ##   1. compiles with `nimony n --opt:speed` (so the shoggoth inliner/optimizer
  ##      that feeds the native path actually runs),
  ##   2. goldens arkham's `<main>.asm.nif` (the typed assembler NIF) against a
  ##      checked-in `<test>.<target>.asm.nif`, and
  ##   3. runs the linked libc-free executable, checking `.output` / `.exitcode`.
  ##
  ## The asm-NIF is byte-stable for a fixed *relative* test path — module
  ## suffixes are derived from the relative path, and a module's own symbols
  ## carry no suffix — so the golden is portable across checkouts/machines as
  ## long as hastur is invoked from the repo root (which it always is). No
  ## normalization is needed.
  ##
  ## It is NOT portable across TARGETS, though — machine code is the point of the
  ## suite. So the golden is per-target (`tinlinecond.x64.asm.nif`,
  ## `tinlinecond.win_x64.asm.nif`, …), named for the `(arch …)` arkham actually
  ## emitted. A target with no checked-in golden yet fails as a missing file;
  ## `--overwrite` writes it, which is how a new one is added.
  ##
  ## Requires the sibling `../nativenif` checkout (arkham/nifasm), exactly like
  ## the `native` subcommand; the directory is `hastur.mode = skip` so the
  ## default `all` sweep leaves this opt-in.
  if not skipBuild:
    buildNimony()
    buildHexer()
    buildShoggoth()
    buildArkham()
    buildNifasm()
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and
       x.path.extractFilename != "setup.nim":   # the runner itself, not a test
      files.add x.path
  sort files
  for file in files:
    let msgs = file.changeFileExt(".msgs")
    if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
      continue                              # negative test: not a codegen case
    inc c.total
    let cacheArg =
      if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
      else: ""
    let (compilerOutput, compilerExitCode) =
      execLocal("nimony",
        "n --opt:speed --silentMake --isMain " & cacheArg & quoteShell(file))
    if compilerExitCode != 0:
      failure c, file, "native compiler exitcode 0",
        removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
      continue
    # 1) Golden arkham's assembler NIF for the main module.
    let asmFile = generatedFile(file, ".asm.nif")
    if not asmFile.fileExists():
      failure c, file, "arkham asm.nif", "missing: " & asmFile
      continue
    let target = asmNifTarget(asmFile)
    if target.len == 0:
      failure c, file, "arkham asm.nif with an (arch …)", "no target in: " & asmFile
      continue
    diffFiles(c, file, file.changeFileExt(target & ".asm.nif"), asmFile, overwrite)
    # 2) Behavioural check: the linked ELF must run and match .output/.exitcode.
    let exe = generatedExeFile(file)
    if not exe.fileExists():
      failure c, file, "native executable", "missing: " & exe
      continue
    let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
    var output = file.changeFileExt(".output")
    if testProgramExitCode != 0:
      output = file.changeFileExt(".exitcode")
      if not output.fileExists():
        failure c, file, "test program exitcode 0",
          "exitcode " & $testProgramExitCode & "\n" & testProgramOutput
        continue
    if output.fileExists():
      let outputSpec = readFile(output).strip
      if outputSpec != testProgramOutput.strip:
        if overwrite:
          writeFile(output, testProgramOutput)
        failure c, file, outputSpec, testProgramOutput
  reportFailures c
  echo c.total - c.failures, " / ", c.total,
    " native-codegen tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native-codegen tests failed."
  else:
    echo "SUCCESS."

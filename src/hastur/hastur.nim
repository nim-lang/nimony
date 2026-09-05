## Hastur - Tester tool for Nimony and its related subsystems (Leng etc).
## (c) 2024-2025 Andreas Rumpf
##
## This module is the COMMAND LINE — the usage text, the option parsing and the
## dispatch. Everything it dispatches to lives in a sibling module here in
## `src/hastur/`, and `kit.nim` re-exports that surface for a test directory's
## `setup.nim` custom runner.

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../../icons/hastur.res".}
    else:
      {.link: "../../icons/hastur_icon.o".}

import std / [assertions, parseopt, strutils, os, osproc]

import context, category, joined, nativelist, runner, walk, builders, deps,
       tiers, boot, native, record, bugcmd, gitcmds, wasmdiff
import install

const
  Version = "0.6.0"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024-2026 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  build [all|nimony|nifler|hexer|lengc|shoggoth|nifmake|validator|dagon|pnak|arkham|nifasm|native|nifbench]   build selected tools (default: all).
                       `nifbench` is the NIF micro-benchmark suite (bench/),
                       built with host Nim so it can be compared against the same
                       source built by `nimony c` and `nimony n`.
                       `all` builds arkham + nifasm too when the sibling
                       `../nativenif` is checked out, and says so when it is not;
                       `native` (arkham + nifasm + shoggoth) asks for them by
                       name and errors out instead.
  tiers [native]       compile every module on the bootstrap list with nimony.
                       `native` uses `nimony n` (arkham + nifasm) instead of
                       `nimony c`: the list is a walk of the bootstrap DAG from
                       the leaves up, so it attributes each native gap to the
                       smallest module that reaches it — which `boot` cannot do,
                       since it compiles three whole tools and reports only the
                       first that died. Nothing fails fast: the run ends with one
                       line per distinct diagnostic and the modules reaching it.
                       `--forward:` is appended to every compile (e.g.
                       `--forward:-d:release`).
  boot [options]       Self-host the *full* nimony toolchain (nimony,
                       nimsem, hexer). `bin0/` is a fresh copy of the
                       host-Nim-built toolchain; `binN/` is `binN-1/`'s
                       nimony recompiling all three from source. Runs a
                       fixed number of self-compile passes and leaves the
                       results in place — nothing is installed back to
                       `bin/`. Extra args are forwarded to every
                       `nimony c` invocation.
                       Every stage is compiled with `-d:release` by default
                       (the wider test: shoggoth + the `when defined(release)`
                       paths). `--no-release` boots at the default opt level.
                       To boot at another mode use `--forward:` so the flag
                       survives getopt intact — `--forward:-d:danger` replaces
                       the default rather than stacking on it.
                       On linux/amd64 every stage is built with the C-FREE
                       NATIVE backend (`nimony n`: arkham + nifasm, no C
                       compiler, no libc) when those two are built; otherwise
                       through the C backend.
  selfcheck            full compiler regression check: rebuilds the nimony
                       toolchain (nimony+nimsem+hexer share `programs.nim`),
                       runs `tiers`, then `boot --valgrind` (release). Use this
                       after touching any module the compiler itself imports.
  <dir>                run the test tree rooted at <dir> (see Files below):
                       each directory is either a setup.nim custom runner or
                       the built-in nimony runner, recursively. This is the
                       general entry point — point it at any suite.
  all                  run the whole suite: `<tests>` + `<examples>`.
  nativevalgrind       run the native regression set with `-d:valgrind`, under
                       memcheck, and demand that memcheck reports nothing. This
                       is what says the allocator's heap instrumentation
                       (`lib/std/system/valgrind.nim`) describes the heap
                       truthfully rather than plausibly. Linux/AArch64 for now,
                       which is where `VgClientRequest` has a lowering.
  native               run the curated native-backend regression set through
                       `nimony n` (arkham + nifasm, from the sibling
                       `../nativenif` checkout). See `NativeTestDirs`/`Files`.
  wasmdiff             differential harness: run every `tests/ithaqua/*.nim`
                       through BOTH the native backend (arkham, the oracle)
                       and the wasm backend (ithaqua) and require matching
                       stdout + exit code. Needs the sibling `../nativenif`
                       and `node` on PATH.
  lengc                 run Leng tests.
  test <file>/<dir>    run a single test <file>, or a flat <dir> of tests.
  joined <dir>         run <dir>'s joinable tests as ONE program (see
                       `--joined` below). This is what the test pool spawns
                       per directory; by hand it is how you run or debug a
                       single group.
  bug [file]           build nimony+hexer and compile <file> to fill nimcache/.
                       If no file is provided `bug.nim` is used.
  rep                  repeat the last failing tool command from the session.
  record <file> <tout> track the results to make it part of the test suite.
  update deps          re-pin `src/nativenif.commit` — the sibling nativenif
                       commit arkham and nifasm are built from — to whatever
                       `../nativenif` currently has checked out. Every other
                       command PUTS nativenif on that commit before building
                       it (a dirty nativenif tree is left alone, with a
                       warning). The file is read at run time; an empty or
                       absent one means "build whatever is checked out".
  update parser        re-pin `src/nifler/nimparser/upstream.commit` — Nim's
                       own `compiler/parser.nim`, which nifler parses `.nim`
                       source with — to the tip of nim-lang/Nim `devel`, and
                       check it out. `build nifler` checks out the pin (from a
                       Nim git checkout on this machine if it has the commit,
                       else one HTTPS fetch); an empty or absent pin means
                       "use whatever parser the host compiler ships".
  install              write activation script(s) at the project root that
                       prepend the toolchain dirs to `$PATH` for the current
                       shell. On Windows, also download MinGW+LLVM (gcc,
                       clang, lld) and Nim's DLL deps into `external/`.
  clean                remove all generated files.
  sync [new-branch]    delete current branch and pull the latest
                       changes from remote. Optionally creates a new branch.

Arguments are forwarded to the Nimony compiler.

Options:
  --overwrite           overwrite the selected test results. Implies
                        `--joined:off`: a golden is regenerated from the test
                        running on its own. A plain test that prints without
                        an `.output` file gets one written here — that is what
                        makes it joinable.
  --ast                 track the contents of the AST too
  --codegen             track the contents of the code generator too
  --version             show the version
  --help                show this help
  --forward:OPTION      pass an option to the Nimony compiler
  --debug               build the front-end tools (nimony, nimsem, hexer, …)
                        unoptimized; they default to -d:release, like arkham
                        and nifasm. For gdb work on the tools themselves —
                        a debug toolchain is ~8x slower at compiling, so all
                        test runs want the default
  --release             no-op for the toolchain (release is the default); for
                        `boot` it additionally compiles the bootstrapped
                        nimony's own output with --opt:speed
  --jobs:N|auto         run up to N tests in parallel (auto = #cores)
  --joined:on|off       compile a directory's plain tests into ONE program
                        (default on) instead of one per test. Most of a test's
                        cost is the process tree behind it — nifler, nimsem,
                        hexer, lengc, nifmake, the C compiler, the linker —
                        so this is the big lever on Windows. A group that
                        fails is re-run test by test, so failures stay
                        attributable; `off` skips the grouping altogether.
  --native:on|off       on WINDOWS, compile the tests `hastur native` vouches
                        for with `nimony n` rather than `nimony c` (default
                        on). Same assertion, no gcc and no linker behind it —
                        the other half of the lever `--joined` pulls. Ignored
                        on every other host.
  --cachedir:PATH       use PATH instead of `nimcache/` for intermediates
  --bindir:PATH         resolve the toolchain (nimony, lengc, …) from
                        directory PATH instead of hastur's own directory
                        (implies --no-build). Binaries not found there are
                        looked up on `$PATH`.
  --no-build            skip the setup.hastur prep step during the tree walk
  --skip:DIR            leave DIR out of the tree walk (repeatable). For
                        splitting one sweep across CI runners: the tester job
                        passes `--skip:tests/boot` while a second job runs
                        `hastur tests/boot`. Unlike `hastur.mode = skip` this
                        does not change what a plain local run covers.
  --native-debug        build arkham + nifasm unoptimized (they default to
                        -d:release); for `-d:arkhamDbgSym` / gdb toolchain work
  --boot-backend:auto|c|native
                        which backend `boot` compiles the stages with. `auto`
                        (default) is per-host: `nimony n` where the native
                        backend is complete and arkham/nifasm are in bin/,
                        `nimony c` otherwise. The other two force it, which is
                        how the two are timed against each other.
  --valgrind            for `boot`: build with -DMI_TRACK_VALGRIND=1 so
                        mimalloc plays nicely with valgrind, then run a
                        valgrind smoke test on the bootstrapped binary.
                        Forces the C backend (valgrind cannot see the native
                        backend's libc-free heap).

Files (per test directory, all optional):
  setup.nim             a custom runner program that owns this directory and
                        its subtree: hastur compiles+runs it (it imports
                        `src/hastur/kit` as the test kit) and takes its exit
                        code as the verdict. For suites that aren't a folder
                        of inputs (boot, incremental, validator) or need a
                        bespoke tool (dagon, pnak, hexer, controlflow,
                        contracts).
  setup.hastur          prep for a built-in-runner directory: each line is a
                        hastur subcommand (e.g. `build nimony`) run before the
                        tests beneath it. `tests/setup.hastur` builds the
                        toolchain for the whole sweep.
  <test>.nojoin         keep <test> out of its directory's joined program (see
                        `--joined`). For a test whose output is not
                        reproducible in a shared process, or that races with
                        its neighbours over a shared build artifact.
  hastur.mode           this directory's category for the built-in nimony
                        runner: nosystem, track, compat, valgrind, opt, or
                        skip (excluded from the sweep, but still run when
                        pointed at directly). Absent means normal.
"""

proc writeHelp() = quitWithText(Usage)
proc writeVersion() = quitWithText(Version & "\n")

proc handleCmdLine =
  var primaryCmd = ""
  var rawPrimary = ""   # unnormalized first arg; a directory path stays intact
  var args: seq[string] = @[]

  var flags: set[RecordFlag] = {}
  var overwrite = false
  var forward = ""
  var withValgrind = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if primaryCmd.len == 0:
        primaryCmd = key.normalize
        rawPrimary = key
      else:
        args.add key
    of cmdLongOption, cmdShortOption:
      # `--cachedir` / `--jobs` / `--forward` can appear anywhere — they
      # configure the test runner regardless of position relative to the
      # primary cmd. `--forward` in particular MUST be position-agnostic
      # because the parallel test runner spawns child `hastur test ...`
      # invocations and threads the forward value back in after `test`,
      # so requiring it before the subcommand would silently lose it in
      # the child. Other long options stay tied to the pre-command
      # position (or to the `record` subcommand).
      let n = normalize(key)
      case n
      of "cachedir":
        if val.len == 0: writeHelp()
        nimcacheDir = val
      of "bindir":
        # Point hastur at a prebuilt/installed toolchain instead of the
        # project-local `bin/`. Implies `--no-build`: there is no source
        # tree to rebuild from when testing out-of-tree code.
        if val.len == 0: writeHelp()
        toolchainDir = val
        skipBuild = true
      of "jobs", "j":
        if val == "auto" or val.len == 0:
          parallelJobs = countProcessors()
        else:
          try: parallelJobs = max(1, parseInt(val))
          except: writeHelp()
      of "no-build", "nobuild":
        skipBuild = true
      of "skip":
        if val.len == 0: writeHelp()
        skipDirs.add normalizeDirKey(val)
      of "joined":
        # `--joined:off` gives every test its own process again. The joined
        # runner already falls back to that per group on failure; this is for
        # ruling the joining out entirely.
        joinTests = val.normalize notin ["off", "no", "false", "0"]
      of "native":
        # `--native:off` puts the Windows tree walk back on the C backend for
        # the whitelisted tests it would otherwise compile with `nimony n`
        # (see `nativelist.walkUsesNative`). Nothing anywhere else reads it.
        walkNative = val.normalize notin ["off", "no", "false", "0"]
      of "native-debug", "nativedebug":
        # Build arkham + nifasm UNOPTIMIZED (they default to -d:release; see
        # nativeToolPrefix). For `-d:arkhamDbgSym` / gdb work on the toolchain.
        nativeToolsDebug = true
      of "debug":
        # Build the front-end tools UNOPTIMIZED (they default to -d:release;
        # see nimcPrefix). Position-agnostic like `--release` below: these
        # decide how the toolchain is built, and the tree walk rebuilds it
        # via `setup.hastur` long after the subcommand was parsed.
        debugBuild = true
      of "release":
        # Build-mode-wise a no-op (release IS the default), for the toolchain
        # itself *and* — since it is now `boot`'s default — for the toolchain
        # `boot` compiles. Retained as the explicit spelling of that default.
        bootRelease = true
      of "no-release", "norelease":
        # Boot at the default opt level. For isolating whether a boot failure
        # is release-specific (`-d:release` turns the shoggoth optimizer on and
        # flips the `when defined(release)` paths at once, so a green
        # `--no-release` boot narrows the cause to one of those two).
        bootRelease = false
      of "valgrind":
        withValgrind = true
      of "boot-backend", "bootbackend":
        # Which backend `boot` compiles the stages with. `auto` (the default)
        # is `useNativeBoot`; the other two are for measuring one against the
        # other, which is otherwise impossible on a host where the native path
        # is the automatic one.
        case val.normalize
        of "auto": bootBackend = bbAuto
        of "c": bootBackend = bbC
        of "native", "n": bootBackend = bbNative
        else: writeHelp()
      of "forward":
        # Accumulate so callers can layer flags — `--forward:--cc:clang
        # --forward:--passL:-fuse-ld=lld` reaches nimony as both options
        # rather than only the last one. The whole string is appended
        # verbatim to the nimony command line.
        if forward.len > 0: forward.add ' '
        forward.add val
      else:
        if primaryCmd.len == 0 or primaryCmd == "record":
          case n
          of "help", "h": writeHelp()
          of "version", "v": writeVersion()
          of "codegen": flags.incl RecordCodegen
          of "ast": flags.incl RecordAst
          of "overwrite": overwrite = true
          else: writeHelp()
        else:
          args.add key
          if val.len != 0:
            args[^1].add ':'
            args[^1].add val
    of cmdEnd: assert false, "cannot happen"
  if primaryCmd.len == 0:
    writeHelp()

  createDir binDir()

  case primaryCmd
  of "all":
    # `all` is now the tree walk: `tests/` (each suite via its setup.nim or the
    # built-in nimony runner; `tests/setup.hastur` builds the toolchain first)
    # plus `examples/` and `bench/`. Directories marked `hastur.mode = skip`
    # (dagon, hexer) stay out of the sweep but remain runnable via
    # `hastur tests/<dir>`.
    #
    # `bench/` is in the sweep so the benchmarks cannot rot unnoticed — they are
    # not run by anything else, and a benchmark that stopped compiling is only
    # discovered when someone reaches for it. Its `hastur.mode = bench` compiles
    # them with `-d:benchSmoke`, which shrinks every workload to a deterministic
    # smoke run: the question here is "does it build and does it still compute
    # the same numbers", never "how fast".
    walkRoots(["tests", "examples", "bench"], forward, overwrite)

  of "tiers":
    # `buildNimonyToolchain`, not `buildNimony`: every module on the tier list is
    # compiled by driving nimsem and hexer, so rebuilding the driver alone would
    # check current sources against whichever nimsem/hexer `bin/` was left with.
    if not skipBuild: buildNimonyToolchain()
    # `hastur tiers native` walks the same list through `nimony n`. Spelled as a
    # positional rather than reusing `--boot-backend:`, which names what `boot`
    # does and would read as a flag with no effect here.
    var tiersNative = false
    for a in items(args):
      case a.normalize
      of "native", "n": tiersNative = true
      of "c": tiersNative = false
      else: quit "tiers: unknown argument " & a & " (expected `native` or `c`)"
    tierTests(tiersNative, forward)

  of "boot":
    # Same reason, one step further: `bin0/` is a COPY of `bin/`, so a stale
    # nimsem or hexer there is what every later stage is grown from.
    if not skipBuild: buildNimonyToolchain()
    var bootArgs = ""
    for a in items(args):
      if bootArgs.len > 0: bootArgs.add ' '
      bootArgs.add quoteShell(a)
    # `--forward:<flag>` is appended verbatim to every stage's `nimony c`
    # command line. Unlike positional `args`, getopt keeps the value intact
    # (dashes and all), so this is the way to forward flags like
    # `-d:nimNativeAlloc` that must survive unmangled into nimony (and thus
    # into nimsem, where the `when defined(...)` is evaluated).
    if forward.len > 0:
      if bootArgs.len > 0: bootArgs.add ' '
      bootArgs.add forward
    bootCmd(bootArgs, withValgrind, release = bootRelease)

  of "selfcheck":
    selfcheckCmd()

  of "build":
    const showProgress = true
    # Only fetch the submodule when it is actually missing. A `jj` workspace
    # (or a `git worktree`) shares one repo and has no gitlink of its own, so
    # `git submodule update --init` fails there with "Unable to find current
    # revision" even when the sources are present. Checking for the files is
    # what the build cares about anyway.
    if not fileExists("vendor" / "mimalloc" / "src" / "static.c"):
      exec "git submodule update --init"
    case (if args.len > 0: args[0] else: "")
    of "", "all":
      buildNifler(showProgress)
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildLengc(showProgress)
      buildShoggoth(showProgress)
      buildNiflink(showProgress)
      buildHexer(showProgress)
      buildNifmake(showProgress)
      buildValidator(showProgress)
      buildDagon(showProgress)
      buildPnak(showProgress)
      buildNativeTools(showProgress)
    of "nifler":
      buildNifler(showProgress)
    of "nimony":
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildHexer(showProgress)
    of "lengc":
      buildLengc(showProgress)
    of "shoggoth":
      buildShoggoth(showProgress)
    of "niflink":
      buildNiflink(showProgress)
    of "arkham":
      buildArkham(showProgress)
    of "nifasm":
      buildNifasm(showProgress)
    of "native":
      # The C-free native toolchain used by `nimony n`: arkham + nifasm (from
      # the sibling `../nativenif`) plus shoggoth (the opt-gated Leng optimizer
      # that also feeds the native path). `all` builds these three too; this
      # spelling is the one that rebuilds JUST them, and errors out rather than
      # skip when the sibling checkout is missing.
      buildArkham(showProgress)
      buildNifasm(showProgress)
      buildShoggoth(showProgress)
    of "hexer":
      buildHexer(showProgress)
    of "nifmake":
      buildNifmake(showProgress)
    of "nifbench":
      buildNifbench(showProgress)
    of "validator":
      buildValidator(showProgress)
    of "dagon":
      buildDagon(showProgress)
    of "pnak":
      buildPnak(showProgress)
    else:
      writeHelp()
    removeDir "nimcache"

  of "native":
    # Run the curated native-backend regression set through `nimony n`. Build the
    # front end AND the C-free native toolchain (arkham + nifasm + shoggoth live in
    # the sibling `../nativenif`; nifmake drives the `n` pipeline).
    #
    # The front end is rebuilt HERE and not left to whatever `bin/` happens to
    # hold. The shared host-Nim artifact cache is restored through a
    # `restore-keys:` prefix, so a commit that changes `src/**` misses the exact
    # key and still gets the PREVIOUS commit's `bin/` handed to it — which is how
    # the new closure tests of #2292 came to be run against the pre-#2292 hexer
    # and reported as eight "native gap" failures.
    #
    # nimsem/nimony/hexer and not `nifler`: those three share `programs.nim` and
    # ARE what a native run exercises, while nifler is the tier-0 Nim parser in
    # front of them — and the most expensive tool in the tree by a distance
    # (28s cold, more than the three together). `build all` covers it, in this
    # same job for CI and via `tests/setup.hastur` for a tree walk.
    if not skipBuild:
      buildNimonyToolchain()
      buildNifmake()
      buildShoggoth()
      buildArkham()
      buildNifasm()
    nativetests(overwrite)

  of "nativevalgrind":
    # The same corpus as `native`, built with `-d:valgrind` and run under
    # memcheck. Separate command rather than part of `native` because it is a
    # different question (is the ALLOCATOR's story to valgrind true?) at maybe
    # twenty times the runtime — memcheck's interpretation is not cheap.
    if not skipBuild:
      buildNimonyToolchain()
      buildNifmake()
      buildShoggoth()
      buildArkham()
      buildNifasm()
    nativeValgrindTests()

  of "wasmdiff":
    # Differential harness: the native backend (arkham) as the executable oracle
    # for the wasm backend (ithaqua). Builds both toolchains, then diffs stdout
    # and exit code of every `tests/ithaqua/*.nim` fixture across the two
    # pipelines. `wasmdiff.nim` builds what it needs itself.
    wasmdiffCmd()

  of "lengc":
    buildLengc()

  of "test":
    if args.len > 0:
      for arg in args:
        if not arg.dirExists():
          test arg, overwrite, categoryOf(arg), forward
        elif fileExists(arg / "setup.nim"):
          setupDirCmd arg, overwrite, forward
        else:
          testDirCmd arg, overwrite, forward
    else:
      quit "`test` takes an argument"
  of "joined":
    # Internal: the worker the parallel pool spawns for a directory's joined
    # group. Usable by hand to run (or debug) one group.
    if args.len == 1 and args[0].dirExists():
      joinedDirCmd args[0], overwrite, forward
    else:
      quit "`joined` takes one directory"
  of "bug", "debug":
    if args.len == 0:
      args = @["bug.nim"]
    bugCmd(args, forward)
  of "rep":
    repCmd()
  of "record":
    buildNimony()
    if args.len == 2:
      let inp = args[0].addFileExt(".nim")
      let outp = args[1].addFileExt(".nim")
      let dest = if splitFile(args[1]).dir == "": "tests/nimony/basics" / outp
                 else: outp
      record inp, dest, flags, categoryOf(dest)
    else:
      quit "`record` takes two arguments"
  of "clean":
    removeDir "nimcache"
    removeDir "bin"
    for n in 0 .. 9:
      removeDir "bin" & $n
    # The joined-group drivers are generated into the test tree itself.
    for root in ["tests", "examples"]:
      if dirExists(root):
        for f in walkDirRec(root):
          if isGeneratedTestFile(f) and f.endsWith(".nim"): removeFile f
  of "update":
    # `update <what>` rather than a bare `update`: there is more than one pin,
    # and they do not move together. `deps` is the sibling nativenif checkout;
    # `parser` is Nim's own `compiler/parser.nim` that nifler is built with,
    # whose bump can break the build on an older host install and so is never
    # something to inherit from a nativenif re-pin.
    if args.len == 1 and args[0] == "deps": updateDepsCmd()
    elif args.len == 1 and args[0] == "parser": updateParserCmd()
    else: writeHelp()
  of "install":
    runInstall(args)
  of "sync":
    syncCmd(if args.len > 0: args[0] else: "")
  of "pull":
    pullpush("pull")
  of "push":
    pullpush("push")
  else:
    if dirExists(rawPrimary):
      walkCmd(rawPrimary, forward, overwrite)
    else:
      quit "invalid command: " & primaryCmd

# Guarded even though nothing imports this module: a directory's `setup.nim`
# custom runner takes the test kit from `kit.nim`, which deliberately does not
# reach the CLI — so importing the kit can never parse a command line or run a
# command as a side effect.
when isMainModule:
  handleCmdLine()

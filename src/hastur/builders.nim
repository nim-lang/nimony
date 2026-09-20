## Building the toolchain with the host Nim. Every build writes its binary
## straight into `binDir()` via `-o:` — nothing is produced next to its source
## and moved afterwards.

import std / [syncio, os, osproc, hashes]
import context, deps, concurrent, binstamps

proc nimcPrefix*(): string =
  ## The front-end tools ship OPTIMIZED by default, exactly like arkham and
  ## nifasm (see `nativeToolPrefix`). A debug toolchain is ~8x slower at
  ## compiling (measured on `nimony n bug.nim`: nimsem 0.155s release vs
  ## 1.204s debug, hexer 0.065s vs 0.676s), which dominates every test run —
  ## and because the suites rebuild the toolchain via `setup.hastur`, an
  ## unoptimized default silently downgraded whatever the user had built
  ## before. `--debug` opts out for gdb work on the tools themselves.
  #
  # `--warningAsError:ProveInit:off` and `--warningAsError:Uninit:off`:
  # Nimony's `src/config.nims` promotes these warnings to errors, but Nim
  # 2.2.10's host stdlib (typedthreads.nim, deques.nim) trips them on
  # patterns that aren't actionable from our side. Without these overrides,
  # any tool using `createThread` (hastur itself) or `initDeque` (pnak)
  # fails to build on Nim 2.2.10.
  (if debugBuild: "nim c " else: "nim c -d:release ") &
    "--warningAsError:ProveInit:off --warningAsError:Uninit:off "

proc nativeToolPrefix*(): string =
  ## arkham + nifasm are the native backend's HOT codegen tools: they run
  ## once per module on every `nimony n` build, so unlike the other tools they
  ## ship OPTIMIZED by default. A debug build roughly doubles `nimony n` cold
  ## time (measured: nimsem 50.7s debug → 39.5s release) because — unlike the C
  ## backend, whose heavy lifting is gcc (always optimized) — the native
  ## backend's heavy lifting IS these two tools. Pass `--native-debug` to hastur
  ## to build them unoptimized instead (for `-d:arkhamDbgSym` / gdb work on the
  ## toolchain itself).
  (if nativeToolsDebug: "nim c " else: "nim c -d:release ") &
    "--warningAsError:ProveInit:off --warningAsError:Uninit:off "

proc validatePassesFlag(): string =
  ## Enable the phase-aware IR validator only when running on CI. GitHub Actions
  ## (and most other CI providers) set `CI=true` in the environment, so we key
  ## off that: locally the validator stays opt-in via `NIMONY_VALIDATE=1`, which
  ## keeps iteration fast while guaranteeing the check on every PR. On Windows
  ## CI the in-process post-sem validator is turned off — the per-module walk
  ## amplifies Windows' per-process overhead enough to dominate the test phase,
  ## and the Linux/macOS jobs already catch the same drift on every PR.
  if getEnv("CI").len > 0 or getEnv("NIMONY_VALIDATE").len > 0:
    when defined(windows):
      "-d:skipPostSemValidator "
    else:
      "-d:validatePasses "
  else:
    ""

proc toolCmd(name, source: string; extraFlags = ""): string =
  ## Build one host-Nim tool STRAIGHT into `binDir()`. `-o:` rather than
  ## compiling next to the source and moving the result afterwards: the binary
  ## only ever exists where it is used, so a build cannot leave a stray copy in
  ## `src/` for someone to run by accident, and the outcome no longer depends on
  ## whether the tool's own directory happens to carry an `--outdir` in its
  ## `nim.cfg`.
  nimcPrefix() & extraFlags & "-o:" &
    (binDir() / name.addFileExt(ExeExt)).quoteShell & " " & source

proc buildTool(name, source: string; showProgress: bool; extraFlags = "") =
  createDir binDir()
  exec toolCmd(name, source, extraFlags), showProgress

proc nativeToolCmd(tool: string): string =
  nativeToolPrefix() & "--outdir:" & binDir() & " " & NativenifDir &
    "/src/" & tool & "/" & tool & ".nim"

proc buildNifler*(showProgress = false) =
  ## Nifler parses `.nim` with Nim's OWN `compiler/parser.nim`, which this repo
  ## does not carry: `syncNimParser` checks it out at the pinned commit first,
  ## the same assume-nothing arrangement `buildArkham` has with the sibling
  ## nativenif checkout. It is a no-op once the file is on the pin, so this
  ## costs a `fileExists` on every build but the first.
  syncNimParser()
  buildTool("nifler", "src/nifler/nifler.nim", showProgress)

const
  Nifler2Cache = "nimcache_static" / "nifler2"
    ## nifler2's own `--nimcache`, out of `nimcache/`: `hastur build` wipes
    ## that one at the end, and a cold nifler2 compile is ~30s of every build.
  Nifler2Toolchain = ["nimony", "nimsem", "hexer", "lengc", "shoggoth",
                      "nifmake", "niflink", "nifler", "nifler2"]
    ## Everything that shapes what lands in `Nifler2Cache`: the driver, the
    ## phases it runs and the parsers (nifler2 parses itself once it exists).

proc toolchainStamp(tools: openArray[string]): string =
  ## Content hash of the tools, link-time stamps masked (`maskBuildStamps`):
  ## the host-Nim tools are relinked by every build, so neither their mtime
  ## nor their raw bytes say whether they changed.
  var h: Hash = 0
  for t in tools:
    h = h !& hash(t)
    let p = binDir() / t.addFileExt(ExeExt)
    if fileExists(p):
      var buf = readFile(p)
      maskBuildStamps(buf)
      h = h !& hash(buf)
  result = $(!$h)

proc buildNifler2*(showProgress = false) =
  ## nifler2 is a NIMONY program (its parser is generated by the
  ## `deps/parsegen` plugin), so it is built last, by the `bin/nimony` the
  ## other builds just produced. The first build parses nifler2 with nifler;
  ## once it exists nimony prefers it for every parse but `config` and
  ## `--docs` (`tooldirs.parserTool`), so later builds are self-parsed.
  ##
  ## The build is incremental across runs. nimony's incremental build tracks
  ## the sources but not the compiler, so the cache is only reused by exactly
  ## the toolchain that filled it — any change to one of `Nifler2Toolchain`
  ## (a macOS relink, which re-signs, counts as one) starts it cold.
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  if not fileExists(nimony):
    quit "nifler2: " & nimony & " not found; run `hastur build nimony` first"
  let stampFile = Nifler2Cache / "toolchain.stamp"
  let stamp = toolchainStamp(Nifler2Toolchain)
  if not fileExists(stampFile) or readFile(stampFile) != stamp:
    removeDir Nifler2Cache
    createDir Nifler2Cache
    writeFile stampFile, stamp
  exec nimony.quoteShell & " c -d:release --silentMake --nimcache:" &
       Nifler2Cache.quoteShell & " --out:" &
       (binDir() / "nifler2".addFileExt(ExeExt)).quoteShell &
       " src/nifler2/nifler2.nim", showProgress

proc buildNimsem*(showProgress = false) =
  buildTool("nimsem", "src/nimony/nimsem.nim", showProgress, validatePassesFlag())

proc buildNimony*(showProgress = false) =
  buildTool("nimony", "src/nimony/nimony.nim", showProgress, validatePassesFlag())

proc buildControlflow*(showProgress = false) =
  buildTool("controlflow", "src/nimony/controlflow.nim", showProgress)

proc buildContracts*(showProgress = false) =
  buildTool("contracts", "src/nimony/contracts.nim", showProgress)

proc buildLengc*(showProgress = false) =
  buildTool("lengc", "src/lengc/lengc.nim", showProgress)

proc buildShoggoth*(showProgress = false) =
  buildTool("shoggoth", "src/lengc/shoggoth/shoggoth.nim", showProgress)

proc buildNiflink*(showProgress = false) =
  ## `niflink` (the C-backend link driver) reads a link manifest NIF and links
  ## the project; built on the nifcore API.
  buildTool("niflink", "src/niflink/niflink.nim", showProgress)

proc buildArkham*(showProgress = false) =
  ## `arkham` (Leng -> typed asm-NIF native codegen) lives in the sibling
  ## `../nativenif` repo and reuses nimony's NIF libraries via its committed
  ## sibling-relative `nim.cfg`. `syncNativenif` clones the checkout if it is
  ## missing and puts it on the `src/nativenif.commit` pin, so asking for arkham
  ## by name is enough. arkham's own `nim.cfg` already sets `--outdir:bin`; we
  ## pass it explicitly so the result is deterministic regardless of the current
  ## directory.
  syncNativenif()
  createDir binDir()
  exec nativeToolCmd("arkham"), showProgress

proc buildNifasm*(showProgress = false) =
  ## `nifasm` (asm-NIF -> static, libc-free ELF/Mach-O/PE executable; also the
  ## linker) — sibling repo, same assume-exists arrangement as `buildArkham`.
  syncNativenif()
  createDir binDir()
  exec nativeToolCmd("nifasm"), showProgress

proc buildJorogumo*(showProgress = false) =
  ## `jorogumo` — the whole web back end in ONE binary: `jorogumo j` renders a
  ## Leng program as a self-contained `.js` file, `jorogumo w` as a
  ## whole-program `.wasm` module. Sibling repo, same assume-exists arrangement
  ## as `buildArkham`.
  ##
  ## There is no separate wasm tool to build any more (`ithaqua` was folded into
  ## `../nativenif/src/web` — one code generator, two renderers), so this single
  ## proc is what `nimony j`, `nimony w` and `hastur wasmdiff` all ask for. It
  ## stays off the default build the way arkham/nifasm once did.
  syncNativenif()
  createDir binDir()
  exec nativeToolCmd("jorogumo"), showProgress

proc hostGetsNativeTools(): bool =
  ## Does `build all` build arkham + nifasm here? They are part of the
  ## toolchain now — a native boot and every `nimony n` need them in `bin/`,
  ## and leaving them to a separate opt-in command meant each caller had to
  ## remember (CI did not, and the Windows bootstrap of #2325 quietly kept
  ## using the C backend for it).
  ##
  ## Two hosts do not get them, and both say so instead of failing the build:
  ## there is no native target for a 32-bit host, and the sibling checkout is
  ## something a plain `git clone` of this repo does not bring. Asking for them
  ## by name (`build native`, `build arkham`, `hastur native`) still goes
  ## straight at `buildArkham`, whose `syncNativenif` CLONES the sibling when
  ## it is missing — an explicit request earns a ~100 MB fetch, a blanket
  ## `all` does not, which is why the `dirExists` guard stays here rather than
  ## moving down into `syncNativenif`.
  when defined(cpu64):
    result = dirExists(NativenifDir)
    if not result:
      echo "[build] ", NativenifDir, " not found — skipping arkham + nifasm; ",
           "`nimony n` and a native boot need it (clone nim-lang/nativenif next to this repo)"
  else:
    echo "[build] no native backend target for this host — skipping arkham + nifasm"
    result = false

proc buildHexer*(showProgress = false) =
  buildTool("hexer", "src/hexer/hexer.nim", showProgress)

proc buildNifmake*(showProgress = false) =
  buildTool("nifmake", "src/nifmake/nifmake.nim", showProgress)

proc buildNifbench*(showProgress = false) =
  ## The host-Nim build of `nifbench`, which is the BASELINE column: the point
  ## of the tool is to compile the same source with `nim c`, `nimony c` and
  ## `nimony n` and diff the per-phase timings, so this one has to exist for
  ## the other two to mean anything. Kept out of `all` — it is a measuring
  ## instrument, not part of the toolchain.
  buildTool("nifbench", "bench/nifbench.nim", showProgress)

proc buildValidator*(showProgress = false) =
  buildTool("validator", "src/validator/validator.nim", showProgress)

proc buildDagon*(showProgress = false) =
  buildTool("dagon", "src/dagon/dagon.nim", showProgress)

proc buildPnak*(showProgress = false) =
  buildTool("pnak", "src/pnak/pnak.nim", showProgress)

proc buildNimonyToolchain*(showProgress = false) =
  ## Rebuild every host-Nim-compiled binary that shares `src/nimony/programs.nim`
  ## (or any other module reused across compiler stages). A change to a shared
  ## helper like `suffixToNif` only takes effect once nimony, nimsem AND hexer
  ## are all re-linked, so `hastur selfcheck` (and any caller that wants a
  ## fully-consistent toolchain) goes through this rather than `buildNimony`
  ## alone — which is what masked a hexer bug during the doc-generator work.
  ## lengc and shoggoth are here because `boot` self-hosts them as well.
  buildNimsem(showProgress)
  buildNimony(showProgress)
  buildHexer(showProgress)
  buildLengc(showProgress)
  buildShoggoth(showProgress)

proc buildAll*() =
  ## `build all`: every host-Nim tool at once, then nifler2, which is compiled
  ## by the nimony (and nimsem, hexer, …) the first step produced. The host
  ## builds are independent — each has its own `-o:` and Nim keeps a nimcache
  ## per project — and one at a time they were ~40s of every tree walk, most
  ## of it on one core. Each tool's output is printed in one piece as it ends.
  syncNimParser()
  var cmds = @[
    toolCmd("nifler", "src/nifler/nifler.nim"),
    toolCmd("nimsem", "src/nimony/nimsem.nim", validatePassesFlag()),
    toolCmd("nimony", "src/nimony/nimony.nim", validatePassesFlag()),
    toolCmd("lengc", "src/lengc/lengc.nim"),
    toolCmd("shoggoth", "src/lengc/shoggoth/shoggoth.nim"),
    toolCmd("niflink", "src/niflink/niflink.nim"),
    toolCmd("hexer", "src/hexer/hexer.nim"),
    toolCmd("nifmake", "src/nifmake/nifmake.nim"),
    toolCmd("validator", "src/validator/validator.nim"),
    toolCmd("dagon", "src/dagon/dagon.nim"),
    toolCmd("pnak", "src/pnak/pnak.nim")]
  if hostGetsNativeTools():
    syncNativenif()
    cmds.add nativeToolCmd("arkham")
    cmds.add nativeToolCmd("nifasm")
  createDir binDir()
  let exitCodes = runConcurrently(cmds)
  for i in 0 ..< cmds.len:
    if exitCodes[i] != 0:
      quit "FAILURE " & cmds[i] & "\n"
  buildNifler2(showProgress = true)

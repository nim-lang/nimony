## The pinned sibling `../nativenif` checkout arkham and nifasm are built
## from: reading the pin, putting the checkout on it, and moving it.

import std / [syncio, os, osproc, strutils]
import context

# ---- the pinned `nativenif` dependency -------------------------------------
# arkham and nifasm are built from a sibling repo, so "which nativenif" is an
# input to every native build the same way a source file is — and until this
# pin existed it was whatever each machine happened to have checked out. That
# is not a hypothetical: a native-suite failure could mean a real gap, a
# nativenif commit newer than the one the tests were recorded against, or a
# local branch someone was mid-way through. `src/nativenif.commit` makes it one
# answer, recorded in this repo and moved deliberately by `hastur update deps`.

const
  NativenifDir* = "../nativenif"
    ## Sibling checkout arkham + nifasm live in. Their committed `nim.cfg`s
    ## reach back here through `../../../nimony/src/lib`, so the layout — and
    ## this repo's directory name — is already load-bearing.
  NativenifUrl* = "https://github.com/nim-lang/nativenif"
    ## Cloned from over HTTPS rather than SSH: the auto-clone below has to work
    ## on a machine that has never pushed to this repo — CI, or someone who
    ## just cloned nimony — and `git@github.com:` needs a key for a read that
    ## does not. A checkout that already exists is never re-pointed, so anyone
    ## who prefers SSH keeps their own remote.
  NativenifCommitFile* = "src/nativenif.commit"
    ## Under `src/` with the rest of what a build is made of — and so a change to
    ## it invalidates the CI artifact cache, whose key hashes `src/**`. Read
    ## relative to the project root, which is where every hastur command runs.
    ##
    ## Contents: the commit hash, then whitespace, then the commit's date. The
    ## date is for readability only — a diff moving the pin should say *when* it
    ## moved to without anyone running `git show` — so nothing checks it and
    ## only the first field is ever compared.

proc firstField(content: string): string =
  ## The commit hash out of a pin file: everything after the first whitespace is
  ## the human-readable date (and any other note someone leaves there), dropped
  ## here so it can never make a pin that matches HEAD look like one that does
  ## not. Also what turns `git ls-remote`'s "<sha>\trefs/heads/devel" into a sha.
  let s = content.strip
  let cut = s.find({' ', '\t', '\n', '\r'})
  result = if cut < 0: s else: s[0 ..< cut]

proc readPin(file: string): string =
  ## A pin is read at runtime, not compiled in: a `slurp`ed copy would let the
  ## file and the binary disagree, so `git checkout <pin>` and `hastur update
  ## deps` would go on enforcing and reporting whatever commit the binary was
  ## last built from — silently, and worst on a stale `bin/hastur`, which is the
  ## exact failure mode these pins exist to end. Missing or empty file means "no
  ## pin", each pin's own escape hatch.
  if fileExists(file): firstField(readFile(file)) else: ""

proc pinnedNativenifCommit(): string =
  ## No pin means "build whatever is checked out" — the escape hatch for
  ## bisecting nativenif itself.
  readPin(NativenifCommitFile)

proc gitIn(dir, args: string): (string, int) =
  execCmdEx("git -C " & dir.quoteShell & " " & args)

proc commitDate(dir, commit: string): string =
  ## Committer date of `commit`, `YYYY-MM-DD`, for the pin file's comment field.
  ## Best-effort: a date we cannot read is simply left out rather than fatal —
  ## nothing reads it back.
  let (dateOut, dateCode) = gitIn(dir, "show --no-patch --format=%cd --date=short " & commit)
  if dateCode == 0: dateOut.strip else: ""

proc cloneNativenif(): bool =
  ## Clone the sibling checkout when it is not there yet, and say whether we
  ## did. arkham and nifasm are part of the toolchain, so "clone nim-lang/
  ## nativenif next to this repo" was a build error that had exactly one
  ## remedy — and every caller of it was a person typing the same command
  ## back. Do it for them.
  ##
  ## Deliberately not shallow: the pin is regularly a commit behind whatever
  ## the default branch has, and `--depth 1` would fetch a history that cannot
  ## reach it — so the checkout below would immediately have to deepen again.
  ## The repo is small enough (a couple of hundred commits) that a full clone
  ## is the cheaper of the two.
  if dirExists(NativenifDir): return false
  echo "[deps] cloning ", NativenifUrl, " into ", NativenifDir
  let (cloneOut, cloneCode) = execCmdEx(
    "git clone --quiet " & NativenifUrl.quoteShell & " " & NativenifDir.quoteShell)
  if cloneCode != 0:
    quit "hastur: cannot clone " & NativenifUrl & " into " & NativenifDir &
         " (it holds arkham + nifasm):\n" & cloneOut
  result = true

var nativenifChecked = false

proc pinFailure(pin, head, why, remedy: string) {.noreturn.} =
  quit "hastur: " & NativenifDir & " is at " & head & ", but " &
       NativenifCommitFile & " pins " & pin & ":\n" & why & "\n" & remedy &
       "\n(or `hastur update deps` to re-pin to " & head & ")"

proc syncNativenif*() =
  ## Put `../nativenif` on the pinned commit before anything is built from it,
  ## and FAIL the build when that cannot be done. arkham and nifasm built from
  ## some other commit is a build whose test results mean nothing, so every
  ## way of not being on the pin — a malformed pin, a tree that is not a git
  ## checkout, uncommitted work, a branch someone is working on, a commit that
  ## cannot be fetched — is an error, never a warning.
  ##
  ## A clean, detached tree (CI, an auto-clone, a previous run of this proc) is
  ## moved onto the pin. A tree with uncommitted changes or on a branch is
  ## someone's work and is NOT moved: the build stops and says how to proceed.
  ## The pin's own escape hatch stays: an empty or absent pin file builds
  ## whatever is checked out (for bisecting nativenif itself).
  if nativenifChecked: return
  nativenifChecked = true
  # The clone comes first and is not conditional on the pin: whether the
  # checkout exists and which commit it sits on are two different questions,
  # and an unpinned tree still has to be there to be built from.
  let justCloned = cloneNativenif()
  let pin = pinnedNativenifCommit()
  if pin.len == 0: return
  if pin.len != 40 or not pin.allCharsInSet(HexDigits):
    quit "hastur: " & NativenifCommitFile & " does not hold a commit hash: '" & pin &
         "'\nExpected a full 40-digit SHA-1 (`hastur update deps` writes one)."
  let (headOut, headCode) = gitIn(NativenifDir, "rev-parse HEAD")
  if headCode != 0:
    quit "hastur: " & NativenifDir & " is not a git checkout, so it cannot be verified to be at " &
         pin & " (pinned by " & NativenifCommitFile & "):\n" & headOut
  let head = headOut.strip
  if cmpIgnoreCase(head, pin) == 0: return

  let (dirty, dirtyCode) = gitIn(NativenifDir, "status --porcelain --untracked-files=no")
  if dirtyCode != 0 or dirty.strip.len > 0:
    pinFailure(pin, head, "it has uncommitted changes, which are not moved for you.",
               "Commit or stash them, then `git -C " & NativenifDir &
               " checkout --detach " & pin & "`")

  # A checkout sitting on a branch is someone's working tree, and committing a
  # fix there is exactly what makes it stop being dirty — so silently detaching
  # it would build the arkham that fix was replacing. Stop instead. CI and a
  # fresh auto-clone are on their default branch by construction, not because
  # someone is working in them, so those are moved like a detached tree.
  let (branch, branchCode) = gitIn(NativenifDir, "symbolic-ref --quiet --short HEAD")
  if branchCode == 0 and branch.strip.len > 0 and getEnv("CI").len == 0 and
     not justCloned:
    pinFailure(pin, head, "it is on branch '" & branch.strip & "', which is not moved for you.",
               "To build the pin: `git -C " & NativenifDir & " checkout --detach " & pin & "`")

  # A checkout that never had the pin — a shallow CI clone of the default
  # branch, or a tree that has not fetched in a while. Ask for the commit by
  # name first: github.com serves any SHA reachable from a ref, which is the
  # cheapest way to get exactly the one object we are after.
  if gitIn(NativenifDir, "cat-file -e " & pin & "^{commit}")[1] != 0:
    echo "[deps] fetching ", pin, " into ", NativenifDir
    if gitIn(NativenifDir, "fetch --no-tags origin " & pin)[1] != 0:
      # A server that refuses the object by name (git's default for anything
      # but github.com: `uploadpack.allowReachableSHA1InWant`) still serves the
      # refs, but on a shallow clone their history stops above the pin — so
      # deepen, or fetching the branch tips would not bring it either.
      let shallow = gitIn(NativenifDir, "rev-parse --is-shallow-repository")[0].strip
      discard gitIn(NativenifDir,
                    if shallow == "true": "fetch --no-tags --unshallow origin"
                    else: "fetch --no-tags origin")
    let (missing, missingCode) = gitIn(NativenifDir, "cat-file -e " & pin & "^{commit}")
    if missingCode != 0:
      quit "hastur: " & NativenifCommitFile & " pins " & pin &
           ", which " & NativenifDir & " cannot fetch:\n" & missing &
           "\nIs the commit pushed? `hastur update deps` re-pins to the local HEAD."

  echo "[deps] nativenif: ", head, " -> ", pin, " (pinned by ", NativenifCommitFile, ")"
  let (coOut, coCode) = gitIn(NativenifDir, "checkout --detach --quiet " & pin)
  if coCode != 0:
    quit "hastur: cannot check out " & pin & " in " & NativenifDir & ":\n" & coOut
  let after = gitIn(NativenifDir, "rev-parse HEAD")[0].strip
  if cmpIgnoreCase(after, pin) != 0:
    quit "hastur: " & NativenifDir & " is at " & after & " after checking out " & pin

# ---- Nim's own parser, checked out for nifler ------------------------------
# Nifler parses `.nim` source with the host compiler's parser: `src/nifler/nim.cfg`
# puts `$nim` on the search path and `bridge.nim` imports `compiler / syntaxes`.
# The toolchain can therefore only read the syntax that compiler knows — and one
# construct Nimony's own stdlib uses, concept refinement
#
#   IntegerArithmetic* = concept of Arithmetic          # lib/std/math.nim:14
#
# is in no release: Nim 2.2.10 answers `identifier expected, but got 'keyword of'`
# and every module that reaches `std/math` — which through the plugins is most of
# the tree — fails to parse, long before anything semantic.
#
# `src/nifler/config.nims` redirects the module with `patchFile` to
# `nimparser/parser.nim`. That file is not in this repo and must not be: a copy
# of somebody else's source checked in here is a fork, and a fork nobody
# remembers to re-sync. `hastur build nifler` checks it out instead, at the
# commit `upstream.commit` pins — so what nifler parses with is a recorded,
# reviewable input exactly like `src/nativenif.commit`, and `hastur update
# parser` is the one sanctioned way to move it.

const
  NimParserDir* = "src" / "nifler" / "nimparser"
  NimParserFile* = NimParserDir / "parser.nim"
    ## Where `config.nims`' `patchFile` looks. Git-ignored: it is Nim's file,
    ## not ours.
  NimParserStampFile* = NimParserDir / "parser.fetched"
    ## Which commit `parser.nim` was checked out at. Without it the only way to
    ## answer "is this file the pin?" is to fetch and diff, i.e. every build
    ## would go to the network. Git-ignored alongside the file it describes.
  NimParserCommitFile* = NimParserDir / "upstream.commit"
    ## The pin, in `src/nativenif.commit`'s format: hash, whitespace, date.
    ## Tracked — this is the reviewable half.
  NimRepoUrl* = "https://github.com/nim-lang/Nim"
  NimDevelBranch* = "devel"
  NimParserInRepo = "compiler/parser.nim"
    ## Its path inside the Nim tree — used both as a `git show` object path and
    ## as the tail of the raw-content URL, so it stays spelled with a forward
    ## slash on every host.

proc hostNimTree(): string =
  ## The host compiler's own source checkout, when it has one: `nim` in PATH is
  ## `<tree>/bin/nim`, and a `<tree>/.git` means the blob may already be on this
  ## disk. Worth the lookup — a Nim developer's install is a checkout, so the
  ## common case needs no network at all — and cheap to be wrong about, since
  ## the download below is the fallback either way.
  let exe = findExe("nim")
  if exe.len == 0: return ""
  var resolved = exe
  # choosenim (and a hand-made `~/bin/nim`) put a symlink in PATH; the tree we
  # are after is above the real file, not above the link.
  try: resolved = expandFilename(exe)
  except OSError, IOError: discard
  let tree = resolved.parentDir.parentDir
  # `.git` is a file, not a directory, in a worktree or a submodule.
  result = if dirExists(tree / ".git") or fileExists(tree / ".git"): tree else: ""

proc nimFileFromCheckout(tree, pin, inRepo, dest: string): bool =
  ## `git show <pin>:<inRepo>` out of a Nim checkout that already has the
  ## commit. Deliberately never fetches into it: that tree is someone's working
  ## repo — we read one object out of it and touch nothing else. A commit it
  ## does not have is not an error here, just a "no".
  if gitIn(tree, "cat-file -e " & pin & ":" & inRepo)[1] != 0: return false
  let (blob, code) = gitIn(tree, "show " & pin & ":" & inRepo)
  if code != 0 or blob.len == 0: return false
  writeFile(dest, blob)
  result = true

proc downloadNimFile(pin, inRepo, dest: string): bool =
  ## One file over HTTPS, by exact commit — the whole Nim repo is a ~400 MB
  ## clone to reach 90 KB, and `git archive --remote` is not something github.com
  ## serves. Written through a temporary so a failed transfer cannot leave a
  ## truncated file behind for the next build to compile.
  let curl = findExe("curl")
  if curl.len == 0: return false
  let url = "https://raw.githubusercontent.com/nim-lang/Nim/" & pin & "/" & inRepo
  let tmp = dest & ".tmp"
  let code = execCmd(curl.quoteShell & " -sSL --fail -o " & tmp.quoteShell & " " & url.quoteShell)
  if code != 0 or not fileExists(tmp) or getFileSize(tmp) == 0:
    removeFile tmp
    return false
  moveFile(tmp, dest)
  result = true

proc fetchNimFiles(what, pin: string; files: openArray[(string, string)]): bool =
  ## Every `(inRepo, dest)` of `files` at Nim commit `pin`: from a Nim checkout
  ## on this machine when it has the commit, else over HTTPS. All or nothing as
  ## far as the caller's stamp is concerned: false if any file is missing.
  let tree = hostNimTree()
  var fromTree = tree.len > 0
  for (inRepo, _) in files:
    if fromTree and gitIn(tree, "cat-file -e " & pin & ":" & inRepo)[1] != 0:
      fromTree = false
  if fromTree:
    result = true
    for (inRepo, dest) in files:
      if not nimFileFromCheckout(tree, pin, inRepo, dest): result = false
    if result:
      echo "[deps] ", what, ": ", pin[0 ..< min(pin.len, 12)], " from ", tree
      return
  echo "[deps] ", what, ": fetching ", pin[0 ..< min(pin.len, 12)], " from ", NimRepoUrl
  result = true
  for (inRepo, dest) in files:
    if not downloadNimFile(pin, inRepo, dest): result = false

var nimParserChecked = false

proc syncNimParser*() =
  ## Put `nimparser/parser.nim` on the pinned commit before nifler is built from
  ## it. Called by `buildNifler` the way `syncNativenif` is called by
  ## `buildArkham`: asking for the tool by name is enough.
  if nimParserChecked: return
  nimParserChecked = true
  let pin = readPin(NimParserCommitFile)
  if pin.len == 0:
    # No pin is the escape hatch, and it is a real one: `config.nims` applies
    # the patch only if the file is there, so an unpinned tree with no
    # `parser.nim` builds nifler against the host compiler's own parser — which
    # is exactly right on a `devel` install, and the only way out if a pinned
    # parser ever stops compiling against a newer `$nim/compiler`.
    return
  if fileExists(NimParserFile) and readPin(NimParserStampFile) == pin: return

  createDir NimParserDir
  if fetchNimFiles("nifler parser", pin, [(NimParserInRepo, NimParserFile)]):
    writeFile(NimParserStampFile, pin & "\n")
    return

  # Everything below is a warning and not a `quit`: a build that cannot reach
  # the network is still a build, and the host's parser may well be new enough.
  echo "[deps] WARNING: cannot check out ", NimParserInRepo, " @ ", pin,
       " (no Nim checkout holding it, and `curl` failed or is missing)"
  if fileExists(NimParserFile):
    let had = readPin(NimParserStampFile)
    echo "[deps] WARNING: keeping the copy in ", NimParserDir, " — it is ",
         (if had.len > 0: had else: "of unrecorded origin"), ", not the pin"
  else:
    echo "[deps] WARNING: nifler will parse with the HOST compiler's parser; a ",
         "release Nim cannot read `concept of` (lib/std/math.nim) and most of ",
         "the suite will fail to parse"

proc updateParserCmd*() =
  ## `hastur update parser`: re-pin `src/nifler/nimparser/upstream.commit` to the
  ## tip of nim-lang/Nim `devel`, then check that commit out right away — so the
  ## pin is proven fetchable before anyone commits it.
  ##
  ## Its own command rather than a rider on `update deps`: moving this pin has a
  ## failure mode moving nativenif's does not. The parser is compiled against
  ## whatever `$nim/compiler` the host ships, so a newer one can stop building on
  ## an older install — that is a deliberate decision, not something to inherit
  ## from a nativenif re-pin.
  let (lsOut, lsCode) = execCmdEx("git ls-remote " & NimRepoUrl.quoteShell & " " & NimDevelBranch)
  let tip = firstField(lsOut)
  if lsCode != 0 or tip.len != 40 or not tip.allCharsInSet(HexDigits):
    quit "hastur update parser: cannot resolve " & NimRepoUrl & " " & NimDevelBranch & ":\n" & lsOut
  let pin = readPin(NimParserCommitFile)

  # The date is for readability only (same as `src/nativenif.commit`), so a machine
  # with no Nim checkout simply writes the hash alone.
  let tree = hostNimTree()
  var date = ""
  if tree.len > 0 and gitIn(tree, "cat-file -e " & tip & "^{commit}")[1] == 0:
    date = commitDate(tree, tip)
  writeFile(NimParserCommitFile,
            (if date.len > 0: tip & " " & date else: tip) & "\n")
  echo "[deps] ", NimParserCommitFile, ": ",
       (if pin.len > 0: pin else: "(unpinned)"), " -> ", tip,
       (if date.len > 0: " (" & date & ")" else: "")
  syncNimParser()

proc updateDepsCmd*() =
  ## `hastur update deps`: re-pin `src/nativenif.commit` to whatever `../nativenif`
  ## has checked out right now. The one sanctioned way to move the pin, so that
  ## "the native backend needs a newer arkham" is a reviewable one-line diff
  ## next to the test results that needed it.
  if not dirExists(NativenifDir):
    quit "hastur update deps: " & NativenifDir & " not found"
  let (headOut, headCode) = gitIn(NativenifDir, "rev-parse HEAD")
  if headCode != 0:
    quit "hastur update deps: " & NativenifDir & " is not a git checkout"
  let head = headOut.strip

  let (dirty, _) = gitIn(NativenifDir, "status --porcelain --untracked-files=no")
  if dirty.strip.len > 0:
    echo "[deps] WARNING: ", NativenifDir, " has uncommitted changes; ", head,
         " does NOT include them"
  let (remotes, remotesCode) = gitIn(NativenifDir, "branch --remotes --contains " & head)
  if remotesCode != 0 or remotes.strip.len == 0:
    echo "[deps] WARNING: ", head, " is on no remote branch — push it, ",
         "or CI cannot fetch the pin"

  let pin = pinnedNativenifCommit()
  let date = commitDate(NativenifDir, head)
  let line = (if date.len > 0: head & " " & date else: head) & "\n"
  if head == pin:
    # Same commit, but the file may predate the date field (or carry a stale
    # one, e.g. after a rebase changed the committer date): rewrite it rather
    # than leave the comment disagreeing with the hash it annotates.
    if not fileExists(NativenifCommitFile) or readFile(NativenifCommitFile) != line:
      writeFile(NativenifCommitFile, line)
      echo "[deps] ", NativenifCommitFile, ": still ", head, ", date -> ", date
    else:
      echo "[deps] ", NativenifCommitFile, " already pins ", head
    return
  writeFile(NativenifCommitFile, line)
  echo "[deps] ", NativenifCommitFile, ": ",
       (if pin.len > 0: pin else: "(unpinned)"), " -> ", head,
       (if date.len > 0: " (" & date & ")" else: "")

const BootNativeTools* = ["arkham", "nifasm"]
  ## Carried too for a native boot: `nimony n` reaches them through `findTool`,
  ## i.e. relative to the running nimony, so without them in `binN/` stage N
  ## would silently drive `bin/`'s copies.

proc missingNativeTools*(): seq[string] =
  ## Which of `BootNativeTools` are not in `bin/`. They come from the sibling
  ## `../nativenif`, so a checkout that never had it has neither.
  result = @[]
  for tool in BootNativeTools:
    if not fileExists(binDir() / tool.addFileExt(ExeExt)): result.add tool

# ---- Nim's cycle collectors, included by `system/orc` and `system/yrc` ------
# `--mm:orc` and `--mm:yrc` run Nim's own `lib/system/orc.nim` and `yrc.nim`
# (they say `when defined(nimony)` where the compilers differ), so a fix to
# either collector lands once, in Nim. They are copied into
# `lib/std/system/upstream/` (git-ignored) at the commit
# `lib/std/system/upstream.commit` pins, the same arrangement as the parser
# above. Moving the pin is a plain edit: point it at a pushed Nim commit.

const
  NimRuntimeDir* = "lib" / "std" / "system" / "upstream"
  NimRuntimeCommitFile* = "lib" / "std" / "system" / "upstream.commit"
    ## Tracked. Next to the directory rather than in it: the directory is
    ## git-ignored as a whole.
  NimRuntimeStampFile = NimRuntimeDir / "upstream.fetched"
  NimRuntimeFiles = ["orc.nim", "yrc.nim", "cellseqs_v2.nim"]
    ## In `lib/system/` of the Nim tree, under the same names here.

var nimRuntimeChecked = false

proc syncNimRuntime*() =
  ## Put `lib/std/system/upstream/` on the pinned commit. Called wherever
  ## nimony is built: the copies are part of the stdlib that build serves, and
  ## without them `--mm:orc`/`--mm:yrc` programs do not compile.
  if nimRuntimeChecked: return
  nimRuntimeChecked = true
  let pin = readPin(NimRuntimeCommitFile)
  if pin.len == 0: return # no pin: keep whatever is in the directory
  var present = readPin(NimRuntimeStampFile) == pin
  for f in NimRuntimeFiles:
    if not fileExists(NimRuntimeDir / f): present = false
  if present: return

  createDir NimRuntimeDir
  var files: seq[(string, string)] = @[]
  for f in NimRuntimeFiles:
    files.add ("lib/system/" & f, NimRuntimeDir / f)
  if fetchNimFiles("orc/yrc runtime", pin, files):
    writeFile(NimRuntimeStampFile, pin & "\n")
  else:
    # a warning, like the parser's: only `--mm:orc`/`--mm:yrc` need the files
    echo "[deps] WARNING: cannot check out Nim's lib/system/{orc,yrc,cellseqs_v2}.nim @ ",
         pin, " (no Nim checkout holding it, and `curl` failed or is missing); ",
         "--mm:orc and --mm:yrc will not compile"

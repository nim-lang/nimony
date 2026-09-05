## The pinned sibling `../nativenif` checkout arkham and nifasm are built
## from: reading the pin, putting the checkout on it, and moving it.

import std / [syncio, os, osproc, strutils]
import context

# ---- the pinned `nativenif` dependency -------------------------------------
# arkham and nifasm are built from a SIBLING repo, so "which nativenif" is an
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
    ## date is READABILITY ONLY — a diff moving the pin should say *when* it
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
  ## A pin is read at RUNTIME, not compiled in: a `slurp`ed copy would let the
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
  ## Deliberately NOT shallow: the pin is regularly a commit behind whatever
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

proc syncNativenif*() =
  ## Put `../nativenif` on the pinned commit before anything is built from it.
  ##
  ## Never at the cost of uncommitted work: a dirty tree is left alone with a
  ## warning, because the person who dirtied it is precisely the person who
  ## wants arkham built from THEIR edits. A clean tree gets detached onto the
  ## pin (the branch that was there is still a `git switch -` away).
  if nativenifChecked: return
  nativenifChecked = true
  # The clone comes first and is not conditional on the pin: whether the
  # checkout EXISTS and which commit it sits on are two different questions,
  # and an unpinned tree still has to be there to be built from.
  let justCloned = cloneNativenif()
  let pin = pinnedNativenifCommit()
  if pin.len == 0: return
  let (headOut, headCode) = gitIn(NativenifDir, "rev-parse HEAD")
  if headCode != 0:
    echo "[deps] ", NativenifDir, " is not a git checkout; building it as it is"
    return
  let head = headOut.strip
  if head == pin: return

  let (dirty, dirtyCode) = gitIn(NativenifDir, "status --porcelain --untracked-files=no")
  if dirtyCode != 0 or dirty.strip.len > 0:
    echo "[deps] WARNING: ", NativenifDir, " has uncommitted changes — leaving it at ",
         head
    echo "[deps] WARNING: arkham/nifasm are built from YOUR tree, not from the ",
         NativenifCommitFile, " pin (", pin, ")"
    return

  # A checkout sitting on a BRANCH is someone's working tree, and committing a
  # fix there is exactly what makes it stop being dirty — so the guard above
  # would hand it straight to the `git checkout` below and build the arkham that
  # fix was replacing. Leave it be and say so. CI is the one place that must
  # enforce the pin regardless: its checkout is on the default branch too, and a
  # pin deliberately BEHIND that branch is the whole point of pinning.
  # `justCloned` is the third exemption alongside CI: a clone made two
  # statements ago is on its default branch by construction, not because
  # someone is working in it, so leaving it there would mean the very first
  # build after an auto-clone quietly ignored the pin.
  let (branch, branchCode) = gitIn(NativenifDir, "symbolic-ref --quiet --short HEAD")
  if branchCode == 0 and branch.strip.len > 0 and getEnv("CI").len == 0 and
     not justCloned:
    echo "[deps] WARNING: ", NativenifDir, " is on branch '", branch.strip, "' at ", head
    echo "[deps] WARNING: leaving it there; ", NativenifCommitFile, " pins ", pin
    echo "[deps] to build the pin instead: git -C ", NativenifDir,
         " checkout --detach ", pin
    return

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

# ---- Nim's own parser, checked out for nifler ------------------------------
# Nifler parses `.nim` source with the HOST compiler's parser: `src/nifler/nim.cfg`
# puts `$nim` on the search path and `bridge.nim` imports `compiler / syntaxes`.
# The toolchain can therefore only read the syntax that compiler knows — and one
# construct Nimony's own stdlib uses, concept refinement
#
#   IntegerArithmetic* = concept of Arithmetic          # lib/std/math.nim:14
#
# is in no release: Nim 2.2.10 answers `identifier expected, but got 'keyword of'`
# and every module that reaches `std/math` — which through the plugins is most of
# the tree — fails to PARSE, long before anything semantic.
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
    ## Which commit `parser.nim` was checked out AT. Without it the only way to
    ## answer "is this file the pin?" is to fetch and diff, i.e. every build
    ## would go to the network. Git-ignored alongside the file it describes.
  NimParserCommitFile* = NimParserDir / "upstream.commit"
    ## The pin, in `src/nativenif.commit`'s format: hash, whitespace, date.
    ## Tracked — this is the reviewable half.
  NimRepoUrl* = "https://github.com/nim-lang/Nim"
  NimDevelBranch* = "devel"
  NimParserInRepo = "compiler/parser.nim"
    ## Its path INSIDE the Nim tree — used both as a `git show` object path and
    ## as the tail of the raw-content URL, so it stays spelled with a forward
    ## slash on every host.

proc hostNimTree(): string =
  ## The host compiler's own SOURCE checkout, when it has one: `nim` in PATH is
  ## `<tree>/bin/nim`, and a `<tree>/.git` means the blob may already be on this
  ## disk. Worth the lookup — a Nim developer's install IS a checkout, so the
  ## common case needs no network at all — and cheap to be wrong about, since
  ## the download below is the fallback either way.
  let exe = findExe("nim")
  if exe.len == 0: return ""
  var resolved = exe
  # choosenim (and a hand-made `~/bin/nim`) put a SYMLINK in PATH; the tree we
  # are after is above the real file, not above the link.
  try: resolved = expandFilename(exe)
  except OSError, IOError: discard
  let tree = resolved.parentDir.parentDir
  # `.git` is a FILE, not a directory, in a worktree or a submodule.
  result = if dirExists(tree / ".git") or fileExists(tree / ".git"): tree else: ""

proc parserFromCheckout(tree, pin: string): bool =
  ## `git show <pin>:compiler/parser.nim` out of a Nim checkout that already has
  ## the commit. Deliberately never FETCHES into it: that tree is someone's
  ## working repo — we read one object out of it and touch nothing else. A
  ## commit it does not have is not an error here, just a "no".
  if gitIn(tree, "cat-file -e " & pin & ":" & NimParserInRepo)[1] != 0: return false
  let (blob, code) = gitIn(tree, "show " & pin & ":" & NimParserInRepo)
  if code != 0 or blob.len == 0: return false
  writeFile(NimParserFile, blob)
  echo "[deps] nifler parser: ", pin[0 ..< min(pin.len, 12)], " from ", tree
  result = true

proc downloadParser(pin: string): bool =
  ## One file over HTTPS, by exact commit — the whole Nim repo is a ~400 MB
  ## clone to reach 90 KB, and `git archive --remote` is not something github.com
  ## serves. Written through a temporary so a failed transfer cannot leave a
  ## truncated parser behind for the next build to compile.
  let curl = findExe("curl")
  if curl.len == 0: return false
  let url = "https://raw.githubusercontent.com/nim-lang/Nim/" & pin & "/" & NimParserInRepo
  let tmp = NimParserFile & ".tmp"
  echo "[deps] nifler parser: fetching ", pin[0 ..< min(pin.len, 12)], " from ", NimRepoUrl
  let code = execCmd(curl.quoteShell & " -sSL --fail -o " & tmp.quoteShell & " " & url.quoteShell)
  if code != 0 or not fileExists(tmp) or getFileSize(tmp) == 0:
    removeFile tmp
    return false
  moveFile(tmp, NimParserFile)
  result = true

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
    # the patch only if the file is THERE, so an unpinned tree with no
    # `parser.nim` builds nifler against the host compiler's own parser — which
    # is exactly right on a `devel` install, and the only way out if a pinned
    # parser ever stops compiling against a newer `$nim/compiler`.
    return
  if fileExists(NimParserFile) and readPin(NimParserStampFile) == pin: return

  createDir NimParserDir
  var ok = false
  let tree = hostNimTree()
  if tree.len > 0: ok = parserFromCheckout(tree, pin)
  if not ok: ok = downloadParser(pin)
  if ok:
    writeFile(NimParserStampFile, pin & "\n")
    return

  # Everything below is a WARNING and not a `quit`: a build that cannot reach
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

  # The date is READABILITY ONLY (same as `src/nativenif.commit`), so a machine
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

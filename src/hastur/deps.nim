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

proc pinnedNativenifCommit(): string =
  ## The pin is read at RUNTIME, not compiled in: a `slurp`ed copy would let the
  ## file and the binary disagree, so `git checkout <pin>` and `hastur update
  ## deps` would go on enforcing and reporting whatever commit the binary was
  ## last built from — silently, and worst on a stale `bin/hastur`, which is the
  ## exact failure mode this pin exists to end. Missing or empty file means "no
  ## pin": build whatever is checked out, the escape hatch for bisecting
  ## nativenif itself.
  ##
  ## Everything after the first whitespace is the human-readable date (and any
  ## other note someone leaves there): dropped here, so it can never make a pin
  ## that matches HEAD look like one that does not.
  if fileExists(NativenifCommitFile):
    let content = readFile(NativenifCommitFile).strip
    let cut = content.find({' ', '\t', '\n', '\r'})
    if cut < 0: content else: content[0 ..< cut]
  else: ""

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

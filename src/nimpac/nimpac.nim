## Nimony Package Control.
## (c) 2026 Andreas Rumpf

##[
Design
------

For every package either a specific commit is used or HEAD.
The resolution strategy is based on a package's depth:

YourApp (depth 0)
├── A (depth 1) → wants C@abc123
│   └── C (depth 2)
└── B (depth 1)
    └── D (depth 2)  → wants C@def456
        └── C (depth 3)

C@abc123 is used for A and D because that is the requirement
for A which is closer to YourApp.

A package's dependencies are extracted from its `.nimble` file.

If YourApp decides to list all its direct and indirect
dependencies explicitly and with specific commits then it becomes
a lockfile.

The same algorithm can be used for updates and initial
checkouts -- the classic breadth-first traversal.

A `requires` line in a `.nimble` file may use either a URL form or a
bare package name:

  requires "<git-url>[#<commit>] [<ignored version constraint>]"
  requires "<name>[#<commit>] [<ignored version constraint>]"

URL form is used as-is. Bare names are resolved via Nim's official
`packages.json` (downloaded once into the user cache directory),
following `alias` entries. If that misses, a GitHub `language:nim`
search picks the unique case-insensitive match. A `requires "nim ..."`
line refers to the compiler itself and is ignored.

Link files
----------

Inside the deps directory an entry can be either a directory (a real
checkout) or a plain text file. A file entry is a *link* whose first
non-empty line is a path -- absolute, or relative to the deps directory --
to where the package actually lives. This is Nimble's `.nimble-link`
feature, indispensable for hacking on several packages at once.

When a link is present nimpac will not clone or checkout: it parses
the linked package's `.nimble` to discover transitive dependencies,
then references the linked location from the generated `nim.cfg`.

nimony.paths generation
------------------

After resolution nimpac (re-)writes a managed block in `nim.cfg` next
to the input `.nimble`, with one `--path:` directive per resolved
package. The block is delimited by sentinel comments so user content
in `nim.cfg` is preserved across runs.
]##

import std / [os, osproc, parseopt, strutils, syncio, assertions, tables, deques, algorithm, json, sets, times, terminal, streams]

include ".." / lib / nifprelude
import ".." / lib / tooldirs

const
  Version = "0.1.0"
  Usage = "Nimpac - Nimony package control. Version " & Version & """

  (c) 2026 Andreas Rumpf
Usage:
  nimpac [options] [command] [args]

Commands:
  fetch [.nimble]       (default) clone or update the dependency closure
                        of the given .nimble file (or the .nimble file in
                        the current directory) and rewrite nim.cfg.
  search <terms>        search packages.json + GitHub for matching Nim
                        packages and print candidates.
  refresh               re-download packages.json into the cache.
  translate <pkg> <ver> walk <pkg>'s .nimble history and print the SHA
                        of the latest commit whose `version = "..."`
                        equals <ver>. Pipe it into a `requires` line:
                          requires "<url>#$(nimpac translate <pkg> <ver>)"

Options:
  --depsdir:DIR         where to put the cloned packages (default: deps)
  --cfg:FILE            nim.cfg path to patch (default: nimony.paths next to
                        the input .nimble; can also be a nim.cfg path)
  --nocfg               skip nimony.paths/nim.cfg generation
  --offline             do not touch the network (cached packages.json
                        and existing clones only)
  --deep                always clone with full history (default: shallow,
                        auto-promoted to deep when a commit hash is pinned)
  --parallel:N|auto         run up to N git clones/fetches in parallel
                        (default: 1; auto = #cores). BFS dispatch order
                        is preserved per depth.
  --nimony              `search` only: restrict to GitHub topics `nimony`
                        / `nim3` and packages.json entries tagged the same.
                        Does not affect explicit `requires` resolution.
  --version             show the version
  --help                show this help
"""

  PackagesRepoUrl = "https://github.com/nim-lang/packages.git"
  GithubSearchUrl = "https://api.github.com/search/repositories?q="
  PackagesTtlDays = 7

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

# ---------------------------------------------------------------------------
# Colored CLI output
# ---------------------------------------------------------------------------

type
  MsgKind = enum
    mInfo    ## informational ("cloning", "patched", "resolved")
    mNote    ## softer aside ("foo pinned to ...")
    mWarn    ## non-fatal problem
    mError   ## fatal — followed by quit

proc msgStream(k: MsgKind): File =
  ## All status messages go to stderr so commands like `translate` can
  ## use stdout exclusively for their data (the resolved SHA), making
  ## them safe to consume via shell `$(...)` capture.
  stderr

proc useColors(s: File): bool =
  isatty(s) and getEnv("NO_COLOR").len == 0

proc say(kind: MsgKind; parts: varargs[string, `$`]) =
  ## Single sink for all CLI status messages. Picks stream + ANSI color +
  ## prefix from `kind`. Auto-disables color when the stream isn't a TTY
  ## or `NO_COLOR` is set.
  const
    Prefix: array[MsgKind, string] = [
      mInfo:  "[nimpac] ",
      mNote:  "[nimpac] note: ",
      mWarn:  "[nimpac] warning: ",
      mError: "[nimpac] error: "]
    Color: array[MsgKind, ForegroundColor] = [
      mInfo:  fgCyan,
      mNote:  fgBlue,
      mWarn:  fgYellow,
      mError: fgRed]
  let s = msgStream(kind)
  if useColors(s):
    s.setForegroundColor(Color[kind], bright = true)
    s.write Prefix[kind]
    s.resetAttributes()
  else:
    s.write Prefix[kind]
  for p in parts: s.write p
  s.write '\n'

proc fatal(parts: varargs[string, `$`]) {.noreturn.} =
  say mError, parts
  quit 1

# ---------------------------------------------------------------------------
# Requirement parsing
# ---------------------------------------------------------------------------

type
  Requirement = object
    spec: string   ## raw text from the .nimble (for diagnostics)
    name: string   ## canonical key — derived from URL or supplied name
    url: string    ## git URL, "" if input was a bare name (resolved later)
    commit: string ## commit/branch/tag, "" for HEAD

  NimbleSpec = object
    srcDir: string
    requires: seq[Requirement]

proc isUrl(s: string): bool =
  s.contains("://") or s.startsWith("git@")

proc deriveName(url: string): string =
  ## Last path segment of `url`, with a trailing `.git` removed.
  var u = url
  if u.endsWith('/'): u.setLen u.len - 1
  let slash = u.rfind('/')
  if slash >= 0: u = u.substr(slash + 1)
  if u.endsWith(".git"): u.setLen u.len - len(".git")
  result = u

proc parseRequirement(spec: string): Requirement =
  ## Parse `<url-or-name>[#<commit>][ <ignored constraint>]`. Bare names
  ## are kept as-is in `name` with empty `url`; the resolver fills in
  ## `url` later.
  result = Requirement(spec: spec)
  var s = spec.strip()
  let sp = s.find({' ', '\t'})
  let head = if sp >= 0: s.substr(0, sp - 1) else: s
  let hash = head.find('#')
  let stem = if hash >= 0: head.substr(0, hash - 1) else: head
  if hash >= 0: result.commit = head.substr(hash + 1)
  if isUrl(stem):
    result.url = stem
    result.name = deriveName(stem)
  else:
    result.name = stem

# ---------------------------------------------------------------------------
# Run nifler to parse a .nimble file into a NIF TokenBuf
# ---------------------------------------------------------------------------

proc parseNimbleViaNifler(nimbleFile: string): TokenBuf =
  let nifler = findTool("nifler")
  if nifler.len == 0 or not fileExists(nifler):
    quit "cannot find nifler tool; build it with `hastur build nifler`"
  let outFile = getTempDir() / "nimpac_" &
                extractFilename(nimbleFile).changeFileExt("nif")
  let cmd = quoteShell(nifler) & " parse " &
            quoteShell(nimbleFile) & " " & quoteShell(outFile)
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    quit "nifler failed on " & nimbleFile & ": " & output

  var stream = nifstreams.open(outFile)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

# ---------------------------------------------------------------------------
# Cursor-based traversal of the parsed nimble file
# ---------------------------------------------------------------------------

proc readStrLit(n: var Cursor; dest: var string): bool =
  ## If `n` is at a string literal, read it into `dest` and advance.
  result = n.kind == StringLit
  if result:
    dest = pool.strings[n.litId]
    inc n

proc parseRequiresCmd(n: var Cursor; spec: var NimbleSpec) =
  ## Enters a `(cmd ...)` or `(call ...)` whose first child is `requires`.
  ## Each remaining string-literal child becomes one requirement (nimble
  ## allows `requires "a", "b"`).
  n.into:
    inc n  # past `requires` ident
    while n.hasMore:
      var s = ""
      if readStrLit(n, s):
        let r = parseRequirement(s)
        if r.name.len > 0 and r.name != "nim":
          spec.requires.add r
      else:
        skip n

proc parseAsgn(n: var Cursor; spec: var NimbleSpec) =
  ## `(asgn <ident> <value>)` — pick out scalars we care about.
  n.into:
    if n.kind == Ident:
      let lhs = pool.strings[n.litId]
      inc n
      if lhs == "srcDir":
        var s = ""
        discard readStrLit(n, s)
        if s.len > 0: spec.srcDir = s
      else:
        skip n  # value
    else:
      skip n  # lhs
      if n.hasMore: skip n  # rhs

proc parseNimble(nimbleFile: string): NimbleSpec =
  result = NimbleSpec()
  var buf = parseNimbleViaNifler(nimbleFile)
  var n = beginRead(buf)
  if n.kind != ParLe or pool.tags[n.tag] != "stmts":
    quit nimbleFile & ": expected (stmts ...) at top level"
  n.loopInto:
    if n.kind == ParLe:
      let tag = pool.tags[n.tag]
      case tag
      of "asgn":
        parseAsgn(n, result)
      of "cmd", "call":
        var probe = n
        inc probe
        if probe.kind == Ident and pool.strings[probe.litId] == "requires":
          parseRequiresCmd(n, result)
        else:
          skip n
      else:
        skip n
    else:
      skip n
  endRead n

# ---------------------------------------------------------------------------
# Git operations
# ---------------------------------------------------------------------------

proc run(cmd: string): bool =
  execShellCmd(cmd) == 0

proc gitFetch(pkgDir, url: string) =
  if not run("git -C " & quoteShell(pkgDir) & " fetch --tags origin"):
    quit "git fetch failed for " & url

proc looksLikeSha(s: string): bool =
  ## Heuristic: any 7+ char run of pure hex is treated as a commit hash.
  ## A short SHA is 7+; full is 40. Branch/tag names that happen to be all
  ## hex are vanishingly rare and only cost us a deep clone if we miss.
  if s.len < 7: return false
  for ch in s:
    if ch notin {'0'..'9', 'a'..'f', 'A'..'F'}: return false
  true

proc gitClone(url, dest, commit: string; shallow: bool) =
  ## When `shallow` is on we ask git for `--depth 1`, optionally pinned to
  ## a `--branch` (works for both branches and tags). Shallow + raw commit
  ## SHA is unsupported by most servers, so callers decide which mode to
  ## ask for via `looksLikeSha(commit)`.
  ##
  ## `--recurse-submodules` initialises any nested submodules in one shot;
  ## `--shallow-submodules` mirrors the depth=1 choice down into them so
  ## a shallow request stays shallow throughout. Both are no-ops on repos
  ## that don't carry a `.gitmodules`.
  createDir(dest.parentDir)
  var cmd = "git clone --recurse-submodules "
  if shallow:
    cmd.add "--depth 1 --shallow-submodules "
    if commit.len > 0:
      cmd.add "--branch " & quoteShell(commit) & " "
  cmd.add quoteShell(url) & " " & quoteShell(dest)
  if not run(cmd):
    quit "git clone failed for " & url

proc gitRefExists(pkgDir, refName: string): bool =
  run("git -C " & quoteShell(pkgDir) &
      " rev-parse --verify --quiet " & quoteShell(refName) & " >/dev/null 2>&1")

proc gitDefaultBranch(pkgDir: string): string =
  ## Name of the upstream's default branch, e.g. "main" or "master".
  ## `git clone` records this in `refs/remotes/origin/HEAD`. Falls back
  ## to probing the conventional names if that pointer is missing (can
  ## happen on hand-rolled repos). Returns "" if neither is found.
  result = ""
  let (output, code) = execCmdEx(
    "git -C " & quoteShell(pkgDir) &
    " symbolic-ref --short refs/remotes/origin/HEAD")
  if code == 0:
    result = output.strip()
    if result.startsWith("origin/"): result = result.substr(len("origin/"))
    return result
  for name in ["master", "main"]:
    if gitRefExists(pkgDir, "origin/" & name): return name

proc gitSync(pkgDir, commit: string) =
  ## Post-fetch alignment: bring the working tree to the requested ref's
  ## *current* upstream tip. Without this, a rerun on an existing clone
  ## with `commit == ""` would only advance `origin/...` but leave local
  ## HEAD on the previous tip — making "update" silently a no-op.
  ##
  ## - `commit == ""`     → reset to `origin/<default-branch>`.
  ## - `commit` is a SHA  → reset to that exact commit (no upstream alias).
  ## - otherwise          → prefer `origin/<commit>` (advances if it's a
  ##                        branch); fall back to `<commit>` (a tag).
  ##
  ## `reset --hard` is appropriate here: dependency directories should not
  ## carry local edits, and the BFS treats updates as equivalent to a
  ## fresh checkout.
  var target = ""
  if commit.len == 0:
    let b = gitDefaultBranch(pkgDir)
    if b.len == 0:
      say mWarn, "cannot determine default branch in ", pkgDir,
                 "; leaving as-is"
      return
    target = "origin/" & b
  elif looksLikeSha(commit):
    target = commit
  elif gitRefExists(pkgDir, "origin/" & commit):
    target = "origin/" & commit
  else:
    target = commit
  if not run("git -C " & quoteShell(pkgDir) & " reset --hard " &
             quoteShell(target)):
    quit "git reset --hard " & target & " failed in " & pkgDir
  # `reset --hard` updates the index's submodule pointers but not the
  # actual submodule worktrees; without this, re-syncs would silently
  # leave stale submodule contents around. Gated on `.gitmodules` so the
  # common (no-submodule) case stays one shell-out lighter.
  if fileExists(pkgDir / ".gitmodules"):
    if not run("git -C " & quoteShell(pkgDir) &
               " submodule update --init --recursive"):
      say mWarn, "submodule update failed in ", pkgDir

proc gitCurrentCommit(pkgDir: string): string =
  let (output, code) = execCmdEx("git -C " & quoteShell(pkgDir) &
                                 " rev-parse HEAD")
  if code == 0: output.strip() else: ""

# ---------------------------------------------------------------------------
# Name resolution: packages.json + GitHub search
# ---------------------------------------------------------------------------

type
  PackageEntry = object
    name: string
    url: string         ## empty when this entry is an alias
    alias: string       ## name of the canonical entry, or ""
    tags: seq[string]
    description: string

proc packagesRepoDir(): string =
  getCacheDir() / "nimpac" / "packages"

proc packagesCachePath(): string =
  packagesRepoDir() / "packages.json"

proc isStale(path: string; ttlDays: int): bool =
  if not fileExists(path): return true
  let now = epochTime()
  let mtime = toUnixFloat(getLastModificationTime(path))
  (now - mtime) > float(ttlDays * 86400)

proc updatePackagesRepo(force: bool) =
  ## Maintain a shallow clone of nim-lang/packages so we can read its
  ## `packages.json` without depending on stdlib HTTP/SSL. `force` triggers
  ## a `git pull`; otherwise an existing checkout is reused as-is.
  let repo = packagesRepoDir()
  if not dirExists(repo):
    createDir(repo.parentDir)
    say mInfo, "cloning ", PackagesRepoUrl, " -> ", repo
    if not run("git clone --depth 1 " & quoteShell(PackagesRepoUrl) & " " &
               quoteShell(repo)):
      quit "git clone of " & PackagesRepoUrl & " failed"
  elif force:
    say mInfo, "updating ", repo
    if not run("git -C " & quoteShell(repo) & " pull --ff-only origin"):
      say mWarn, "git pull failed; using cached copy"

proc curlGet(url: string): string =
  ## Shell out to `curl` for one-off HTTPS GETs (only the optional GitHub
  ## search uses this). Raises on failure.
  let (output, code) = execCmdEx("curl -sSL " & quoteShell(url))
  if code != 0:
    raise newException(CatchableError,
                       "curl exited " & $code & " for " & url)
  output

proc encodeUrl(s: string): string =
  ## Tiny RFC3986-style percent-encoder. Avoids pulling in std/uri.
  result = newStringOfCap(s.len+8)
  for ch in s:
    case ch
    of 'a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~':
      result.add ch
    else:
      result.add '%'
      result.add toHex(ord(ch), 2)

proc loadPackagesJson(path: string): seq[PackageEntry] =
  result = @[]
  let raw = json.parseFile(path)
  if raw.kind != JArray: return
  for j in raw.items:
    var p = PackageEntry()
    p.name = j.getOrDefault("name").getStr
    if p.name.len == 0: continue
    if j.hasKey("alias"):
      p.alias = j["alias"].getStr
    else:
      p.url = j.getOrDefault("url").getStr
      p.description = j.getOrDefault("description").getStr
      let tags = j.getOrDefault("tags")
      if tags.kind == JArray:
        for t in tags.items: p.tags.add t.getStr
    result.add p

proc lookupExact(pkgs: seq[PackageEntry]; name: string): int =
  ## Index of the (case-insensitive) exact-name match, or -1.
  let ln = name.toLowerAscii
  for i, p in pkgs:
    if p.name.toLowerAscii == ln: return i
  -1

proc resolveAlias(pkgs: seq[PackageEntry]; name: string): PackageEntry =
  var i = lookupExact(pkgs, name)
  var hops = 0
  while i >= 0 and pkgs[i].alias.len > 0 and hops < 16:
    i = lookupExact(pkgs, pkgs[i].alias)
    inc hops
  if i >= 0: pkgs[i] else: PackageEntry()

const
  NimonyTopics = ["nimony", "nim3"]

proc githubSearchOne(term: string; nimOnly: bool; topic: string): seq[PackageEntry] =
  result = @[]
  var url = GithubSearchUrl & encodeUrl(term)
  if nimOnly: url &= "+language:nim"
  if topic.len > 0: url &= "+topic:" & encodeUrl(topic)
  let body =
    try: curlGet(url)
    except CatchableError as e:
      say mWarn, "github search failed: ", e.msg
      return
  let parsed =
    try: parseJson(body)
    except JsonParsingError: return
  let items = parsed.getOrDefault("items")
  if items.kind != JArray: return
  for j in items.items:
    var p = PackageEntry()
    p.name = j.getOrDefault("name").getStr
    p.url = j.getOrDefault("html_url").getStr
    p.description = j.getOrDefault("description").getStr
    let topics = j.getOrDefault("topics")
    if topics.kind == JArray:
      for t in topics.items: p.tags.add t.getStr
    if p.url.len > 0:
      result.add p

proc githubSearch(term: string; topics: openArray[string] = []): seq[PackageEntry] =
  ## Run a GitHub search restricted to language:nim. With non-empty
  ## `topics`, run one query per topic (GitHub qualifiers are AND-only)
  ## and union the results — gives `topic:A OR topic:B` semantics.
  if topics.len == 0:
    result = githubSearchOne(term, nimOnly = true, topic = "")
  else:
    result = @[]
    var seenUrls = initHashSet[string]()
    for tp in topics:
      for p in githubSearchOne(term, nimOnly = true, topic = tp):
        if not seenUrls.containsOrIncl(p.url):
          result.add p

proc passesTagFilter(p: PackageEntry; topics: openArray[string]): bool =
  ## True when `topics` is empty (filter off) or `p` carries any of them.
  if topics.len == 0: return true
  for t in p.tags:
    let lt = t.toLowerAscii
    for want in topics:
      if lt == want: return true
  return false

# ---------------------------------------------------------------------------
# Link-file resolution
# ---------------------------------------------------------------------------

proc readLinkFile(linkPath: string): string =
  ## Read a link file: first non-empty line is interpreted as the target
  ## path (absolute or relative to the link file's directory). Returns "" if
  ## the file is unreadable, empty, or its target doesn't exist.
  if not fileExists(linkPath): return ""
  for raw in lines(linkPath):
    let line = raw.strip()
    if line.len == 0 or line.startsWith('#'): continue
    let target =
      if isAbsolute(line): line
      else: linkPath.parentDir / line
    let norm = target.normalizedPath()
    if dirExists(norm): return norm
    # bad link — surface it; a silent fallback to clone would be misleading
    quit "link file " & linkPath & " points at non-existent " & norm
  return ""

# ---------------------------------------------------------------------------
# BFS resolver
# ---------------------------------------------------------------------------

type
  Resolved = object
    url: string
    commit: string
    depth: int
    dir: string       ## actual filesystem location (clone dir or link target)
    isLink: bool      ## true when sourced from a link file
    srcDir: string    ## as declared in the package's .nimble

  PendingNode = object
    parent: string
    req: Requirement
    depth: int

  Context = object
    depsDir: string
    offline: bool
    deepClones: bool       ## force `--depth=∞` on every clone
    nimonyOnly: bool       ## restrict `search` to topic:nimony / topic:nim3
    parallelJobs: int      ## max concurrent git clones/fetches (>=1)
    packages: seq[PackageEntry]
    packagesLoaded: bool
    resolved: Table[string, Resolved]
    inFlight: Table[string, tuple[depth: int, commit: string]]
      ## packages whose clone/fetch Process is in flight in some slot.
      ## Consulted by `dedupHit` so a second request for the same package
      ## doesn't get dispatched into a parallel slot before the first
      ## completes.

proc findNimbleFile(pkgDir: string): string =
  for f in walkDir(pkgDir):
    if f.kind == pcFile and f.path.endsWith(".nimble"):
      return f.path
  return ""

proc ensurePackages(c: var Context; force = false) =
  ## Make sure packages.json is cached and parsed. With `force`, refresh
  ## the local clone via `git pull`. Honors `c.offline`: never touches the
  ## network, but still loads an existing cached copy.
  let cache = packagesCachePath()
  if not c.offline:
    if force or not fileExists(cache) or isStale(cache, PackagesTtlDays):
      updatePackagesRepo(force or fileExists(cache))
  if fileExists(cache) and not c.packagesLoaded:
    c.packages = loadPackagesJson(cache)
    c.packagesLoaded = true

proc resolveNameToUrl(c: var Context; name: string): string =
  ## Map a bare package name to a git URL. Tries packages.json (with
  ## alias expansion), then GitHub search for an exact case-insensitive
  ## name match. Returns "" on failure.
  result = ""
  ensurePackages(c)
  let p = resolveAlias(c.packages, name)
  if p.url.len > 0: return p.url
  if c.offline: return ""
  var matches = 0
  for cand in githubSearch(name):
    if cmpIgnoreCase(deriveName(cand.url), name) == 0:
      result = cand.url
      inc matches
  if matches != 1: result = ""

proc resolveUrl(c: var Context; r: var Requirement): bool =
  ## Make sure `r.url` is populated; resolve bare names through packages
  ## .json + GitHub. Updates `r.name` to the canonical form derived from
  ## the resolved URL, so two requirements pointing at the same repo
  ## share a `deps/` slot. Returns false when resolution fails.
  if r.url.len > 0: return true
  let url = resolveNameToUrl(c, r.name)
  if url.len == 0: return false
  r.url = url
  let canonical = deriveName(url)
  if canonical.len > 0: r.name = canonical
  return true

# ---------------------------------------------------------------------------
# Slot-based BFS
#
# Mirrors hastur.nim's `parallelTestDir` pattern: a fixed pool of slots,
# each holding one in-flight git Process. Up to `c.parallelJobs` clones
# /fetches run at once. The slow part of materializing a package — the
# actual `git clone` or `git fetch` against the network — runs in
# parallel; the small follow-ups (`gitSync`, parsing the `.nimble`,
# queueing children) are done in the main thread on completion.
#
# Depth-monotonic BFS is preserved at *dispatch* time (not completion):
# a child is enqueued only by its parent's `finalize`, and the deque is
# FIFO, so a depth-N+1 node never gets dispatched before all depth-N
# parents have been popped. Equal-depth ties between an in-flight
# package and a later request are still resolved as "first dispatched
# wins"; `c.inFlight` makes that lookup work before `c.resolved` is set.
# ---------------------------------------------------------------------------

type
  JobKind = enum jkClone, jkFetch
  Slot = object
    p: Process
    kind: JobKind
    req: Requirement
    parent: string
    depth: int
    pkgDir: string

proc startGitClone(req: Requirement; pkgDir: string; shallow: bool): Process =
  createDir(pkgDir.parentDir)
  var args = @["clone", "--quiet", "--recurse-submodules"]
  if shallow:
    args.add "--depth"
    args.add "1"
    args.add "--shallow-submodules"
    if req.commit.len > 0:
      args.add "--branch"
      args.add req.commit
  args.add req.url
  args.add pkgDir
  startProcess("git", args = args, options = {poUsePath, poStdErrToStdOut})

proc startGitFetch(pkgDir: string): Process =
  startProcess("git",
    args = ["-C", pkgDir, "fetch", "--quiet", "--tags", "origin"],
    options = {poUsePath, poStdErrToStdOut})

proc noteCommitConflict(p: PendingNode; prevDepth: int; prevCommit: string) =
  if prevCommit.len > 0 and p.req.commit.len > 0 and
      prevCommit != p.req.commit:
    say mNote, p.req.name, " pinned to ", prevCommit,
               " (from depth ", prevDepth, ") — ignoring ",
               p.parent, "'s request for ", p.req.commit

proc dedupHit(c: Context; p: PendingNode): bool =
  ## Reproduces the serial path's "shallower-or-equal already claimed"
  ## rule, but also consults `c.inFlight` so two slots can't race on the
  ## same package's deps directory.
  result = false
  if p.req.name in c.resolved:
    let prev = c.resolved[p.req.name]
    if prev.depth <= p.depth:
      if not prev.isLink:
        noteCommitConflict(p, prev.depth, prev.commit)
      result = true
  elif p.req.name in c.inFlight:
    let prev = c.inFlight[p.req.name]
    if prev.depth <= p.depth:
      noteCommitConflict(p, prev.depth, prev.commit)
      result = true

proc finalize(c: var Context; queue: var Deque[PendingNode];
              p: PendingNode; info: var Resolved) =
  ## Common tail for slot-completed and inline (link-file) paths: parse
  ## the package's `.nimble`, commit to `c.resolved`, queue transitive
  ## requires.
  let nimbleFile = findNimbleFile(info.dir)
  if nimbleFile.len == 0:
    say mWarn, "no .nimble file in ", info.dir
    c.resolved[p.req.name] = info
    return
  let spec = parseNimble(nimbleFile)
  info.srcDir = spec.srcDir
  c.resolved[p.req.name] = info
  for child in spec.requires:
    queue.addLast PendingNode(parent: p.req.name, req: child,
                              depth: p.depth + 1)

proc tryDispatch(c: var Context; queue: var Deque[PendingNode];
                 slots: var seq[Slot]; active: var int; slot: int) =
  ## Drain the queue into `slots[slot]`. Skips deduped requests and
  ## handles link files inline (no Process needed). Returns once the
  ## slot is filled with an in-flight Process, or the queue is empty.
  while queue.len > 0:
    var p = queue.popFirst()
    if not resolveUrl(c, p.req):
      say mWarn, "cannot resolve ", p.req.spec,
                 " (requested by ", p.parent, ") — skipping"
      continue
    if dedupHit(c, p): continue

    let entry = c.depsDir / p.req.name
    if fileExists(entry):
      let target = readLinkFile(entry)
      say mInfo, "using link ", entry, " -> ", target
      var info = Resolved(url: p.req.url,
                          commit: gitCurrentCommit(target),
                          depth: p.depth, dir: target, isLink: true)
      finalize(c, queue, p, info)
      continue

    let shallow = not c.deepClones and not looksLikeSha(p.req.commit)
    let fresh = not dirExists(entry)
    let process =
      if fresh:
        say mInfo, (if shallow: "cloning (shallow) " else: "cloning "),
                   p.req.url, " -> ", entry
        startGitClone(p.req, entry, shallow)
      else:
        say mInfo, "updating ", entry
        startGitFetch(entry)

    c.inFlight[p.req.name] = (depth: p.depth, commit: p.req.commit)
    slots[slot] = Slot(p: process,
                       kind: if fresh: jkClone else: jkFetch,
                       req: p.req, parent: p.parent, depth: p.depth,
                       pkgDir: entry)
    inc active
    return

proc reap(c: var Context; queue: var Deque[PendingNode];
          slots: var seq[Slot]; active: var int; slot: int): bool =
  ## Non-blocking. If the slot's Process has exited, drain its output,
  ## run the cheap serial follow-ups (`gitSync`, finalize), free the
  ## slot, and return true. Returns false if still running or empty.
  if slots[slot].p == nil: return false
  let exit = peekExitCode(slots[slot].p)
  if exit == -1: return false
  var outp = ""
  var line = ""
  while slots[slot].p.outputStream.readLine(line):
    outp.add line
    outp.add '\n'
  slots[slot].p.close()
  if exit != 0:
    quit "git " &
         (if slots[slot].kind == jkClone: "clone" else: "fetch") &
         " failed for " & slots[slot].req.url & "\n" & outp
  if outp.len > 0:
    stdout.write outp
  gitSync(slots[slot].pkgDir, slots[slot].req.commit)
  let head = gitCurrentCommit(slots[slot].pkgDir)
  var info = Resolved(
    url: slots[slot].req.url,
    commit: if slots[slot].req.commit.len > 0:
              slots[slot].req.commit else: head,
    depth: slots[slot].depth,
    dir: slots[slot].pkgDir,
    isLink: false)
  let p = PendingNode(parent: slots[slot].parent, req: slots[slot].req,
                      depth: slots[slot].depth)
  c.inFlight.del p.req.name
  finalize(c, queue, p, info)
  slots[slot].p = nil
  dec active
  return true

proc fetchAll(c: var Context; rootSpec: NimbleSpec) =
  var queue = initDeque[PendingNode]()
  for r in rootSpec.requires:
    queue.addLast PendingNode(parent: "<root>", req: r, depth: 1)

  let parallel = max(1, c.parallelJobs)
  var slots = newSeq[Slot](parallel)
  var active = 0
  for s in 0 ..< parallel: tryDispatch(c, queue, slots, active, s)
  while active > 0:
    var anyReaped = false
    for s in 0 ..< parallel:
      if reap(c, queue, slots, active, s): anyReaped = true
    # Reap or initial tryDispatch may have queued new work (transitive
    # requires from a finalized package, or from inline-handled links).
    # Refill any empty slots before sleeping.
    for s in 0 ..< parallel:
      if slots[s].p == nil: tryDispatch(c, queue, slots, active, s)
    if active > 0 and not anyReaped:
      sleep(2)

# ---------------------------------------------------------------------------
# nim.cfg generation
# ---------------------------------------------------------------------------

type
  ConfigKind = enum
    NimCfg, NimonyPaths

const
  CfgBegin = "# >>> nimpac begin (managed block — do not edit) <<<"
  CfgEnd = "# <<< nimpac end >>>"

proc cfgPathSpec(c: Context; r: Resolved; cfgDir: string): string =
  ## Path to use in a `--path:` directive — relative to `cfgDir` when
  ## possible (so the project is movable), else absolute.
  let target = if r.srcDir.len > 0: r.dir / r.srcDir else: r.dir
  result = relativePath(target.normalizedPath(),
                        cfgDir.absolutePath, '/')
  if result.startsWith("..") or isAbsolute(result):
    # outside cfgDir (typical for link-file targets) — keep absolute form
    result = target.normalizedPath()

proc generateBlock(c: Context; cfgPath: string; kind: ConfigKind): string =
  let cfgDir = cfgPath.parentDir
  result = CfgBegin & "\n"
  # Sort by name for deterministic output across runs.
  var names: seq[string] = @[]
  for k in c.resolved.keys: names.add k
  names.sort(cmp[string])
  for name in names:
    let r = c.resolved[name]
    let p = cfgPathSpec(c, r, cfgDir)
    case kind
    of NimCfg:
      result.add "--path:\"" & p & "\"\n"
    of NimonyPaths:
      result.add p
      result.add "\n"
  result.add CfgEnd & "\n"

proc patchNimCfg(cfgPath, blockText: string) =
  var existing = ""
  if fileExists(cfgPath):
    existing = readFile(cfgPath)
  let beg = existing.find(CfgBegin)
  if beg >= 0:
    let endi = existing.find(CfgEnd, beg)
    if endi < 0:
      quit cfgPath & ": found '" & CfgBegin &
           "' but no matching end marker — aborting to avoid clobbering"
    var endLine = existing.find('\n', endi)
    if endLine < 0: endLine = existing.len - 1
    let before = existing.substr(0, beg - 1)
    let after = existing.substr(endLine + 1)
    existing = before & blockText & after
  else:
    if existing.len > 0 and not existing.endsWith('\n'):
      existing.add '\n'
    existing.add blockText
  writeFile(cfgPath, existing)
  say mInfo, "patched ", cfgPath

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

proc findRootNimble(): string =
  for f in walkDir(getCurrentDir()):
    if f.kind == pcFile and f.path.endsWith(".nimble"):
      return f.path
  return ""

proc printPkg(p: PackageEntry; origin: string) =
  stdout.writeLine p.name, "  ", p.url, "  (", origin, ")"
  if p.description.len > 0:
    stdout.writeLine "    ", p.description

proc extractNimbleVersion(content: string): string =
  ## Pull `version = "X.Y.Z"` out of a .nimble file content. Tolerant of
  ## whitespace, single/double quotes, and trailing comments.
  for raw in content.splitLines:
    var line = raw
    let hash = line.find('#')
    if hash >= 0: line = line.substr(0, hash - 1)
    line = line.strip()
    if not line.startsWith("version"): continue
    var rest = line.substr(len("version")).strip()
    if rest.startsWith('='): rest = rest.substr(1).strip()
    if rest.len < 2: continue
    let q = rest[0]
    if q != '"' and q != '\'': continue
    let endq = rest.find(q, 1)
    if endq > 0: return rest.substr(1, endq - 1)
  return ""

proc nimbleVersionAt(repo, commit, relPath: string): string =
  ## Read the .nimble at `commit:relPath` and extract its version.
  ## Returns "" if the file isn't reachable at that commit (e.g. before a
  ## rename) — caller should treat that as "no match" and move on.
  let (output, code) = execCmdEx(
    "git -C " & quoteShell(repo) & " show " & quoteShell(commit & ":" & relPath))
  if code != 0: return ""
  extractNimbleVersion(output)

proc lookupRepoDir(name: string): string =
  getCacheDir() / "nimpac" / "lookups" / name

proc translateCmd(c: var Context; pkg, version: string) =
  ## Resolve `<pkg>` to a URL, fully clone it into the lookup cache, then
  ## walk `git log --follow` over its .nimble file (newest first) for the
  ## last commit whose `version =` line equals `<version>`. Prints the SHA
  ## on stdout so it can be piped into a `requires "...#<sha>"` line.
  var req = Requirement(name: pkg, spec: pkg)
  if isUrl(pkg):
    req.url = pkg
    req.name = deriveName(pkg)
  if not resolveUrl(c, req):
    fatal "cannot resolve ", pkg

  let repo = lookupRepoDir(req.name)
  if not dirExists(repo):
    say mInfo, "cloning ", req.url, " -> ", repo
    gitClone(req.url, repo, "", shallow = false)
  else:
    say mInfo, "updating ", repo
    gitFetch(repo, req.url)
    if fileExists(repo / ".git" / "shallow"):
      # We previously cached this as shallow (e.g. via `fetch`); deepen
      # it so `git log` can walk the version history.
      if not run("git -C " & quoteShell(repo) & " fetch --unshallow"):
        say mWarn, "could not unshallow ", repo, "; history may be incomplete"

  let nimbleFile = findNimbleFile(repo)
  if nimbleFile.len == 0:
    fatal "no .nimble file in ", repo
  let rel = relativePath(nimbleFile, repo, '/')

  let (logOut, code) = execCmdEx(
    "git -C " & quoteShell(repo) &
    " log --follow --format=%H -- " & quoteShell(rel))
  if code != 0:
    fatal "git log failed in ", repo

  for h in logOut.splitLines:
    let commit = h.strip()
    if commit.len == 0: continue
    if nimbleVersionAt(repo, commit, rel) == version:
      stdout.writeLine commit
      return
  fatal req.name, " has no commit where version == ", version

proc searchCmd(c: var Context; terms: seq[string]) =
  ensurePackages(c)
  let topics: seq[string] = if c.nimonyOnly: @NimonyTopics else: @[]
  var seenUrls = initHashSet[string]()
  for term in terms:
    let lt = term.toLowerAscii
    for p in c.packages:
      if p.alias.len > 0 or p.url.len == 0: continue
      if not passesTagFilter(p, topics): continue
      var matched = lt in p.name.toLowerAscii
      if not matched:
        for tag in p.tags:
          if lt in tag.toLowerAscii:
            matched = true
            break
      if matched and not seenUrls.containsOrIncl(p.url):
        printPkg(p, "packages.json")
    if not c.offline:
      for p in githubSearch(term, topics):
        if not seenUrls.containsOrIncl(p.url):
          printPkg(p, "github")
  if seenUrls.len == 0:
    stdout.writeLine "[nimpac] no matches for ", terms.join(" ")

proc handleCmdLine() =
  var action = ""
  var args: seq[string] = @[]
  var depsDir = "deps"
  var cfgFile = ""
  var noCfg = false
  var offline = false
  var deepClones = false
  var nimonyOnly = false
  var parallelJobs = 1
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if action.len == 0 and key in ["fetch", "search", "refresh", "translate"]:
        action = key
      else:
        args.add key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "depsdir": depsDir = val
      of "cfg": cfgFile = val
      of "nocfg": noCfg = true
      of "offline": offline = true
      of "deep": deepClones = true
      of "nimony": nimonyOnly = true
      of "parallel", "j":
        if val == "auto" or val.len == 0:
          parallelJobs = countProcessors()
        else:
          try: parallelJobs = max(1, parseInt(val))
          except CatchableError: writeHelp()
      else: quit(Usage)
    of cmdEnd: assert false, "cannot happen"

  if action.len == 0: action = "fetch"

  var ctx = Context(depsDir: absolutePath(depsDir),
                    offline: offline,
                    deepClones: deepClones,
                    nimonyOnly: nimonyOnly,
                    parallelJobs: parallelJobs,
                    resolved: initTable[string, Resolved](),
                    inFlight: initTable[string, tuple[depth: int, commit: string]]())

  case action
  of "fetch":
    let nimbleFile =
      if args.len > 0 and args[0].endsWith(".nimble"): args[0]
      else: findRootNimble()
    if nimbleFile.len == 0 or not fileExists(nimbleFile):
      quit "no .nimble file found"

    createDir(ctx.depsDir)
    let rootSpec = parseNimble(nimbleFile)
    fetchAll(ctx, rootSpec)
    echo "[nimpac] resolved ", ctx.resolved.len, " package(s) into ", ctx.depsDir

    if not noCfg:
      let cfgPath =
        if cfgFile.len > 0: cfgFile
        else: nimbleFile.parentDir / "nimony.paths"
      let kind = if cfgPath.endsWith(".cfg"): NimCfg else: NimonyPaths
      patchNimCfg(cfgPath, generateBlock(ctx, cfgPath, kind))
  of "search":
    if args.len == 0: quit "search: at least one term required"
    searchCmd(ctx, args)
  of "refresh":
    if ctx.offline: quit "refresh requires network access"
    updatePackagesRepo(force = true)
  of "translate":
    if args.len != 2:
      quit "translate: usage: nimpac translate <name|url> <version>"
    translateCmd(ctx, args[0], args[1])
  else:
    quit "Invalid action: " & action

when isMainModule:
  handleCmdLine()

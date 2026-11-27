## The git conveniences: `hastur sync`, `pull`, `push`, `publish_release`.

import std / [osproc, strutils, sequtils, times]
import context

proc syncCmd*(newBranch: string) =
  let (output, status) = execCmdEx("git symbolic-ref --short HEAD")
  if status != 0:
    quit "FAILURE: " & output
  let (defaultBranchOutput, defaultBranchStatus) = execCmdEx("git symbolic-ref refs/remotes/origin/HEAD --short")
  var defaultBranch = "master"
  if defaultBranchStatus == 0:
    # Output is like "origin/main" or "origin/master"
    defaultBranch = defaultBranchOutput.strip()
    if defaultBranch.startsWith("origin/"):
      defaultBranch = defaultBranch[7..^1]
  exec "git checkout " & defaultBranch
  exec "git pull origin " & defaultBranch
  let branch = output.strip()
  if branch != defaultBranch:
    exec "git branch -D " & branch
  if newBranch.len > 0:
    exec "git checkout -B " & newBranch

proc pullpush*(cmd: string) =
  let (output, status) = execCmdEx("git symbolic-ref --short HEAD")
  if status != 0:
    quit "FAILURE: " & output
  exec "git " & cmd & " origin " & output.strip()

const VersionFile = "doc/version.md"

proc gitOut(cmd: string): string =
  let (output, status) = execCmdEx(cmd)
  if status != 0:
    quit "FAILURE " & cmd & "\n" & output
  result = output.strip()

proc parseVersion(s: string): (int, int, int) =
  ## `major.minor.patch`, all three plain decimal numbers.
  let parts = s.split('.')
  if parts.len != 3:
    quit "FAILURE: version `" & s & "` is not of the form major.minor.patch"
  var nums = default(array[3, int])
  for i, p in parts:
    if p.len == 0 or not allCharsInSet(p, Digits) or (p.len > 1 and p[0] == '0'):
      quit "FAILURE: version `" & s & "`: `" & p & "` is not a number"
    nums[i] = parseInt(p)
  result = (nums[0], nums[1], nums[2])

proc publishReleaseCmd*(expected: string) =
  ## Tags the current `master` commit with the version `doc/version.md` holds
  ## and pushes the tag. The version bump itself is a normal commit made
  ## beforehand; this only checks it against the versioning rules:
  ##
  ## - `major.minor.patch`;
  ## - `patch` is even (odd is an in-development version);
  ## - `major + minor + 2020` is the current year;
  ## - it is newer than every `vX.Y.Z` tag that exists.
  let branch = gitOut("git symbolic-ref --short HEAD")
  if branch != "master":
    quit "FAILURE: releases are tagged on `master`, not on `" & branch & "`"
  if execCmdEx("git diff --quiet HEAD -- " & VersionFile).exitCode != 0:
    quit "FAILURE: " & VersionFile & " has uncommitted changes; commit the version bump first"
  exec "git fetch origin master --tags"
  if gitOut("git rev-parse HEAD") != gitOut("git rev-parse origin/master"):
    quit "FAILURE: local `master` is not `origin/master`; push or pull first"

  let content = gitOut("git show HEAD:" & VersionFile)
  let fields = content.splitWhitespace()
  if fields.len == 0:
    quit "FAILURE: " & VersionFile & " is empty"
  let version = fields[0]
  if expected.len > 0 and expected.strip(chars = {'v'}, trailing = false) != version:
    quit "FAILURE: " & VersionFile & " says `" & version & "`, not `" & expected & "`"
  let (major, minor, patch) = parseVersion(version)
  if patch mod 2 != 0:
    quit "FAILURE: version " & version & " has an odd patch number, which marks " &
      "an in-development version; a release has an even one"
  let year = now().year
  if major + minor + 2020 != year:
    quit "FAILURE: version " & version & ": major + minor + 2020 = " &
      $(major + minor + 2020) & ", but it is " & $year

  let tag = "v" & version
  for t in gitOut("git tag --list v*").splitLines():
    let t = t.strip()
    if t.len < 2: continue
    if t == tag:
      quit "FAILURE: tag " & tag & " exists already"
    let parts = t[1..^1].split('.')
    if parts.len == 3 and parts.allIt(it.len > 0 and allCharsInSet(it, Digits)):
      let old = (parseInt(parts[0]), parseInt(parts[1]), parseInt(parts[2]))
      if old >= (major, minor, patch):
        quit "FAILURE: version " & version & " is not newer than the existing tag " & t

  exec "git tag -a " & tag & " -m " & quoteShell("Version " & version)
  exec "git push origin " & tag
  echo "tagged and pushed ", tag, " at ", gitOut("git rev-parse --short HEAD")

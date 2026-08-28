## The git conveniences: `hastur sync`, `pull`, `push`.

import std / [osproc, strutils]
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

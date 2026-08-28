## A test directory's category: what it means, how it maps onto a `nimony`
## subcommand, and how it is resolved from the nearest `hastur.mode` file.

import std / [syncio, os, strutils]

type
  Category* = enum
    Normal, # normal category
    Basics, # basic tests: These are processed with --noSystem
    Tracked # tracked tests: These are processed and can contain "track info"
            # for line, col, filename extraction (useful for nimsuggest-like tests)
    Compat # compatibility mode tests
    Valgrind # valgrind tests
    Optimized # tests compiled with --opt:speed (exercise the shoggoth passes)
    Skip # `hastur.mode = skip`: the tree walk ignores this directory and its
         # subtree (non-suite dirs: fixtures, inputs owned by another runner)

proc toCommand*(cat: Category): string =
  case cat
  of Basics: "m"
  of Tracked: "check --silentMake"
  of Optimized: "c --silentMake --opt:speed"
  of Normal, Compat, Valgrind, Skip: "c --silentMake"

const ModeFile* = "hastur.mode"
  ## A test directory may drop a `hastur.mode` file naming the category that
  ## applies to it and everything beneath it. This replaces the old scheme of
  ## inferring the category from magic directory names (`nosystem`, `track`,
  ## `compat`, `valgrind`, `opt`): a suite is now free to use whatever
  ## directory layout it likes and opts into a special mode explicitly.

proc parseMode*(s, src: string): Category =
  ## Map a `hastur.mode` keyword to a `Category`. The legacy directory names
  ## are the canonical keywords; the enum names are accepted as synonyms.
  case s.strip.normalize
  of "normal", "": Normal
  of "nosystem", "basics": Basics
  of "track", "tracked": Tracked
  of "compat": Compat
  of "valgrind": Valgrind
  of "opt", "optimized": Optimized
  of "skip": Skip
  else: quit "invalid mode '" & s.strip & "' in " & src

proc categoryOfDir*(dir: string): Category =
  ## Resolve the category for `dir` from the nearest `hastur.mode` file in
  ## `dir` or an ancestor. No mode file up the whole chain means `Normal`.
  var d = dir
  while d.len > 0:
    let mf = d / ModeFile
    if fileExists(mf):
      return parseMode(readFile(mf), mf)
    let parent = d.parentDir
    if parent == d: break
    d = parent
  return Normal

proc categoryOf*(path: string): Category =
  ## Category for a test file or directory: the mode of its own directory
  ## (or, for a not-yet-existing `record` destination, its parent directory).
  categoryOfDir(if dirExists(path): path else: path.parentDir)

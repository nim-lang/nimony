## The coverage assertions: checks that a source tree and the test that is
## supposed to cover it have not drifted apart. They are not tests of the
## compiler, so they run once per sweep rather than per directory, and they
## fail the run outright — a silently uncovered module is exactly the thing
## nobody notices.

import std / [syncio, os, algorithm, strutils]

const
  StdlibDir* = "lib/std"
  StdlibAllTest* = "tests/nimony/stdlib/tall.nim"
    ## `tall.nim` imports every stdlib module and does nothing else. Two jobs
    ## depend on that being complete: it is the one test that says the modules
    ## still compile *together*, and `dagon` walks it as the aggregator driver
    ## for the website's documentation, so a module missing here is a module
    ## missing from the docs.

const NonModuleDirs = ["system", "includes", "private", "deps", "errorcodes",
                       "posix", "windows", "ioring"]
  ## Subdirectories of `lib/std` that hold no importable module, with the
  ## reason each is here: `system/`, `includes/` and `errorcodes/` are `include`
  ## fragments (`system.nim` pulls them in), `private/` is private by name,
  ## `deps/` is plugin support code, `posix/`/`windows/` are per-platform
  ## bindings no single build can import at once, and `ioring/` is the internals
  ## `std/ioring` is the front door to.
  ##
  ## A DENY list on purpose. Everything else under `lib/std` — `http/` today —
  ## is a public module the docs must carry, and a new public subdirectory
  ## should fail this check rather than be silently skipped. A new INTERNALS
  ## directory is the case that needs a line here, and its reason with it.

proc collectModules(dir, prefix: string; result: var seq[string]) =
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      result.add prefix & x.path.splitFile.name
    elif x.kind == pcDir and x.path.splitPath.tail notin NonModuleDirs:
      collectModules(x.path, prefix & x.path.splitPath.tail & "/", result)

proc stdlibModules(dir: string): seq[string] =
  ## Every documented stdlib module, spelled the way it is imported:
  ## `hashes`, `http/httpmsg`. See `NonModuleDirs` for what is left out.
  result = @[]
  collectModules(dir, "", result)
  sort result

proc importedModules(testFile: string): seq[string] =
  ## The `std/x` modules `tall.nim` imports. It is a flat list of one-import
  ## lines by construction, so this stays a line scan rather than a parse — but
  ## `import std / a, b` is legal Nimony and would otherwise read as a module
  ## nobody imported, so spaces are dropped and comma lists are split.
  result = @[]
  for raw in lines(testFile):
    var line = raw.strip
    let hash = line.find('#')
    if hash >= 0: line = line.substr(0, hash-1).strip
    if not line.startsWith("import "): continue
    for part in line.substr("import ".len).split(','):
      let m = part.replace(" ", "")
      if m.startsWith("std/"): result.add m.substr("std/".len)
  sort result

proc checkStdlibCoverage*() =
  ## Fail the run when `tall.nim` and `lib/std` have drifted apart. Missing
  ## modules are the point; duplicates and imports of modules that no longer
  ## exist are reported in the same pass because both mean the file was edited
  ## by hand and nobody re-read it.
  if not dirExists(StdlibDir) or not fileExists(StdlibAllTest):
    # Not the repo root (`--bindir` runs from elsewhere): nothing to assert.
    return
  let have = stdlibModules(StdlibDir)
  let imported = importedModules(StdlibAllTest)
  var missing: seq[string] = @[]
  for m in have:
    if binarySearch(imported, m) < 0: missing.add m
  var unknown: seq[string] = @[]
  var dups: seq[string] = @[]
  for i in 0 ..< imported.len:
    if binarySearch(have, imported[i]) < 0: unknown.add imported[i]
    if i > 0 and imported[i] == imported[i-1]: dups.add imported[i]
  if missing.len == 0 and unknown.len == 0 and dups.len == 0: return
  var msg = "FAILURE: " & StdlibAllTest & " is out of sync with " & StdlibDir & "/"
  if missing.len > 0:
    msg.add "\n  not imported (so not compiled together and not documented):\n    import std/" &
            missing.join("\n    import std/")
  if unknown.len > 0:
    msg.add "\n  imported but no such module:\n    " & unknown.join("\n    ")
  if dups.len > 0:
    msg.add "\n  imported twice:\n    " & dups.join("\n    ")
  quit msg

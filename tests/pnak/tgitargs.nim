## `pnak fetch` must not hand git an option disguised as a URL or a commit:
## those come from other people's `.nimble` files, and git runs a command
## for `--upload-pack=<cmd>`. Such a requirement is refused before git runs.
##
## Run manually:
##   hastur build pnak
##   nim c -r tests/pnak/tgitargs.nim

import std / [os, osproc, strutils, syncio, assertions]

const
  PnakBin = "bin" / "pnak"

proc main() =
  let pnak = absolutePath(PnakBin.addFileExt(ExeExt))
  doAssert fileExists(pnak),
    "pnak binary not found at " & pnak &
    "; build it first with `hastur build pnak`"

  let testRoot = absolutePath("tests" / "pnak" / "_workdir_gitargs")
  removeDir testRoot
  createDir testRoot
  defer: removeDir testRoot

  for (spec, what) in [
      ("--upload-pack=touch" & "://x", "URL"),
      ("file:///nonexistent/fakepkg#--upload-pack=touch", "commit")]:
    let project = testRoot / "project"
    removeDir project
    createDir project
    writeFile project / "myapp.nimble",
      "version = \"0.1.0\"\nrequires \"" & spec & "\"\n"
    let (output, code) = execCmdEx(
      quoteShell(pnak) & " --offline --depsdir:deps fetch myapp.nimble",
      workingDir = project)
    doAssert code != 0, "pnak accepted " & spec & "\n" & output
    doAssert ("refusing " & what) in output, output

  echo "OK: pnak git argument test passed"

when isMainModule:
  main()

#       Niflink — the Nimony link driver
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this distribution.

## `niflink` is the C-backend link driver. It reads a *link manifest* — a small
## NIF file that `deps.nim` produces — describing every artifact of a project
## (object files, C sources, routed backend artifacts), the application type and
## the global link flags, then compiles any C sources and links everything into
## the final executable or library. It exists so the backend "tail" of the build
## graph (the per-`cc` nodes plus the branchy link step) can collapse into one
## tool that owns the link.
##
## Built on the nifcore NIF API (`nifcoreparse` + `nifcore`): the manifest is
## parsed into a `TokenBuf` and walked with a plain cursor.
##
## Manifest shape (see `deps.writeLinkManifest`):
##
##   (link
##     (apptype "console")               ; console | gui | lib | staticlib
##     (output "path/to/app")
##     (file "a.o"   (kind "obj"))       ; linked directly
##     (file "b.c"   (kind "csrc"))      ; compiled, then linked
##     (file "g.o"   (kind "obj") (flags "-lvulkan"))  ; per-file link flags
##     (file "k.spv" (kind "artifact"))  ; ignored by the C linker
##     (flags "-lm"))                    ; global link flags
##
## Per-file `(flags …)` (a `.build` module's 4th slot) are placed on the linker
## command line right after their file; the manifest-level `(flags …)` (`passL`)
## apply globally.
##
## Invocation: `niflink <manifest.nif> [output]` (an explicit `output` argument
## overrides the manifest's `(output …)`).

import std / [os, syncio, strutils]
import ".." / "lib" / [nifcore, nifcoreparse]

type
  LinkFile = object
    path: string
    flags: seq[string]    ## per-file link flags, placed next to `path`

  Manifest = object
    apptype: string
    output: string
    objs: seq[LinkFile]   ## `kind "obj"` — linked directly
    csrcs: seq[LinkFile]  ## `kind "csrc"` — compiled to an object, then linked
    flags: seq[string]    ## global link flags

proc readStrChild(c: var Cursor; pool: Pool): string =
  ## The first string-literal child of the current tag, consuming the subtree.
  result = ""
  c.into:
    while c.hasMore:
      if c.kind == StrLit:
        if result.len == 0: result = strVal(c, pool)
        inc c
      else:
        skip c

proc readFlags(c: var Cursor; pool: Pool): seq[string] =
  ## All string-literal children of a `(flags …)` tag, consuming the subtree.
  result = @[]
  c.into:
    while c.hasMore:
      if c.kind == StrLit:
        result.add strVal(c, pool)
        inc c
      else:
        skip c

proc readFileEntry(c: var Cursor; tags: TagPool; pool: Pool): tuple[file: LinkFile, kind: string] =
  ## `(file "path" (kind "...") [ (flags …) ])`.
  var file = LinkFile(path: "", flags: @[])
  var kind = ""
  c.into:
    while c.hasMore:
      if c.kind == StrLit:
        if file.path.len == 0: file.path = strVal(c, pool)
        inc c
      elif c.kind == TagLit and tagName(tags, c.cursorTagId) == "kind":
        kind = readStrChild(c, pool)
      elif c.kind == TagLit and tagName(tags, c.cursorTagId) == "flags":
        file.flags = readFlags(c, pool)
      else:
        skip c
  result = (file, kind)

proc parseManifest(path: string): Manifest =
  result = Manifest(apptype: "console", output: "",
                    objs: @[], csrcs: @[], flags: @[])
  var b = parseFromFile(path)
  var c = b.beginRead()
  if c.kind != TagLit or tagName(b.tags, c.cursorTagId) != "link":
    quit "niflink: not a link manifest: " & path
  c.into:
    while c.hasMore:
      if c.kind == TagLit:
        case tagName(b.tags, c.cursorTagId)
        of "apptype":
          result.apptype = readStrChild(c, b.pool)
        of "output":
          result.output = readStrChild(c, b.pool)
        of "file":
          let f = readFileEntry(c, b.tags, b.pool)
          case f.kind
          of "obj": result.objs.add f.file
          of "csrc": result.csrcs.add f.file
          else: discard   # `artifact` / unknown — not for the C linker
        of "flags":
          result.flags.add readFlags(c, b.pool)
        else:
          skip c
      else:
        inc c

proc runTool(prog: string; args: seq[string]; spillBase: string): int =
  ## Run `prog args…`. The object list can outgrow the OS command-line limit
  ## (Windows' `cmd.exe` caps it near 8 KB), so a long command is spilled to a
  ## file. The thresholds and the per-OS strategy mirror Nim's
  ## `preventLinkCmdMaxCmdLen` in `compiler/extccomp.nim`, which has the scars:
  ##   * Windows / Linux: a `@response-file` the compiler driver expands. GCC's
  ##     response-file parser treats `\` as an escape, so Windows paths must use
  ##     forward slashes (clang/gcc accept them) — we flip `\`→`/`, not escape.
  ##   * macOS: Apple's linker has no `@file`, so we hand the whole command to a
  ##     shell script instead.
  const MaxInlineCmdLen = when defined(windows): 8_000
                          elif defined(macosx): 260_000
                          else: 32_000
  var inline = prog
  for a in args: inline.add " " & quoteShell(a)
  if inline.len <= MaxInlineCmdLen:
    return execShellCmd(inline)

  when defined(macosx):
    let script = spillBase & "_link.sh"
    try:
      writeFile(script, inline)
    except:
      quit "niflink: cannot write link script: " & script
    var shell = getEnv("SHELL")
    if shell.len == 0: shell = "/bin/sh"
    result = execShellCmd(shell & " " & quoteShell(script))
    try: removeFile(script)
    except: discard
  else:
    let rsp = spillBase & "_linkerArgs.txt"
    var content = ""
    for a in args: content.add quoteShell(a) & "\n"
    # The driver we invoke is always gcc/clang here, whose response-file parser
    # mangles backslashes — use forward slashes (valid for paths on Windows).
    content = content.replace('\\', '/')
    try:
      writeFile(rsp, content)
    except:
      quit "niflink: cannot write response file: " & rsp
    result = execShellCmd(prog & " @" & quoteShell(rsp))
    try: removeFile(rsp)
    except: discard

proc main =
  if paramCount() < 1:
    quit "usage: niflink <manifest.nif> [output]"
  let m = parseManifest(paramStr(1))
  var output = m.output
  if paramCount() >= 2 and paramStr(2).len > 0:
    output = paramStr(2)
  if output.len == 0:
    quit "niflink: no output path (in the manifest or on the command line)"

  let cc = getEnv("CC", "cc")
  var objs = m.objs

  # Compile any C sources to an object next to the source, then link them too
  # (carrying any per-file link flags forward onto the produced object).
  for src in m.csrcs:
    let o = src.path & ".o"
    let rc = execShellCmd(cc & " -c " & quoteShell(src.path) & " -o " & quoteShell(o))
    if rc != 0: quit rc
    objs.add LinkFile(path: o, flags: src.flags)

  if m.apptype == "staticlib":
    # `ar` does not take link flags; only the objects are archived.
    var args = @["rcs", output]
    for o in objs: args.add o.path
    quit runTool(getEnv("AR", "ar"), args, output)
  else:
    var args: seq[string] = @[]
    # Each object is followed by its own per-file link flags, so library
    # dependencies declared by a `.build` module resolve in the right order.
    for o in objs:
      args.add o.path
      for f in o.flags: args.add f
    if m.apptype == "lib": args.add "-shared"
    # On Windows the toolchain links with clang, whose native PE TLS layout is
    # mishandled by ld.bfd; LLD lays out `.tls$` the way the loader expects, so
    # native TLS survives to runtime. Mirror the old link step's `-fuse-ld=lld`.
    when defined(windows):
      args.add "-fuse-ld=lld"
    for f in m.flags: args.add f
    args.add "-o"
    args.add output
    quit runTool(cc, args, output)

main()

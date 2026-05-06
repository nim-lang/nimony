## Bootstrap installer for `hastur install`. Cross-platform.
##
## On **Windows** it downloads a winlibs MinGW+LLVM bundle (gcc 14.2.0 +
## LLVM 19.1.7 — gcc for the default `--cc:gcc` path, clang + ld.lld for
## the `--cc:clang` native-PE-TLS path) plus Nim's Windows DLL deps,
## both into `external/`. Inspired by Nim's `tools/finish.nim` (curl +
## 7z) but without registry mutation.
##
## On **all OSes** it writes virtual-env activation scripts at the
## project root that prepend the toolchain paths to `$PATH` for the
## current shell. Pattern lifted from atlas's `nimenv.nim`: each script
## defines a `deactivate` that restores the original `PATH` and prompt,
## so activation is reversible and never leaks into the user's
## persistent environment. The scripts know:
##   - `<projroot>/bin/`            — Nimony's built tools
##   - `<projroot>/external/...`    — vendored MinGW+LLVM (Windows only)

import std / [os, osproc, strutils, syncio]

const
  WinlibsUrl64* =
    "https://github.com/brechtsanders/winlibs_mingw/releases/download/" &
    "14.2.0posix-19.1.7-12.0.0-msvcrt-r3/" &
    "winlibs-x86_64-posix-seh-gcc-14.2.0-llvm-19.1.7-mingw-w64msvcrt-12.0.0-r3.7z"
  WinlibsUrl32* =
    "https://github.com/brechtsanders/winlibs_mingw/releases/download/" &
    "14.2.0posix-19.1.7-12.0.0-msvcrt-r3/" &
    "winlibs-i686-posix-dwarf-gcc-14.2.0-llvm-19.1.7-mingw-w64msvcrt-12.0.0-r3.7z"
  WindepsUrl* = "https://nim-lang.org/download/windeps.zip"

# --- activation scripts ----------------------------------------------------
#
# `$1` is the `;` (Windows) / `:` (POSIX) joined PATH-prefix string the
# script will prepend to the inherited `PATH`. We keep the original
# `PATH` and prompt aside under `_OLD_NIMONY_*` so `deactivate` can
# restore them. Same shape as atlas's nimenv activation scripts.

# Note: these templates are fed through `strutils.%`, which expands `$1`
# from a positional arg and reads `$$` as a literal `$`. Every shell-level
# `$NAME` / PowerShell `$env:` therefore needs to be written as `$$NAME` /
# `$$env:` so the format step doesn't try to substitute it. Same trick
# atlas's nimenv.nim uses.

const
  ShellActivate = """
# Source from your shell:  source activate.sh
if [ -z "$${_OLD_NIMONY_PATH+x}" ]; then
    export _OLD_NIMONY_PATH="$$PATH"
    export _OLD_NIMONY_PS1="$${PS1:-}"
fi

export PATH="$1:$$PATH"
export PS1="(nimony) $${PS1:-}"

deactivate() {
    if [ -n "$${_OLD_NIMONY_PATH+x}" ]; then
        export PATH="$$_OLD_NIMONY_PATH"
        export PS1="$$_OLD_NIMONY_PS1"
        unset _OLD_NIMONY_PATH
        unset _OLD_NIMONY_PS1
        unset -f deactivate
    else
        echo "Not in an activated nimony environment"
    fi
}
"""

  BatchActivate = """
@echo off
REM Run from cmd:  activate.bat
if not defined _OLD_NIMONY_PATH set "_OLD_NIMONY_PATH=%PATH%"
if not defined _OLD_NIMONY_PROMPT set "_OLD_NIMONY_PROMPT=%PROMPT%"
set "PATH=$1;%PATH%"
set "PROMPT=(nimony) %PROMPT%"
doskey deactivate=if defined _OLD_NIMONY_PATH (set "PATH=%%_OLD_NIMONY_PATH%%" ^& set "PROMPT=%%_OLD_NIMONY_PROMPT%%" ^& set "_OLD_NIMONY_PATH=" ^& set "_OLD_NIMONY_PROMPT=") else (echo Not in an activated nimony environment)
"""

  PowerShellActivate = """
# Source from PowerShell:  . .\activate.ps1
if (-not $$env:_OLD_NIMONY_PATH) {
    $$env:_OLD_NIMONY_PATH = $$env:PATH
    $$global:_OLD_NIMONY_PROMPT = (Get-Item Function:\prompt).ScriptBlock
}

$$env:PATH = "$1;$$env:PATH"

function global:prompt {
    "(nimony) " + (& $$global:_OLD_NIMONY_PROMPT)
}

function global:deactivate {
    if ($$env:_OLD_NIMONY_PATH) {
        $$env:PATH = $$env:_OLD_NIMONY_PATH
        Remove-Item Env:\_OLD_NIMONY_PATH
        Set-Item Function:\prompt $$global:_OLD_NIMONY_PROMPT
        Remove-Variable -Name _OLD_NIMONY_PROMPT -Scope Global
        Remove-Item Function:\deactivate
    } else {
        Write-Host "Not in an activated nimony environment"
    }
}
"""

# --- helpers ---------------------------------------------------------------

proc download(url, dest: string) =
  let curl = findExe("curl")
  if curl.len == 0:
    quit "hastur install: `curl` not found in PATH" &
         (when defined(windows): " (install via winget: `winget install curl.curl`)"
          else: " (install via your package manager)")
  let cmd = curl.quoteShell & " -L --fail -o " & dest.quoteShell & " " &
            url.quoteShell
  if execShellCmd(cmd) != 0:
    quit "hastur install: download failed: " & url

proc extract7z(archive, outDir: string) =
  let z7 = findExe("7z")
  if z7.len == 0:
    quit "hastur install: `7z` not found in PATH" &
         (when defined(windows): " (install via winget: `winget install 7zip.7zip`)"
          else: " (install via your package manager: e.g. `apt install p7zip-full`)")
  let cmd = z7.quoteShell & " x -y " & archive.quoteShell &
            " -o" & outDir.quoteShell
  if execShellCmd(cmd) != 0:
    quit "hastur install: 7z extraction failed for " & archive

proc writeWindowsScript(path, body: string) =
  ## cmd.exe is fussy about line endings — `@echo off` followed by an
  ## LF-only `REM` line is enough to break parsing on some Windows
  ## builds (the second line gets eaten and `call activate.bat` then
  ## reports "command not found" when invoked from another shell).
  ## Force CRLF so it parses regardless of how the source was checked
  ## out. PowerShell is fine either way but we normalise both for
  ## consistency.
  writeFile(path, body.replace("\n", "\r\n"))

proc writeActivateScripts(pathEntries: openArray[string]) =
  ## Write platform-appropriate activation scripts at the project root.
  ## On Windows we write both (cmd / PowerShell users coexist); on
  ## POSIX just the shell script. The first arg-substitution is the
  ## already-joined PATH-prefix string.
  when defined(windows):
    let joined = pathEntries.join(";")
    writeWindowsScript("activate.bat", BatchActivate % joined)
    writeWindowsScript("activate.ps1", PowerShellActivate % joined)
  else:
    let joined = pathEntries.join(":")
    writeFile("activate.sh", ShellActivate % joined)
    # Make it convenient to `chmod +x` even though sourcing doesn't
    # require execute bit. Best-effort.
    try: inclFilePermissions("activate.sh", {fpUserExec, fpGroupExec, fpOthersExec})
    except: discard

proc compilersOnPathHint() =
  ## On non-Windows we don't bundle a toolchain; instead nudge the user
  ## toward the system one. No hard failure — the activation script is
  ## still useful even before they install gcc/clang.
  let gcc = findExe("gcc")
  let clang = findExe("clang")
  echo ""
  echo "System compilers detected:"
  echo "  gcc:   ", (if gcc.len > 0: gcc else: "not found")
  echo "  clang: ", (if clang.len > 0: clang else: "not found")
  if gcc.len == 0 and clang.len == 0:
    echo "Install at least one before running `hastur build all`. Examples:"
    when defined(macosx):
      echo "  xcode-select --install   # or: brew install gcc"
    else:
      echo "  apt install gcc clang    # debian/ubuntu"
      echo "  dnf install gcc clang    # fedora"
      echo "  pacman -S gcc clang      # arch"

# --- main entry point ------------------------------------------------------

proc runInstall*(args: seq[string]) =
  for a in args:
    # Hastur's CLI strips leading `-`/`--` before passing args here, so
    # accept the dash-less forms too. Currently no recognized flags;
    # left as a switch for future options without changing the
    # signature.
    case a.normalize
    else: quit "hastur install: unknown argument: " & a

  let projectRoot = getCurrentDir()
  var pathEntries: seq[string] = @[]
  pathEntries.add absolutePath("bin")        # Nimony's built tools

  when defined(windows):
    let cpu = (when hostCPU == "i386": "i386" else: "amd64")
    let mingwUrl = (if cpu == "i386": WinlibsUrl32 else: WinlibsUrl64)
    let mingwDir = "external" / ("mingw-" & cpu)
    let dllsDir = "external" / ("dlls-" & cpu)

    createDir "external"

    if dirExists(mingwDir):
      echo mingwDir, " already exists; skipping MinGW+LLVM download."
    else:
      let archive = "external" / ("mingw-" & cpu & ".7z")
      echo "Downloading winlibs MinGW+LLVM (~500MB) -> ", archive
      download(mingwUrl, archive)
      echo "Extracting -> ", mingwDir
      extract7z(archive, "external")
      # winlibs unpacks to `mingw64/` or `mingw32/` at the top level.
      let extracted = "external" / (if cpu == "i386": "mingw32" else: "mingw64")
      moveDir(extracted, mingwDir)
      try: removeFile(archive) except: discard
      echo "MinGW+LLVM installed at ", mingwDir

    if dirExists(dllsDir):
      echo dllsDir, " already exists; skipping windeps download."
    else:
      let archive = "external" / "windeps.zip"
      echo "Downloading Nim Windows DLL deps -> ", archive
      download(WindepsUrl, archive)
      echo "Extracting -> ", dllsDir
      extract7z(archive, dllsDir)
      try: removeFile(archive) except: discard
      echo "DLL deps installed at ", dllsDir

    pathEntries.add absolutePath(mingwDir / "bin")
    pathEntries.add absolutePath(dllsDir)

  else:
    compilersOnPathHint()

  writeActivateScripts(pathEntries)
  echo ""
  echo "Wrote activation script(s) at ", projectRoot
  when defined(windows):
    echo "Activate with one of:"
    echo "  cmd:        activate.bat"
    echo "  PowerShell: . .\\activate.ps1"
  else:
    echo "Activate with:"
    echo "  source activate.sh"
  echo ""
  echo "PATH entries that will be prepended:"
  for e in pathEntries: echo "  ", e
  echo ""
  echo "Once activated, `deactivate` restores the original PATH/prompt."

import strutils
import posix/posix

proc c_system(cmd: cstring): cint {.
    importc: "system", header: "<stdlib.h>".}

proc quoteShellWindows*(s: string): string {.noSideEffect.} =
  ## Quote `s`, so it can be safely passed to Windows API.
  ##
  ## Based on Python's `subprocess.list2cmdline`.
  ## See `this link <https://msdn.microsoft.com/en-us/library/17w5ykft.aspx>`_
  ## for more details.
  let needQuote = s.contains({' ', '\t'}) or s.len == 0
  result = ""
  var backslashBuff = ""
  if needQuote:
    result.add("\"")

  for c in s:
    if c == '\\':
      backslashBuff.add(c)
    elif c == '\"':
      for i in 0..<backslashBuff.len*2:
        result.add('\\')
      backslashBuff.setLen(0)
      result.add("\\\"")
    else:
      if backslashBuff.len != 0:
        result.add(backslashBuff)
        backslashBuff.setLen(0)
      result.add(c)

  if backslashBuff.len > 0:
    result.add(backslashBuff)
  if needQuote:
    result.add(backslashBuff)
    result.add("\"")


proc quoteShellPosix*(s: string): string {.noSideEffect.} =
  ## Quote ``s``, so it can be safely passed to POSIX shell.
  const safeUnixChars = {'%', '+', '-', '.', '/', '_', ':', '=', '@',
                         '0'..'9', 'A'..'Z', 'a'..'z'}
  if s.len == 0:
    result = "''"
  elif s.allCharsInSet(safeUnixChars):
    result = s
  else:
    result = "'" & s.replace("'", "'\"'\"'") & "'"

when defined(windows) or defined(posix) or defined(nintendoswitch):
  proc quoteShell*(s: string): string {.noSideEffect.} =
    ## Quote ``s``, so it can be safely passed to shell.
    ##
    ## When on Windows, it calls `quoteShellWindows proc`_.
    ## Otherwise, calls `quoteShellPosix proc`_.
    when defined(windows):
      result = quoteShellWindows(s)
    else:
      result = quoteShellPosix(s)

  proc quoteShellCommand*(args: openArray[string]): string =
    ## Concatenates and quotes shell arguments `args`.
    runnableExamples:
      when defined(posix):
        assert quoteShellCommand(["aaa", "", "c d"]) == "aaa '' 'c d'"
      when defined(windows):
        assert quoteShellCommand(["aaa", "", "c d"]) == "aaa \"\" \"c d\""

    # can't use `map` pending https://github.com/nim-lang/Nim/issues/8303
    result = ""
    for i in 0..<args.len:
      if i > 0: result.add " "
      result.add quoteShell(args[i])

proc exitStatusLikeShell*(status: cint): cint =
  ## Converts exit code from `c_system` into a shell exit code.
  when defined(posix):
    if WIFSIGNALED(status):
      # like the shell!
      128 + WTERMSIG(status)
    else:
      WEXITSTATUS(status)
  else:
    status

proc execShellCmd*(command: string): int {.tags: [ExecIOEffect].} =
  ## Executes a `shell command`:idx:.
  ##
  ## Command has the form 'program args' where args are the command
  ## line arguments given to program. The proc returns the error code
  ## of the shell when it has finished (zero if there is no error).
  ## The proc does not return until the process has finished.
  ##
  ## To execute a program without having a shell involved, use `osproc.execProcess proc
  ## <osproc.html#execProcess,string,string,openArray[string],StringTableRef,set[ProcessOption]>`_.
  ##
  ## **Examples:**
  ##   ```Nim
  ##   discard execShellCmd("ls -la")
  ##   ```
  var command = command
  result = exitStatusLikeShell(c_system(command.toCString))

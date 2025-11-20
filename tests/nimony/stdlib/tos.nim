import std/[os, assertions]

when defined(posix):
  assert quoteShellPosix("ls") == "ls"
  assert execShellCmd("ls") == 0

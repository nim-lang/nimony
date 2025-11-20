import std/[os, assertions]

when defined(posix):
  assert execShellCmd("ls") == 0

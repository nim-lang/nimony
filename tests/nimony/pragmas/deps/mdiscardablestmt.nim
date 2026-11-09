import std/syncio

proc imported*(tag: string): string {.discardable.} =
  echo "imported ", tag
  result = tag

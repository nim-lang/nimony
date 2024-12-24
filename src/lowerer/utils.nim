include nifprelude


proc tagToken*(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

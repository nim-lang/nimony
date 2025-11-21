{.build("C", "${path}/../../../vendor/mimalloc/src/static.c", "-DMI_STATS=1 -I${path}/../../../vendor/mimalloc/include").}
when defined(arm):
  {.passL:"-latomic".}

type
  MiStatCount {.importc: "mi_stat_count_t", bycopy.} = object
    total: int64    # total allocated over time
    peak: int64     # peak allocation
    current: int64  # current allocation

  # We only need the first part (prefix) of mi_stats_t up to malloc_requested.
  # mi_stats_get copies exactly 'stats_size' bytes, so we define a prefix that
  # matches the initial layout of mi_stats_t in C.
  MiStatsPrefix {.importc: "mi_stats_t", bycopy.} = object
    version: cint
    pages: MiStatCount             # count of mimalloc pages
    reserved: MiStatCount          # reserved memory bytes
    committed: MiStatCount         # committed bytes
    reset: MiStatCount             # reset bytes
    purged: MiStatCount            # purged bytes
    page_committed: MiStatCount    # committed memory inside pages
    pages_abandoned: MiStatCount   # abandonded pages count
    threads: MiStatCount           # number of threads
    malloc_normal: MiStatCount     # allocated bytes <= MI_LARGE_OBJ_SIZE_MAX
    malloc_huge: MiStatCount       # allocated bytes in huge pages
    malloc_requested: MiStatCount  # malloc requested bytes
    # Note: the real C struct continues with counters and more fields, we falsely declare it as .completeStruct
    # for sizeof(MiStatsPrefix) to work.

proc mi_malloc(size: csize_t): pointer {.importc: "mi_malloc", cdecl.}
proc mi_calloc(nmemb: csize_t, size: csize_t): pointer {.importc: "mi_calloc", cdecl.}
proc mi_realloc(pt: pointer, size: csize_t): pointer {.importc: "mi_realloc", cdecl.}
proc mi_free(p: pointer) {.importc: "mi_free", cdecl.}

proc mi_usable_size(p: pointer): csize_t {.importc: "mi_usable_size", cdecl.}

proc mi_stats_merge() {.importc: "mi_stats_merge", cdecl.}
proc mi_stats_get(stats_size: csize_t; stats: ptr MiStatsPrefix) {.importc: "mi_stats_get", cdecl.}

# Optional: OS-level process info (independent of MI_STAT), not used here but handy:
# proc mi_process_info(elapsed_msecs, user_msecs, system_msecs,
#                      current_rss, peak_rss, current_commit, peak_commit,
#                      page_faults: ptr csize_t) {.importc: "mi_process_info", cdecl.}

proc alloc*(size: int): pointer =
  result = mi_malloc(size.csize_t)

proc realloc*(p: pointer; size: int): pointer =
  result = mi_realloc(p, size.csize_t)

proc dealloc*(p: pointer) =
  mi_free(p)

proc allocatedSize*(p: pointer): int =
  result = int mi_usable_size(p)

proc readStats(): MiStatsPrefix =
  result = default(MiStatsPrefix)
  # Fold thread-local stats into main stats so we get process-wide values.
  mi_stats_merge()
  mi_stats_get(csize_t(sizeof(result)), addr result)

proc getOccupiedMem*(): int =
  ## Returns the number of bytes that are owned by the process and hold data.
  let st = readStats()
  let allocated = st.malloc_normal.current + st.malloc_huge.current
  # Cast to Nim int (pointer-sized); beware of overflow on 32-bit if very large.
  result = int allocated

proc getTotalMem*(): int =
  ## Returns the number of bytes that are owned by the process.
  let st = readStats()
  result = int st.committed.current

proc getFreeMem*(): int =
  ## Returns the number of bytes that are owned by the process, but do not hold any meaningful data.
  let st = readStats()
  let allocated = st.malloc_normal.current + st.malloc_huge.current
  var freeBytes = st.committed.current - allocated
  if freeBytes < 0: freeBytes = 0          # be defensive against underflow if stats lag slightly
  result = int freeBytes


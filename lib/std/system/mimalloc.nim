{.build("C", "${path}/../../../vendor/mimalloc/src/static.c", "-I${path}/../../../vendor/mimalloc/include").}

proc mi_malloc(size: csize_t): pointer {.importc: "mi_malloc".}
proc mi_calloc(nmemb: csize_t, size: csize_t): pointer {.importc: "mi_calloc".}
proc mi_realloc(pt: pointer, size: csize_t): pointer {.importc: "mi_realloc".}
proc mi_free(p: pointer) {.importc: "mi_free".}

proc mi_usable_size(p: pointer): csize_t {.importc: "mi_usable_size".}

proc alloc*(size: int): pointer =
  result = mi_malloc(size.csize_t)

proc realloc*(p: pointer; size: int): pointer =
  result = mi_realloc(p, size.csize_t)

proc dealloc*(p: pointer) =
  mi_free(p)

proc allocatedSize*(p: pointer): int =
  result = int mi_usable_size(p)

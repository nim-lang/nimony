{.compile("vendor/mimalloc/src/static.c", "-Ivendor/mimalloc/include").}

type
  csize_t* {.importc: "size_t", nodecl.} = uint

proc mi_malloc(size: csize_t): pointer {.importc: "mi_malloc".}
proc mi_calloc(nmemb: csize_t, size: csize_t): pointer {.importc: "mi_calloc".}
proc mi_realloc(pt: pointer, size: csize_t): pointer {.importc: "mi_realloc".}
proc mi_free(p: pointer) {.importc: "mi_free".}


proc alloc*(size: int): pointer =
  result = mi_malloc(size.csize_t)

proc realloc*(p: pointer; size: int): pointer =
  result = mi_realloc(p, size.csize_t)

proc dealloc*(p: pointer) =
  mi_free(p)

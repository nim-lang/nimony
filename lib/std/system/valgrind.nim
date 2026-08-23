#
#            Nim's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Valgrind client requests, for the libc-free native allocator.
##
## Valgrind learns about a heap by intercepting `malloc`. The native allocator
## has none to intercept — it takes pages from `mmap` and hands out cells inside
## them — so to valgrind every string, seq and ref in a `nimony n` program lives
## in anonymous memory that was never allocated and can never leak. The reports
## are not wrong, they are empty: `total heap usage: 0 allocs, 0 frees`.
##
## A client request is how a program tells valgrind what it just did. The
## mechanism is a fixed sequence of instructions that valgrind's JIT recognizes
## and replaces, and that does nothing whatsoever when no one is watching (see
## `VgClientRequest` in `src/lib/intrinsics.nim` for why that is exact rather
## than approximate). So the calls below are free in an ordinary run and there is
## no separate code path to keep honest.
##
## Enabled by `-d:valgrind`. Off, every template here expands to nothing.

const vgTracking* = defined(valgrind) and defined(nimNoLibc) and defined(arm64)
  ## Where the requests can actually be made: the native backend on AArch64,
  ## which is where `VgClientRequest` has a lowering.
  ##
  ## The C backend deliberately has none, and wants none: there the heap is
  ## mimalloc's, valgrind already understands it through mimalloc's own
  ## `MI_TRACK_VALGRIND` integration, and duplicating that here would mean two
  ## sets of requests describing one heap.

# `-d:valgrind` on a target without a `VgClientRequest` lowering (native x86-64,
# today) compiles to nothing rather than failing. That is a wart and not a
# decision: a standalone `{.error.}`/`{.warning.}` pragma is rejected inside the
# `system` include chain, so there is nowhere here to say it. Until then the
# observable symptom is honest at least — `vgRunningOnValgrind()` answers false
# and the heap summary stays empty, which is what an uninstrumented build looks
# like, because that is what it is.

when vgTracking:
  proc vgClientRequest(args: pointer): uint {.intrinsic: "VgClientRequest".}

  const
    # `valgrind/valgrind.h`'s core requests, and `memcheck.h`'s, by value. They
    # are a published ABI ("DO NOT CHANGE THE ORDER OF THESE ENTRIES, NOR DELETE
    # ANY", says the header) — which is what makes spelling them as numbers here
    # legitimate rather than a shortcut, and is why a nimony program needs no
    # valgrind headers and no valgrind at build time at all.
    ReqRunningOnValgrind = 0x1001'u
    ReqMallocLikeBlock   = 0x1301'u
    ReqFreeLikeBlock     = 0x1302'u
    ReqResizeInPlace     = 0x130B'u
    # memcheck's are based at `VG_USERREQ_TOOL_BASE('M','C')` = 'M'<<24|'C'<<16.
    ReqMakeMemNoAccess   = 0x4D430000'u
    ReqMakeMemUndefined  = 0x4D430001'u
    ReqMakeMemDefined    = 0x4D430002'u

  template vgDo(r, a1, a2, a3, a4) {.untyped.} =
    ## One request, as a STATEMENT — the form every announcement below uses.
    ##
    ## A template and not a proc, for the same reason valgrind's own interface is
    ## a macro: valgrind records the stack at the instant of the request, so a
    ## helper proc here would put ITSELF at the top of every allocation and every
    ## free stack in every report. That is a frame of pure noise in front of the
    ## answer the reader wants, and it costs one of the `--num-callers` frames
    ## that would otherwise show more of the caller.
    ##
    ## The six-word block lives on the stack because valgrind reads it out of the
    ## client's own memory as the request is made.
    block:
      var blk: array[6, uint] = [uint(r), uint(a1), uint(a2), uint(a3),
                                 uint(a4), 0'u]
      discard vgClientRequest(cast[pointer](addr blk[0]))

  proc vgRequestValue(r, a1, a2, a3, a4: uint): uint {.inline.} =
    ## The expression form, for the one request whose ANSWER is the point.
    ## `vgRunningOnValgrind` is asked once, outside any report, so the extra frame
    ## that made `vgDo` a template does not matter here.
    var blk: array[6, uint] = [r, a1, a2, a3, a4, 0'u]
    result = vgClientRequest(cast[pointer](addr blk[0]))

proc vgRunningOnValgrind*(): bool {.inline.} =
  ## Whether a valgrind tool is watching. False in every ordinary run — including
  ## one built with `-d:valgrind` but not launched under valgrind, since the
  ## request sequence then executes as the no-op it is.
  when vgTracking:
    result = vgRequestValue(ReqRunningOnValgrind, 0, 0, 0, 0) != 0'u
  else:
    result = false

# ── what the allocator says about a block ──────────────────────────────────

template vgMallocLike(p, size, rzB, zeroed) {.untyped.} =
  ## "I just handed out `size` bytes at `p`." From here valgrind counts the block,
  ## remembers this stack as its origin, and will report it if it is never given
  ## back.
  ##
  ## `rzB` is how many guard bytes surround the block. It is 0 here and that is a
  ## real limitation rather than a placeholder: cells are packed adjacently, so
  ## there is nothing to guard with, and an overflow into the next cell is
  ## invisible. Catching those needs the allocator to pad every cell in this
  ## build, which is a separate change with a real memory cost.
  when vgTracking:
    vgDo(ReqMallocLikeBlock, cast[uint](p), size, rzB, zeroed)

template vgFreeLike(p, rzB) {.untyped.} =
  ## "The block at `p` is mine again." Valgrind marks it inaccessible and keeps it
  ## on a quarantine list, which is what turns a later read or write through a
  ## stale pointer into a report naming both this stack and the allocating one.
  when vgTracking:
    vgDo(ReqFreeLikeBlock, cast[uint](p), rzB, 0, 0)

template vgResizeInPlace(p, oldSize, newSize, rzB) {.untyped.} =
  ## A block grown or shrunk without moving. Unused while `realloc` is
  ## alloc-copy-dealloc, and here so that a future in-place path has the request
  ## it would need rather than silently losing the block's identity.
  when vgTracking:
    vgDo(ReqResizeInPlace, cast[uint](p), oldSize, newSize, rzB)

# ── what the allocator says about its own memory ───────────────────────────
#
# The three primitives below exist for one problem: this allocator keeps its
# free-list links INSIDE the cells it has handed back, so the moment a block is
# declared free, the allocator's own bookkeeping writes to it become
# use-after-free — reported against the allocator, on every single dealloc.
#
# The fix is the one mimalloc uses for the identical situation (see
# `vendor/mimalloc/include/mimalloc/track.h` and `mi_block_set_nextx`): open a
# window exactly as wide as the metadata, touch it, close it again. See
# `withFreeCell` in `alloc.nim` for this allocator's version.

template vgMakeMemNoAccess(p, size) {.untyped.} =
  ## Neither readable nor writable: touching it is an error.
  when vgTracking:
    vgDo(ReqMakeMemNoAccess, cast[uint](p), size, 0, 0)

template vgMakeMemUndefined(p, size) {.untyped.} =
  ## Addressable, but holding nothing meaningful yet — reading it before writing
  ## it is an error. What a block becomes when it is handed out.
  when vgTracking:
    vgDo(ReqMakeMemUndefined, cast[uint](p), size, 0, 0)

template vgMakeMemDefined(p, size) {.untyped.} =
  ## Addressable and holding real data: reading it is fine.
  when vgTracking:
    vgDo(ReqMakeMemDefined, cast[uint](p), size, 0, 0)

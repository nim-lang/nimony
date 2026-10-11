#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The tree the parser builds, as links rather than as packed tokens.
##
## `nifcore.TokenBuf` is the right shape for a tree that is *read*: a flat
## array of `NifToken`, a subtree spanned by a jump field, `skip` an O(1)
## pointer bump. It is the wrong shape for a tree that is still being
## *rewritten*, which is what a parser for Nim's grammar does on almost every
## node -- `a + b` has to put the operator in front of an operand that is
## already written, a declaration with three names fans out into three nodes,
## a trailing `do` block moves inside the call in front of it. Against a flat
## array each of those is a memmove of the region since the mark, and reading
## the region back means a `Cursor`, which mints a `CursorOwner` header for
## the buffer and frees it again on the next append.
##
## Here a node is a cell: `down` is its first child, `next` its next sibling.
## Wrapping a run of siblings in a new tag, inserting an operator in front of
## one, moving the last child to the front -- all are pointer assignments that
## touch a constant number of cells, whatever the size of the subtree.
##
## What does NOT change is the atoms. A `payload` is a `nifcore.NifToken`
## built by nifcore's own constructors, so a tag is still a `TagId` and a name
## is still a `StrId` interned in the same `Pool`: only the wiring is local to
## nifler2. The one thing that does not fit in a token's 28-bit payload is a
## numeric literal, which nifcore spreads over up to three chained tokens; a
## cell keeps the value in `wide` instead and `flatten` hands it back to
## nifcore's chaining writer untouched.
##
## Cells come from a bump arena. Allocation is a pointer increment, and the
## whole tree is released by freeing the blocks -- there is nothing per-cell
## to run, which is what lets the rewrites drop subtrees on the floor instead
## of splicing them out.

# A cell links to cells that may not be there: `down` and `next` are nil at
# the edges of the tree, which is the whole point of the representation.
{.feature: "lenientnils".}

import std / assertions
import ".." / lib / nifpools
export nifpools

type
  Node* = ptr LinkedToken

  LinkedToken* = object
    payload*: NifToken       ## kind plus the nifcore id: `TagId`, `StrId`,
                             ## a character, or a bare kind when the value
                             ## lives in `wide`
    info*: NifLineInfo
    wide*: uint64            ## `IntLit`/`UIntLit`/`FloatLit` only: the value,
                             ## which does not fit a token payload
    down*: Node              ## first child; `nil` for an atom
    next*: Node              ## next sibling

const
  BlockSize = 1024           ## cells per arena block

type
  Arena* = object
    ## A bump allocator for cells. `blocks` only ever grows; `used` is the
    ## high-water mark inside the last block.
    blocks: seq[ptr UncheckedArray[LinkedToken]]
    used: int

proc initArena*(): Arena =
  Arena(blocks: @[], used: BlockSize)   ## forces the first `alloc` to grow

proc destroy*(a: var Arena) =
  ## Frees every cell at once. Nothing is walked and no cell has a hook to
  ## run, so a tree of any shape costs one `dealloc` per block.
  for i in 0 ..< a.blocks.len:
    dealloc(a.blocks[i])
  a.blocks.setLen 0
  a.used = BlockSize

proc cellCount*(a: Arena): int {.inline.} =
  ## Cells handed out so far: a size hint for whoever renders the tree.
  if a.blocks.len == 0: 0 else: (a.blocks.len - 1) * BlockSize + a.used

proc alloc*(a: var Arena; payload: NifToken; info: NifLineInfo): Node =
  if a.used >= BlockSize:
    a.blocks.add cast[ptr UncheckedArray[LinkedToken]](
      alloc(sizeof(LinkedToken) * BlockSize))
    a.used = 0
  result = addr a.blocks[a.blocks.len - 1][a.used]
  inc a.used
  result.payload = payload
  result.info = info
  result.wide = 0'u64
  result.down = nil
  result.next = nil

proc allocWide*(a: var Arena; k: NifKind; v: uint64; info: NifLineInfo): Node =
  ## A numeric literal: the kind alone in the payload, the bits in `wide`.
  result = alloc(a, NifToken(uint32(k)), info)
  result.wide = v

# --------------------------------------------------------------- inspection

proc kind*(n: Node): NifKind {.inline.} = kind(n.payload)

proc tag*(n: Node): TagId {.inline.} =
  ## Only for a `TagLit`. The jump field of the payload is not maintained
  ## here -- `down` is the structure -- so it is masked out.
  TagId((uint32(n.payload) shr TagShift) and TagMask)

proc strId*(n: Node): StrId {.inline.} =
  ## Only for an `Ident` or a `StrLit`: cells always intern, never inline.
  ## `identToken`/`strLitToken` put the id in bit 1 upward; bit 0 is the
  ## inline/pool-ref flag, and a cell always interns.
  StrId((uint32(n.payload) shr KindBits) shr 1)

proc charVal*(n: Node): char {.inline.} =
  char(uint32(n.payload) shr KindBits)

proc intVal*(n: Node): int64 {.inline.} = cast[int64](n.wide)
proc uintVal*(n: Node): uint64 {.inline.} = n.wide
proc floatVal*(n: Node): float64 {.inline.} = cast[float64](n.wide)

proc isEmpty*(n: Node): bool {.inline.} =
  ## The `.` that stands for an absent child.
  n != nil and kind(n.payload) == DotToken

proc childCount*(n: Node): int =
  result = 0
  var k = n.down
  while k != nil:
    inc result
    k = k.next

# --------------------------------------------------------------- copying

proc copyTree*(a: var Arena; n: Node): Node =
  ## A fresh, unlinked copy of the subtree at `n`. Needed where one parsed
  ## tree ends up in more than one place -- the shared type and value of
  ## `var a, b: int` -- because a cell carries its own `next` and so can sit
  ## in exactly one sibling chain.
  if n == nil: return nil
  result = alloc(a, n.payload, n.info)
  result.wide = n.wide
  var src = n.down
  var prev: Node = nil
  while src != nil:
    let c = copyTree(a, src)
    if prev == nil: result.down = c
    else: prev.next = c
    prev = c
    src = src.next

# --------------------------------------------------------------- flattening

proc flattenOne(dest: var TokenBuf; n: Node; pool: Pool) =
  ## A name goes back through the *string* builder, not through the `StrId`
  ## one: the latter reads nifpools' global pool, and a plugin parses into its
  ## own. The builder then re-applies nifcore's inline rule, so the bytes are
  ## the ones a `TokenBuf` written directly would have had.
  case kind(n.payload)
  of TagLit:
    addParLe(dest, tag(n), n.info)
    var k = n.down
    while k != nil:
      flattenOne(dest, k, pool)
      k = k.next
    addParRi dest
  of DotToken:
    addDotToken(dest, n.info)
  of Ident:
    addIdent(dest, pool.strings[strId(n)], n.info)
  of StrLit:
    addStrLit(dest, pool.strings[strId(n)], n.info)
  of CharLit:
    addCharLit(dest, charVal(n), n.info)
  of IntLit:
    addIntLit(dest, intVal(n), n.info)
  of UIntLit:
    addUIntLit(dest, uintVal(n), n.info)
  of FloatLit:
    addFloatLit(dest, floatVal(n), n.info)
  else:
    raiseAssert "linkedtok: cannot flatten " & $kind(n.payload)

proc flatten*(dest: var TokenBuf; first: Node; pool: Pool) =
  ## Writes the sibling chain starting at `first` into `dest` as ordinary
  ## packed tokens, jumps and all -- nifcore's own `closeTag` computes those.
  ## This is the bridge back for everything that reads a `TokenBuf`: the
  ## plugin entry point, and `nifler2 tree`.
  var n = first
  while n != nil:
    flattenOne(dest, n, pool)
    n = n.next

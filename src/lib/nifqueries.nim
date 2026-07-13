#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this distribution, for details.

## Small, non-consuming queries over bounded ``nifcore`` cursors.

import std/assertions
import nifcore

func tagName*(cursor: Cursor): string {.inline.} =
  ## Returns the textual tag at ``cursor``.
  assert cursor.kind == TagLit
  cursor.tags.tagName(cursor.cursorTagId)

func tagIs*(cursor: Cursor; name: string): bool {.inline.} =
  ## Tests a cursor's tag without exposing its per-buffer tag pool.
  not cursor.cursorIsNil and cursor.kind == TagLit and cursor.tagName == name

proc findChildTag*(node: Cursor; name: string): Cursor =
  ## Finds the first direct child with tag ``name``.
  result = default(Cursor)
  if node.kind != TagLit:
    return
  var children = node.childCursor()
  while children.hasMore:
    if children.tagIs(name):
      return children
    children.skip

proc findDescendantTag*(node: Cursor; name: string): Cursor =
  ## Finds the first descendant with tag ``name`` in depth-first order.
  result = default(Cursor)
  if node.kind != TagLit:
    return
  var children = node.childCursor()
  while children.hasMore:
    if children.tagIs(name):
      return children
    if children.kind == TagLit:
      let nested = children.findDescendantTag(name)
      if not nested.cursorIsNil:
        return nested
    children.skip

proc findChildKind*(node: Cursor; kind: NifKind): Cursor =
  ## Finds the first direct child with token ``kind``.
  result = default(Cursor)
  if node.kind != TagLit:
    return
  var children = node.childCursor()
  while children.hasMore:
    if children.kind == kind:
      return children
    children.skip

proc findLastChildKind*(node: Cursor; kind: NifKind): Cursor =
  ## Finds the last direct child with token ``kind``.
  result = default(Cursor)
  if node.kind != TagLit:
    return
  var children = node.childCursor()
  while children.hasMore:
    if children.kind == kind:
      result = children
    children.skip

when isMainModule:
  import nifcoreparse

  var tree = parseFromBuffer(
    "(root (direct \"first\" \"last\") (nested (target value)))", "test")
  var root = tree.beginRead()
  doAssert root.tagIs("root")
  let direct = root.findChildTag("direct")
  doAssert direct.tagIs("direct")
  doAssert direct.findChildKind(StrLit).strVal == "first"
  doAssert direct.findLastChildKind(StrLit).strVal == "last"
  doAssert root.findDescendantTag("target").tagIs("target")
  doAssert root.findChildTag("missing").cursorIsNil
  root.endRead()

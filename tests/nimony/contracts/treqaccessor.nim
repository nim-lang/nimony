# issue #2994: comparing two calls of an imported accessor with a
# `.requires` crashed nimsem with `could not find symbol`.

import std/syncio
import deps/mreqaccessor

type Selection = object
  cursor: Cursor

func reselect(before: Selection; cursor: Cursor): bool =
  if not before.cursor.initialized or not cursor.initialized:
    return false
  before.cursor.lastOrdinal != cursor.lastOrdinal

echo reselect(Selection(cursor: initCursor(1)), initCursor(2))
echo reselect(Selection(cursor: initCursor(3)), initCursor(3))
echo reselect(Selection(), initCursor(3))

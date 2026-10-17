## This module implements symlink (symbolic link) handling on `Path`.
##
## **See also:**
## * `paths module <paths.html>`_ for path handling
## * `os module <os.html>`_ for the `string` based versions

import std/paths  # separate import for better symbol resolution
import private/oscommons
import private/ossymlinks

export paths.Path, paths.path, paths.`/`, paths.`$`

when supportedSystem:
  proc symlinkExists*(link: Path): bool {.inline, sideEffect.} =
    ## Returns true if the symlink `link` exists. Will return true
    ## regardless of whether the link points to a directory or file.
    result = symlinkExists($link)

proc createSymlink*(src, dest: Path) {.inline, raises.} =
  ## Create a symbolic link at `dest` which points to the item specified
  ## by `src`. On most operating systems, will fail if a link already exists.
  ##
  ## .. warning:: Some OS's (such as Microsoft Windows) restrict the creation
  ##   of symlinks to root users (administrators) or users with developer mode enabled.
  ##
  ## See also:
  ## * `createHardlink proc`_
  ## * `expandSymlink proc`_
  createSymlink($src, $dest)

proc createHardlink*(src, dest: Path) {.inline, raises.} =
  ## Create a hard link at `dest` which points to the item specified
  ## by `src`.
  ##
  ## See also:
  ## * `createSymlink proc`_
  createHardlink($src, $dest)

proc expandSymlink*(symlinkPath: Path): Path {.inline, raises.} =
  ## Returns the stored target of the symbolic link `symlinkPath`.
  ##
  ## This expands exactly one level of indirection, like POSIX `readlink`.
  ## If the target is itself a symbolic link, it is returned as-is rather than
  ## being expanded further. Raises if `symlinkPath` is not a symbolic link or
  ## the target cannot be read.
  ##
  ## On Windows this reads symbolic links and junctions; other reparse
  ## points raise.
  ##
  ## See also:
  ## * `createSymlink proc`_
  result = path(expandSymlink($symlinkPath))

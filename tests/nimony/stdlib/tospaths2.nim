import std/assertions
import std/private/ospaths2


when not defined(windows):
  # Test normalizePathEnd
  block:
    var path = "some/path/to/dir/"
    normalizePathEnd(path, trailingSep = false)
    assert path == "some/path/to/dir", "failed to remove trailing separator"

    path = "some/path/to/dir"
    normalizePathEnd(path, trailingSep = true)
    assert path == "some/path/to/dir/", "failed to add trailing separator"

    # Test empty path
    path = ""
    normalizePathEnd(path, trailingSep = true)
    assert path == "", "empty path should remain empty"

    # Test path with multiple trailing separators
    path = "some/path////"
    normalizePathEnd(path, trailingSep = false)
    assert path == "some/path", "failed to normalize multiple trailing separators"

    # Test path ending with /.
    path = "some/path/."
    normalizePathEnd(path, trailingSep = false)
    assert path == "some/path", "failed to normalize path ending with /."

    path = "some/path/."
    normalizePathEnd(path, trailingSep = true)
    assert path == "some/path/", "failed to normalize path ending with /. with trailing sep"

  # Test joinPath and `/` operator
  block:
    assert joinPath("dir", "file") == "dir/file"
    assert joinPath("dir/", "file") == "dir/file"
    assert joinPath("", "file") == "file"
    assert joinPath("dir", "") == "dir"
    
    # Test the `/` operator
    assert "dir" / "file" == "dir/file"
    assert "dir/" / "file" == "dir/file"
    assert "" / "file" == "file"

  # Test splitPath
  block:
    let (head, tail) = splitPath("path/to/file.txt")
    assert head == "path/to"
    assert tail == "file.txt"
    
    let (head2, tail2) = splitPath("file.txt")
    assert head2 == ""
    assert tail2 == "file.txt"

  # Test isAbsolute
  block:
    assert isAbsolute("/path/to/file") == true
    assert isAbsolute("path/to/file") == false
    assert isAbsolute("") == false

  # Test parentDir
  block:
    assert parentDir("/path/to/file") == "/path/to"
    assert parentDir("path/to/file") == "path/to"
    assert parentDir("file") == "."
    assert parentDir("/") == ""

  # Test splitFile
  block:
    let (dir, name, ext) = splitFile("/path/to/file.txt")
    assert dir == "/path/to"
    assert name == "file"
    assert ext == ".txt"
    
    let (dir2, name2, ext2) = splitFile("file")
    assert dir2 == ""
    assert name2 == "file"
    assert ext2 == ""

  import std/syncio

  # Test extractFilename
  block:
    assert extractFilename("/path/to/file.txt") == "file.txt"
    assert extractFilename("file.txt") == "file.txt"
    assert extractFilename("/path/to/") == ""

  # Test changeFileExt and addFileExt
  block:
    assert changeFileExt("file.txt", ".md") == "file.md"
    assert changeFileExt("file", ".txt") == "file.txt"
    assert changeFileExt("/path/file.txt", ".md") == "/path/file.md"
    assert addFileExt("file", ".txt") == "file.txt"
    assert addFileExt("file.txt", ".md") == "file.txt"

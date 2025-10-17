import std/paths
import std/assertions
import std/private/osseps
import std/private/ospaths2 except normalizePath
import std/pathnorm


proc normalizePath2(path: Path; dirSep = DirSep): Path =
  result = path(pathnorm.normalizePath($path, dirSep))

# func joinPath(parts: varargs[Path]): Path =
#   var estimatedLen = 0
#   var state = 0
#   for p in parts: estimatedLen += ($p).len
#   var res = newStringOfCap(estimatedLen)
#   for i in 0..high(parts):
#     joinPathImpl(res, state, $parts[i])
#   result = path(res)


func joinPath(head, tail: Path): Path {.inline.} =
  head / tail

# Normalization tests
when not defined(windows):
  block:
    var path = path("a/b/c")
    normalizePath(path)
    assert $path == "a/b/c"

    path = path("a//b/c")
    normalizePath(path)
    assert $path == "a/b/c"

    path = path("./a/b/c")
    normalizePath(path)
    assert $path == "a/b/c"

  # path joining tests
  block:
    let p1 = path("usr")
    let p2 = path("local")
    let p3 = path("bin")

    assert p1 / p2 == path("usr/local")
    assert p1 / p2 / p3 == path("usr/local/bin")

# Platform-specific tests
when defined(windows):
  block:
    var path = path(r"c:/users")
    normalizePath(path)
    assert $path == r"c:\users"

    let drive = path("c:")
    let users = path("users")
    assert drive / users == path(r"c:\users")
elif defined(posix):
  block:
    var path = path("/usr/local")
    normalizePath(path)
    assert $path == "/usr/local"

    let root = path("/usr")
    let local = path("local")
    assert root / local == path("/usr/local")

# Test path normalization
block:
  assert normalizePath2(path"a/b/c", '/') == path"a/b/c"
  assert normalizePath2(path"a//b/c", '/') == path"a/b/c"
  assert normalizePath2(path"./a/b/c", '/') == path"a/b/c"
  when defined(windows):
    assert normalizePath2(path"a/b/c", '\\') == path"a\\b\\c"
  when defined(posix):
    assert normalizePath2(path"a/b/c", '/') == path"a/b/c"

# Test path joining
block:
  assert joinPath(path"usr", path"local") == path"usr/local"
  assert joinPath(path"usr/", path"local") == path"usr/local"
  assert joinPath(path"", path"local") == path"local"
  assert joinPath(path"usr", path"") == path"usr"

  # # Test with multiple segments
  # assert joinPath(path"usr", path"local", path"bin") == path"usr/local/bin"
  # assert joinPath(path"", path"usr", path"local") == path"usr/local"

  # Test / operator
  assert path"usr" / path"local" == path"usr/local"
  assert path"usr/" / path"local" == path"usr/local"
  assert path"" / path"local" == path"local"

# Test platform-specific behavior
when defined(windows):
  block:
    assert normalizePath2(path"c:/users", '\\') == path"c:\\users"
    assert joinPath(path"c:", path"users") == path"c:\\users"
    assert path"c:" / path"users" == path"c:\\users"
elif defined(posix):
  block:
    assert normalizePath2(path"/usr/local", '/') == path"/usr/local"
    assert joinPath(path"/usr", path"local") == path"/usr/local"
    assert path"/usr" / path"local" == path"/usr/local"

# Test path normalization
block:
  assert normalizePath2(path"a/b/c", '/') == path"a/b/c"
  assert normalizePath2(path"a//b/c", '/') == path"a/b/c"
  assert normalizePath2(path"./a/b/c", '/') == path"a/b/c"
  when defined(windows):
    assert normalizePath2(path"a/b/c", '\\') == path"a\\b\\c"
  when defined(posix):
    assert normalizePath2(path"a/b/c", '/') == path"a/b/c"


block splitFile:
  when false: # TODO: bugs for tuple destructors
    assert splitFile(path("")) == (path(""), path(""), "")
    assert splitFile(path("abc/")) == (path("abc"), path(""), "")
    assert splitFile(path("/")) == (path("/"), path(""), "")
    assert splitFile(path("./abc")) == (path("."), path("abc"), "")
    assert splitFile(path(".txt")) == (path(""), path(".txt"), "")
    assert splitFile(path("abc/.txt")) == (path("abc"), path(".txt"), "")
    assert splitFile(path("abc")) == (path(""), path("abc"), "")
    assert splitFile(path("abc.txt")) == (path(""), path("abc"), ".txt")
    assert splitFile(path("/abc.txt")) == (path("/"), path("abc"), ".txt")
    assert splitFile(path("/foo/abc.txt")) == (path("/foo"), path("abc"), ".txt")
    assert splitFile(path("/foo/abc.txt.gz")) == (path("/foo"), path("abc.txt"), ".gz")
    assert splitFile(path(".")) == (path(""), path("."), "")
    assert splitFile(path("abc/.")) == (path("abc"), path("."), "")
    assert splitFile(path("..")) == (path(""), path(".."), "")
    assert splitFile(path("a/..")) == (path("a"), path(".."), "")
    assert splitFile(path("/foo/abc....txt")) == (path("/foo"), path("abc..."), ".txt")

# # execShellCmd is tested in tosproc



  when defined(macos):
    assert unixToNativePath(path"./") == path":"
    assert unixToNativePath(path"./abc") == path":abc"
    assert unixToNativePath(path"../abc") == path"::abc"
    assert unixToNativePath(path"../../abc") == path":::abc"
    assert unixToNativePath(path"/abc", path"a") == path"abc"
    assert unixToNativePath(path"/abc/def", path"a") == path"abc:def"
  elif doslikeFileSystem:
    assert unixToNativePath(path"./") == path(".\\")
    assert unixToNativePath(path"./abc") == path(".\\abc")
    assert unixToNativePath(path"../abc") == path("..\\abc")
    assert unixToNativePath(path"../../abc") == path("..\\..\\abc")
    assert unixToNativePath(path"/abc", path"a") == path("a:\\abc")
    assert unixToNativePath(path"/abc/def", path"a") == path("a:\\abc\\def")
  else:
    #Tests for unix
    assert unixToNativePath(path"./") == path"./"
    assert unixToNativePath(path"./abc") == path"./abc"
    assert unixToNativePath(path"../abc") == path"../abc"
    assert unixToNativePath(path"../../abc") == path"../../abc"
    assert unixToNativePath(path"/abc", path"a") == path"/abc"
    assert unixToNativePath(path"/abc/def", path"a") == path"/abc/def"

  block extractFilenameTest:
    assert extractFilename(path("")) == path("")
    when defined(posix):
      assert extractFilename(path("foo/bar")) == path("bar")
      assert extractFilename(path("foo/bar.txt")) == path("bar.txt")
      assert extractFilename(path("foo/")) == path("")
      assert extractFilename(path("/")) == path("")
    when doslikeFileSystem:
      assert extractFilename(path(r"foo\bar")) == path("bar")
      assert extractFilename(path(r"foo\bar.txt")) == path("bar.txt")
      assert extractFilename(path(r"foo\")) == path("")
      assert extractFilename(path(r"C:\")) == path("")

  block lastPathPartTest:
    assert lastPathPart(path"") == path""
    when defined(posix):
      assert lastPathPart(path"foo/bar.txt") == path"bar.txt"
      assert lastPathPart(path"foo/") == path"foo"
      assert lastPathPart(path"/") == path""
    when doslikeFileSystem:
      assert lastPathPart(path(r"foo\bar.txt")) == path"bar.txt"
      assert lastPathPart(path(r"foo\")) == path"foo"

  proc canon(x: string): Path =
    result = normalizePath2(path(x), '/')
  assert canon"/foo/../bar" == path"/bar"
  assert canon"foo/../bar" == path"bar"

  assert canon"/f/../bar///" == path"/bar"
  assert canon"f/..////bar" == path"bar"

  assert canon"../bar" == path"../bar"
  assert canon"/../bar" == path"/../bar"

  assert canon("foo/../../bar/") == path"../bar"
  assert canon("./bla/blob/") == path"bla/blob"
  assert canon(".hiddenFile") == path".hiddenFile"
  assert canon("./bla/../../blob/./zoo.nim") == path"../blob/zoo.nim"

  assert canon("C:/file/to/this/long") == path"C:/file/to/this/long"
  assert canon("") == path""
  assert canon("foobar") == path"foobar"
  assert canon("f/////////") == path"f"

  assert normalizePath2(path"./foo//bar/../baz", '/') == path"foo/baz"


  when not defined(windows): # TODO: ??? windows
    try:
      assert relativePath(path"/foo/bar//baz.nim", path"/foo", '/') == path"bar/baz.nim"
      assert relativePath(path"/Users/me/bar/z.nim", path"/Users/other/bad", '/') == path"../../me/bar/z.nim"

      assert relativePath(path"/Users/me/bar/z.nim", path"/Users/other", '/') == path"../me/bar/z.nim"

      # `//` is a UNC path, `/` is the current working directory's drive, so can't
      # run this test on Windows.
      when not doslikeFileSystem:
        assert relativePath(path"/Users///me/bar//z.nim", path"//Users/", '/') == path"me/bar/z.nim"
      assert relativePath(path"/Users/me/bar/z.nim", path"/Users/me", '/') == path"bar/z.nim"
      assert relativePath(path"", path"/users/moo", '/') == path""
      assert relativePath(path"foo", path"", '/') == path"foo"
      assert relativePath(path"/foo", path"/Foo", '/') == (when FileSystemCaseSensitive: path"../foo" else: path".")
      assert relativePath(path"/Foo", path"/foo", '/') == (when FileSystemCaseSensitive: path"../Foo" else: path".")
      assert relativePath(path"/foo", path"/fOO", '/') == (when FileSystemCaseSensitive: path"../foo" else: path".")
      assert relativePath(path"/foO", path"/foo", '/') == (when FileSystemCaseSensitive: path"../foO" else: path".")

      assert relativePath(path"foo", path".", '/') == path"foo"
      assert relativePath(path".", path".", '/') == path"."
      assert relativePath(path"..", path".", '/') == path".."

      assert relativePath(path"foo", path"foo") == path"."
      assert relativePath(path"", path"foo") == path""
      assert relativePath(path"././/foo", path"foo//./") == path"."

      assert relativePath(paths.getCurrentDir() / path"bar", path"foo") == path"../bar".unixToNativePath
      assert relativePath(path"bar", paths.getCurrentDir() / path"foo") == path"../bar".unixToNativePath
    except:
      assert false

  when doslikeFileSystem:
    try:
      assert relativePath(r"c:\foo.nim".path, r"C:\".path) == r"foo.nim".path
      assert relativePath(r"c:\foo\bar\baz.nim".path, r"c:\foo".path) == r"bar\baz.nim".path
      assert relativePath(r"c:\foo\bar\baz.nim".path, r"d:\foo".path) == r"c:\foo\bar\baz.nim".path
      assert relativePath(r"\foo\baz.nim".path, r"\foo".path) == r"baz.nim".path
      assert relativePath(r"\foo\bar\baz.nim".path, r"\bar".path) == r"..\foo\bar\baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\\foo\bar".path) == r"baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\\foO\bar".path) == r"baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\\bar\bar".path) == r"\\foo\bar\baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\\foo\car".path) == r"\\foo\bar\baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\\goo\bar".path) == r"\\foo\bar\baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"c:\".path) == r"\\foo\bar\baz.nim".path
      assert relativePath(r"\\foo\bar\baz.nim".path, r"\foo".path) == r"\\foo\bar\baz.nim".path
      assert relativePath(r"c:\foo.nim".path, r"\foo".path) == r"c:\foo.nim".path
    except:
      assert false

  assert joinPath(path"usr", path"") == unixToNativePath(path"usr")
  # assert joinPath(path"usr", path"") == (path"usr").dup(add path"")
  assert joinPath(path"", path"lib") == path"lib"
  # assert joinPath(path"", path"lib") == path"".dup(add path"lib")
  assert joinPath(path"", path"/lib") == unixToNativePath(path"/lib")
  assert joinPath(path"", path"/lib") == unixToNativePath(path"/lib")
  # assert joinPath(path"usr/", path"/lib") == path"usr/".dup(add path"/lib")
  assert joinPath(path"", path"") == unixToNativePath(path"") # issue #13455
  # assert joinPath(path"", path"") == path"".dup(add path"")
  assert joinPath(path"", path"/") == unixToNativePath(path"/")
  # assert joinPath(path"", path"/") == path"".dup(add path"/")
  assert joinPath(path"/", path"/") == unixToNativePath(path"/")
  # assert joinPath(path"/", path"/") == path"/".dup(add path"/")
  assert joinPath(path"/", path"") == unixToNativePath(path"/")
  # assert joinPath(path"/" / path"") == unixToNativePath(path"/") # weird test case...
  assert joinPath(path"/", path"/a/b/c") == unixToNativePath(path"/a/b/c")
  assert joinPath(path"foo/", path"") == unixToNativePath(path"foo/")
  assert joinPath(path"foo/", path"abc") == unixToNativePath(path"foo/abc")
  assert joinPath(path"foo//./", path"abc/.//") == unixToNativePath(path"foo/abc/")
  # assert path"foo//./".dup(add path"abc/.//") == unixToNativePath(path"foo/abc/")
  assert joinPath(path"foo", path"abc") == unixToNativePath(path"foo/abc")
  # assert path"foo".dup(add path"abc") == unixToNativePath(path"foo/abc")
  assert joinPath(path"", path"abc") == unixToNativePath(path"abc")

  assert joinPath(path"zook/.", path"abc") == unixToNativePath(path"zook/abc")

  # controversial: inconsistent with `joinPath("zook/.","abc")`
  # on linux, `./foo` and `foo` are treated a bit differently for executables
  # but not `./foo/bar` and `foo/bar`
  assert joinPath(path".", path"/lib") == unixToNativePath(path"./lib")
  assert joinPath(path".", path"abc") == unixToNativePath(path"./abc")

  # cases related to issue #13455
  # assert joinPath(path"foo", path"", path"") == path"foo"
  assert joinPath(path"foo", path"") == path"foo"
  assert joinPath(path"foo/", path"") == unixToNativePath(path"foo/")
  assert joinPath(path"foo/", path".") == path"foo"
  assert joinPath(path"foo", path"./") == unixToNativePath(path"foo/")
  # assert joinPath(path"foo", path"", path"bar/") == unixToNativePath(path"foo/bar/")

  # issue #13579
  assert joinPath(path"/foo", path"../a") == unixToNativePath(path"/a")
  assert joinPath(path"/foo/", path"../a") == unixToNativePath(path"/a")
  assert joinPath(path"/foo/.", path"../a") == unixToNativePath(path"/a")
  assert joinPath(path"/foo/.b", path"../a") == unixToNativePath(path"/foo/a")
  assert joinPath(path"/foo///", path"..//a/") == unixToNativePath(path"/a/")
  assert joinPath(path"foo/", path"../a") == unixToNativePath(path"a")

  when doslikeFileSystem:
    assert joinPath(path"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Common7\\Tools\\", path"..\\..\\VC\\vcvarsall.bat") == r"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat".path
    assert joinPath(path"C:\\foo", path"..\\a") == r"C:\a".path
    assert joinPath(path"C:\\foo\\", path"..\\a") == r"C:\a".path


# block: # bug #23663
#   var s: HashSet[path]
#   s.incl("/a/b/c/..".path)
#   assert "/a/b/".path in s
#   assert "/a/b/c".path notin s

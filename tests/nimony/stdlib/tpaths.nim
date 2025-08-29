import std/paths
import std/assertions
import std/private/osseps
import std/private/ospaths2 except normalizePath
import std/pathnorm


proc normalizePath2(path: Path; dirSep = DirSep): Path =
  result = initPath(pathnorm.normalizePath($path, dirSep))

# func joinPath(parts: varargs[Path]): Path =
#   var estimatedLen = 0
#   var state = 0
#   for p in parts: estimatedLen += ($p).len
#   var res = newStringOfCap(estimatedLen)
#   for i in 0..high(parts):
#     joinPathImpl(res, state, $parts[i])
#   result = initPath(res)


func joinPath(head, tail: Path): Path {.inline.} =
  head / tail

# Normalization tests
when not defined(windows):
  block:
    var path = initPath("a/b/c")
    normalizePath(path)
    assert $path == "a/b/c"

    path = initPath("a//b/c")
    normalizePath(path)
    assert $path == "a/b/c"

    path = initPath("./a/b/c")
    normalizePath(path)
    assert $path == "a/b/c"

  # initPath joining tests
  block:
    let p1 = initPath("usr")
    let p2 = initPath("local")
    let p3 = initPath("bin")

    assert p1 / p2 == initPath("usr/local")
    assert p1 / p2 / p3 == initPath("usr/local/bin")

# Platform-specific tests
when defined(windows):
  block:
    var path = initPath(r"c:/users")
    normalizePath(path)
    assert $path == r"c:\users"

    let drive = initPath("c:")
    let users = initPath("users")
    assert drive / users == initPath(r"c:\users")
elif defined(posix):
  block:
    var path = initPath("/usr/local")
    normalizePath(path)
    assert $path == "/usr/local"

    let root = initPath("/usr")
    let local = initPath("local")
    assert root / local == initPath("/usr/local")

# Test path normalization
block:
  assert normalizePath2(initPath"a/b/c", '/') == initPath"a/b/c"
  assert normalizePath2(initPath"a//b/c", '/') == initPath"a/b/c"
  assert normalizePath2(initPath"./a/b/c", '/') == initPath"a/b/c"
  when defined(windows):
    assert normalizePath2(initPath"a/b/c", '\\') == initPath"a\\b\\c"
  when defined(posix):
    assert normalizePath2(initPath"a/b/c", '/') == initPath"a/b/c"

# Test path joining
block:
  assert joinPath(initPath"usr", initPath"local") == initPath"usr/local"
  assert joinPath(initPath"usr/", initPath"local") == initPath"usr/local"
  assert joinPath(initPath"", initPath"local") == initPath"local"
  assert joinPath(initPath"usr", initPath"") == initPath"usr"
  
  # # Test with multiple segments
  # assert joinPath(initPath"usr", initPath"local", initPath"bin") == initPath"usr/local/bin"
  # assert joinPath(initPath"", initPath"usr", initPath"local") == initPath"usr/local"

  # Test / operator
  assert initPath"usr" / initPath"local" == initPath"usr/local"
  assert initPath"usr/" / initPath"local" == initPath"usr/local"
  assert initPath"" / initPath"local" == initPath"local"

# Test platform-specific behavior
when defined(windows):
  block:
    assert normalizePath2(initPath"c:/users", '\\') == initPath"c:\\users"
    assert joinPath(initPath"c:", initPath"users") == initPath"c:\\users"
    assert initPath"c:" / initPath"users" == initPath"c:\\users"
elif defined(posix):
  block:
    assert normalizePath2(initPath"/usr/local", '/') == initPath"/usr/local"
    assert joinPath(initPath"/usr", initPath"local") == initPath"/usr/local"
    assert initPath"/usr" / initPath"local" == initPath"/usr/local"

# Test path normalization
block:
  assert normalizePath2(initPath"a/b/c", '/') == initPath"a/b/c"
  assert normalizePath2(initPath"a//b/c", '/') == initPath"a/b/c"
  assert normalizePath2(initPath"./a/b/c", '/') == initPath"a/b/c"
  when defined(windows):
    assert normalizePath2(initPath"a/b/c", '\\') == initPath"a\\b\\c"
  when defined(posix):
    assert normalizePath2(initPath"a/b/c", '/') == initPath"a/b/c"


block splitFile:
  when false: # TODO: bugs for tuple destructors
    assert splitFile(initPath("")) == (initPath(""), initPath(""), "")
    assert splitFile(initPath("abc/")) == (initPath("abc"), initPath(""), "")
    assert splitFile(initPath("/")) == (initPath("/"), initPath(""), "")
    assert splitFile(initPath("./abc")) == (initPath("."), initPath("abc"), "")
    assert splitFile(initPath(".txt")) == (initPath(""), initPath(".txt"), "")
    assert splitFile(initPath("abc/.txt")) == (initPath("abc"), initPath(".txt"), "")
    assert splitFile(initPath("abc")) == (initPath(""), initPath("abc"), "")
    assert splitFile(initPath("abc.txt")) == (initPath(""), initPath("abc"), ".txt")
    assert splitFile(initPath("/abc.txt")) == (initPath("/"), initPath("abc"), ".txt")
    assert splitFile(initPath("/foo/abc.txt")) == (initPath("/foo"), initPath("abc"), ".txt")
    assert splitFile(initPath("/foo/abc.txt.gz")) == (initPath("/foo"), initPath("abc.txt"), ".gz")
    assert splitFile(initPath(".")) == (initPath(""), initPath("."), "")
    assert splitFile(initPath("abc/.")) == (initPath("abc"), initPath("."), "")
    assert splitFile(initPath("..")) == (initPath(""), initPath(".."), "")
    assert splitFile(initPath("a/..")) == (initPath("a"), initPath(".."), "")
    assert splitFile(initPath("/foo/abc....txt")) == (initPath("/foo"), initPath("abc..."), ".txt")

# # execShellCmd is tested in tosproc



  when defined(macos):
    assert unixToNativePath(initPath"./") == initPath":"
    assert unixToNativePath(initPath"./abc") == initPath":abc"
    assert unixToNativePath(initPath"../abc") == initPath"::abc"
    assert unixToNativePath(initPath"../../abc") == initPath":::abc"
    assert unixToNativePath(initPath"/abc", initPath"a") == initPath"abc"
    assert unixToNativePath(initPath"/abc/def", initPath"a") == initPath"abc:def"
  elif doslikeFileSystem:
    assert unixToNativePath(initPath"./") == initPath(".\\")
    assert unixToNativePath(initPath"./abc") == initPath(".\\abc")
    assert unixToNativePath(initPath"../abc") == initPath("..\\abc")
    assert unixToNativePath(initPath"../../abc") == initPath("..\\..\\abc")
    assert unixToNativePath(initPath"/abc", initPath"a") == initPath("a:\\abc")
    assert unixToNativePath(initPath"/abc/def", initPath"a") == initPath("a:\\abc\\def")
  else:
    #Tests for unix
    assert unixToNativePath(initPath"./") == initPath"./"
    assert unixToNativePath(initPath"./abc") == initPath"./abc"
    assert unixToNativePath(initPath"../abc") == initPath"../abc"
    assert unixToNativePath(initPath"../../abc") == initPath"../../abc"
    assert unixToNativePath(initPath"/abc", initPath"a") == initPath"/abc"
    assert unixToNativePath(initPath"/abc/def", initPath"a") == initPath"/abc/def"

  block extractFilenameTest:
    assert extractFilename(initPath("")) == initPath("")
    when defined(posix):
      assert extractFilename(initPath("foo/bar")) == initPath("bar")
      assert extractFilename(initPath("foo/bar.txt")) == initPath("bar.txt")
      assert extractFilename(initPath("foo/")) == initPath("")
      assert extractFilename(initPath("/")) == initPath("")
    when doslikeFileSystem:
      assert extractFilename(initPath(r"foo\bar")) == initPath("bar")
      assert extractFilename(initPath(r"foo\bar.txt")) == initPath("bar.txt")
      assert extractFilename(initPath(r"foo\")) == initPath("")
      assert extractFilename(initPath(r"C:\")) == initPath("")

  block lastPathPartTest:
    assert lastPathPart(initPath"") == initPath""
    when defined(posix):
      assert lastPathPart(initPath"foo/bar.txt") == initPath"bar.txt"
      assert lastPathPart(initPath"foo/") == initPath"foo"
      assert lastPathPart(initPath"/") == initPath""
    when doslikeFileSystem:
      assert lastPathPart(initPath(r"foo\bar.txt")) == initPath"bar.txt"
      assert lastPathPart(initPath(r"foo\")) == initPath"foo"

  proc canon(x: string): Path =
    result = normalizePath2(initPath(x), '/')
  assert canon"/foo/../bar" == initPath"/bar"
  assert canon"foo/../bar" == initPath"bar"

  assert canon"/f/../bar///" == initPath"/bar"
  assert canon"f/..////bar" == initPath"bar"

  assert canon"../bar" == initPath"../bar"
  assert canon"/../bar" == initPath"/../bar"

  assert canon("foo/../../bar/") == initPath"../bar"
  assert canon("./bla/blob/") == initPath"bla/blob"
  assert canon(".hiddenFile") == initPath".hiddenFile"
  assert canon("./bla/../../blob/./zoo.nim") == initPath"../blob/zoo.nim"

  assert canon("C:/file/to/this/long") == initPath"C:/file/to/this/long"
  assert canon("") == initPath""
  assert canon("foobar") == initPath"foobar"
  assert canon("f/////////") == initPath"f"

  assert normalizePath2(initPath"./foo//bar/../baz", '/') == initPath"foo/baz"


  when not defined(windows): # TODO: ??? windows
    try:
      assert relativePath(initPath"/foo/bar//baz.nim", initPath"/foo", '/') == initPath"bar/baz.nim"
      assert relativePath(initPath"/Users/me/bar/z.nim", initPath"/Users/other/bad", '/') == initPath"../../me/bar/z.nim"

      assert relativePath(initPath"/Users/me/bar/z.nim", initPath"/Users/other", '/') == initPath"../me/bar/z.nim"

      # `//` is a UNC path, `/` is the current working directory's drive, so can't
      # run this test on Windows.
      when not doslikeFileSystem:
        assert relativePath(initPath"/Users///me/bar//z.nim", initPath"//Users/", '/') == initPath"me/bar/z.nim"
      assert relativePath(initPath"/Users/me/bar/z.nim", initPath"/Users/me", '/') == initPath"bar/z.nim"
      assert relativePath(initPath"", initPath"/users/moo", '/') == initPath""
      assert relativePath(initPath"foo", initPath"", '/') == initPath"foo"
      assert relativePath(initPath"/foo", initPath"/Foo", '/') == (when FileSystemCaseSensitive: initPath"../foo" else: initPath".")
      assert relativePath(initPath"/Foo", initPath"/foo", '/') == (when FileSystemCaseSensitive: initPath"../Foo" else: initPath".")
      assert relativePath(initPath"/foo", initPath"/fOO", '/') == (when FileSystemCaseSensitive: initPath"../foo" else: initPath".")
      assert relativePath(initPath"/foO", initPath"/foo", '/') == (when FileSystemCaseSensitive: initPath"../foO" else: initPath".")

      assert relativePath(initPath"foo", initPath".", '/') == initPath"foo"
      assert relativePath(initPath".", initPath".", '/') == initPath"."
      assert relativePath(initPath"..", initPath".", '/') == initPath".."

      assert relativePath(initPath"foo", initPath"foo") == initPath"."
      assert relativePath(initPath"", initPath"foo") == initPath""
      assert relativePath(initPath"././/foo", initPath"foo//./") == initPath"."

      assert relativePath(paths.getCurrentDir() / initPath"bar", initPath"foo") == initPath"../bar".unixToNativePath
      assert relativePath(initPath"bar", paths.getCurrentDir() / initPath"foo") == initPath"../bar".unixToNativePath
    except:
      assert false

  when doslikeFileSystem:
    try:
      assert relativePath(r"c:\foo.nim".initPath, r"C:\".initPath) == r"foo.nim".initPath
      assert relativePath(r"c:\foo\bar\baz.nim".initPath, r"c:\foo".initPath) == r"bar\baz.nim".initPath
      assert relativePath(r"c:\foo\bar\baz.nim".initPath, r"d:\foo".initPath) == r"c:\foo\bar\baz.nim".initPath
      assert relativePath(r"\foo\baz.nim".initPath, r"\foo".initPath) == r"baz.nim".initPath
      assert relativePath(r"\foo\bar\baz.nim".initPath, r"\bar".initPath) == r"..\foo\bar\baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\\foo\bar".initPath) == r"baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\\foO\bar".initPath) == r"baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\\bar\bar".initPath) == r"\\foo\bar\baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\\foo\car".initPath) == r"\\foo\bar\baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\\goo\bar".initPath) == r"\\foo\bar\baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"c:\".initPath) == r"\\foo\bar\baz.nim".initPath
      assert relativePath(r"\\foo\bar\baz.nim".initPath, r"\foo".initPath) == r"\\foo\bar\baz.nim".initPath
      assert relativePath(r"c:\foo.nim".initPath, r"\foo".initPath) == r"c:\foo.nim".initPath
    except:
      assert false

  assert joinPath(initPath"usr", initPath"") == unixToNativePath(initPath"usr")
  # assert joinPath(initPath"usr", initPath"") == (initPath"usr").dup(add initPath"")
  assert joinPath(initPath"", initPath"lib") == initPath"lib"
  # assert joinPath(initPath"", initPath"lib") == initPath"".dup(add initPath"lib")
  assert joinPath(initPath"", initPath"/lib") == unixToNativePath(initPath"/lib")
  assert joinPath(initPath"", initPath"/lib") == unixToNativePath(initPath"/lib")
  # assert joinPath(initPath"usr/", initPath"/lib") == initPath"usr/".dup(add initPath"/lib")
  assert joinPath(initPath"", initPath"") == unixToNativePath(initPath"") # issue #13455
  # assert joinPath(initPath"", initPath"") == initPath"".dup(add initPath"")
  assert joinPath(initPath"", initPath"/") == unixToNativePath(initPath"/")
  # assert joinPath(initPath"", initPath"/") == initPath"".dup(add initPath"/")
  assert joinPath(initPath"/", initPath"/") == unixToNativePath(initPath"/")
  # assert joinPath(initPath"/", initPath"/") == initPath"/".dup(add initPath"/")
  assert joinPath(initPath"/", initPath"") == unixToNativePath(initPath"/")
  # assert joinPath(initPath"/" / initPath"") == unixToNativePath(initPath"/") # weird test case...
  assert joinPath(initPath"/", initPath"/a/b/c") == unixToNativePath(initPath"/a/b/c")
  assert joinPath(initPath"foo/", initPath"") == unixToNativePath(initPath"foo/")
  assert joinPath(initPath"foo/", initPath"abc") == unixToNativePath(initPath"foo/abc")
  assert joinPath(initPath"foo//./", initPath"abc/.//") == unixToNativePath(initPath"foo/abc/")
  # assert initPath"foo//./".dup(add initPath"abc/.//") == unixToNativePath(initPath"foo/abc/")
  assert joinPath(initPath"foo", initPath"abc") == unixToNativePath(initPath"foo/abc")
  # assert initPath"foo".dup(add initPath"abc") == unixToNativePath(initPath"foo/abc")
  assert joinPath(initPath"", initPath"abc") == unixToNativePath(initPath"abc")

  assert joinPath(initPath"zook/.", initPath"abc") == unixToNativePath(initPath"zook/abc")

  # controversial: inconsistent with `joinPath("zook/.","abc")`
  # on linux, `./foo` and `foo` are treated a bit differently for executables
  # but not `./foo/bar` and `foo/bar`
  assert joinPath(initPath".", initPath"/lib") == unixToNativePath(initPath"./lib")
  assert joinPath(initPath".", initPath"abc") == unixToNativePath(initPath"./abc")

  # cases related to issue #13455
  # assert joinPath(initPath"foo", initPath"", initPath"") == initPath"foo"
  assert joinPath(initPath"foo", initPath"") == initPath"foo"
  assert joinPath(initPath"foo/", initPath"") == unixToNativePath(initPath"foo/")
  assert joinPath(initPath"foo/", initPath".") == initPath"foo"
  assert joinPath(initPath"foo", initPath"./") == unixToNativePath(initPath"foo/")
  # assert joinPath(initPath"foo", initPath"", initPath"bar/") == unixToNativePath(initPath"foo/bar/")

  # issue #13579
  assert joinPath(initPath"/foo", initPath"../a") == unixToNativePath(initPath"/a")
  assert joinPath(initPath"/foo/", initPath"../a") == unixToNativePath(initPath"/a")
  assert joinPath(initPath"/foo/.", initPath"../a") == unixToNativePath(initPath"/a")
  assert joinPath(initPath"/foo/.b", initPath"../a") == unixToNativePath(initPath"/foo/a")
  assert joinPath(initPath"/foo///", initPath"..//a/") == unixToNativePath(initPath"/a/")
  assert joinPath(initPath"foo/", initPath"../a") == unixToNativePath(initPath"a")

  when doslikeFileSystem:
    assert joinPath(initPath"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Common7\\Tools\\", initPath"..\\..\\VC\\vcvarsall.bat") == r"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat".initPath
    assert joinPath(initPath"C:\\foo", initPath"..\\a") == r"C:\a".initPath
    assert joinPath(initPath"C:\\foo\\", initPath"..\\a") == r"C:\a".initPath


# block: # bug #23663
#   var s: HashSet[initPath]
#   s.incl("/a/b/c/..".initPath)
#   assert "/a/b/".initPath in s
#   assert "/a/b/c".initPath notin s

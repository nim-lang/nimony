import std/pathnorm
import std/assertions

import std/private/osseps
import std/syncio

when doslikeFileSystem:
  block: # / -> /
    var state = 0
    var result: string = ""
    addNormalizePath("//?/c:/./foo//bar/../baz", result, state, '/')
    assert result == "//?/c:/foo/baz"
    addNormalizePath("me", result, state, '/')
    assert result == "//?/c:/foo/baz/me"

  block: # / -> \
    var state = 0
    var result: string = ""
    addNormalizePath(r"//?/c:/./foo//bar/../baz", result, state, '\\')
    assert result == r"\\?\c:\foo\baz"
    addNormalizePath("me", result, state, '\\')
    assert result == r"\\?\c:\foo\baz\me"

  # block: # Append path component to UNC drive
  #   var state = 0
  #   var result: string = ""
  #   addNormalizePath(r"//?/c:", result, state, '\\')
  #   echo result
  #   assert result == r"\\?\c:"
    # addNormalizePath("Users", result, state, '\\')
    # assert result == r"\\?\c:\Users"
    # addNormalizePath("me", result, state, '\\')
    # assert result == r"\\?\c:\Users\me"

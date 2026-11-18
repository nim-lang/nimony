#
#
#            Nim's Runtime Library
#        (c) Copyright 2017 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module supports helper routines for working with `cstring`
## without having to convert `cstring` to `string`, in order to
## save allocations.
##
## See also
## ========
## * `strutils module <strutils.html>`_ for working with `string`

{.feature: "staticContracts".}

import std/[assertions, strutils]

func startsWith*(s, prefix: cstring): bool =
  ## Returns true if `s` starts with `prefix`.
  runnableExamples:
    assert startsWith(cstring"Hello, Nimion", cstring"Hello")
    assert not startsWith(cstring"Hello, Nimion", cstring"Nimion")
    assert startsWith(cstring"Hello", cstring"")

  result = false
  var i = 0
  while true:
    if prefix[i] == '\0': return true
    if s[i] != prefix[i]: return false
    inc(i)

func endsWith*(s, suffix: cstring): bool =
  ## Returns true if `s` ends with `suffix`.
  runnableExamples:
    assert endsWith(cstring"Hello, Nimion", cstring"Nimion")
    assert not endsWith(cstring"Hello, Nimion", cstring"Hello")
    assert endsWith(cstring"Hello", cstring"")

  result = false
  let slen = s.len
  let suflen = len(suffix)
  if suflen > slen: return false
  var i = 0
  let j = slen - suflen
  while i < suflen:
    if s[i + j] != suffix[i]: return false
    inc(i)
  result = true

func cmpIgnoreStyle*(a, b: cstring): int =
  ## Semantically the same as `cmp(normalize($a), normalize($b))`. It
  ## is just optimized to not allocate temporary strings. This should
  ## NOT be used to compare Nim identifier names, use `macros.eqIdent`
  ## for that. Returns:
  ## * 0 if `a == b`
  ## * < 0 if `a < b`
  ## * \> 0 if `a > b`
  runnableExamples:
    assert cmpIgnoreStyle(cstring"hello", cstring"H_e_L_Lo") == 0

  var i = 0
  var j = 0
  while true:
    while a[i] == '_': inc(i)
    while b[j] == '_': inc(j)
    let aa = toLowerAscii(a[i])
    let bb = toLowerAscii(b[j])
    result = ord(aa) - ord(bb)
    if result != 0 or aa == '\0': break
    inc(i)
    inc(j)

func cmpIgnoreCase*(a, b: cstring): int =
  ## Compares two strings in a case insensitive manner. Returns:
  ## * 0 if `a == b`
  ## * < 0 if `a < b`
  ## * \> 0 if `a > b`
  runnableExamples:
    assert cmpIgnoreCase(cstring"hello", cstring"HeLLo") == 0
    assert cmpIgnoreCase(cstring"echo", cstring"hello") < 0
    assert cmpIgnoreCase(cstring"yellow", cstring"hello") > 0

  var i = 0
  while true:
    let aa = toLowerAscii(a[i])
    let bb = toLowerAscii(b[i])
    result = ord(aa) - ord(bb)
    if result != 0 or aa == '\0': break
    inc(i)

#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Produces a Nim proc that maps a string to an enum value.
## This produces a binary tree search that is faster than
## hashing as hashing requires 2 passes over the string.

import std / [macros, assertions]

import stringviews, stringtrees

proc strAtLe(s: StringView; idx: int; ch: char): bool {.inline.} =
  result = idx < s.len and s[idx] <= ch

proc strAtLe(s: string; idx: int; ch: char): bool {.inline.} =
  result = idx < s.len and s[idx] <= ch

proc decodeSolution(dest: NimNode; s: seq[SearchNode]; i: int;
                    selector: NimNode) =
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    var cond = newTree(nnkIfStmt)
    var elifBranch = newTree(nnkElifBranch)
    elifBranch.add newCall(bindSym"strAtLe", selector, newLit(f.best[1]), newLit(f.best[0]))

    decodeSolution elifBranch, s, f.thenA, selector

    var elseBranch = newTree(nnkElse)
    decodeSolution elseBranch, s, f.elseA, selector

    cond.add elifBranch
    cond.add elseBranch
    dest.add cond

  of LinearSearch:
    var cond = newTree(nnkIfStmt)
    for x in s[i].choices:
      var elifBranch = newTree(nnkElifBranch)
      elifBranch.add newCall(bindSym"==", selector, newLit(x[0]))
      var action = newTree(nnkStmtList, newTree(nnkReturnStmt, ident(x[1])))
      elifBranch.add action
      cond.add elifBranch
    dest.add cond

proc genMatcher(body, selector: NimNode; a: openArray[Key]) =
  let solution = createSearchTree a
  decodeSolution body, solution, 0, selector

macro declareMatcher*(name: untyped; e: typedesc; start: static[int] = 1;
                      prefixLen: static[int] = 0): untyped =
  let typ = e.getTypeInst[1]
  let typSym = typ.getTypeImpl.getTypeInst # skip aliases etc to get type sym
  let impl = typSym.getImpl[2]
  expectKind impl, nnkEnumTy
  var fVal = ""
  var fStr = "" # string of current field

  var a: seq[Key] = @[]
  var i = 0
  for f in impl:
    inc i
    if i <= start+1: continue # skip `low(e)`
    case f.kind
    of nnkEmpty: continue # skip first node of `enumTy`
    of nnkSym, nnkIdent:
      fVal = f.strVal
      fStr = fVal
    of nnkAccQuoted:
      fVal = ""
      for ch in f:
        fVal.add ch.strVal
      fStr = fVal
    of nnkEnumFieldDef:
      fVal = f[0].strVal
      case f[1].kind
      of nnkStrLit:
        fStr = f[1].strVal
      of nnkTupleConstr:
        fStr = f[1][1].strVal
      of nnkIntLit:
        fStr = f[0].strVal
      else:
        let fAst = f[0].getImpl
        if fAst.kind == nnkStrLit:
          fStr = fAst.strVal
        else:
          error("Invalid tuple syntax!", f[1])
    else: error("Invalid node for enum type `" & $f.kind & "`!", f)
    a.add (fStr.substr(prefixLen), fVal)

  var body = newStmtList()
  genMatcher body, ident"sel", a

  #echo repr body
  template t(name, body, e: untyped): untyped {.dirty.} =
    proc `name`*(sel: StringView|string; onError = low(e)): e =
      body
      return onError
  result = getAst t(name, body, e)

when isMainModule:
  type
    MyEnum = enum
      UnknownValue, ValueA, ValueB = "value B", ValueC

  declareMatcher whichKeyword, MyEnum, ord(ValueA)

  assert whichKeyword(toStringViewUnsafe"ValueC") == ValueC

  const
    AllKeys = ["alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit",
     "atomic_noexcept", "auto",
     "bitand", "bitor", "bool", "break",
     "catch", "char", "char8_t", "char16_t", "char32_t", "class", "compl",
     "concept", "const", "consteval", "constexpr", "constinit", "const_cast", "continue",
     "co_await", "co_return", "co_yield",
     "decltype", "default", "delete", "do", "double", "dynamic_cast",
     "goto",
     "if", "inline", "int",
     "long",
     "main", "mutable",
     "namespace", "new", "noexcept", "not", "not_eq", "nullptr",
     "operator", "or", "or_eq",
     "private", "protected", "public",
     "reflexpr", "register", "reinterpret_cast", "requires", "return",
     "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
     "switch", "synchronized",
     "template", "this", "thread_local", "throw", "true", "try", "typedef",
     "typeid", "typename",
     "union", "unsigned", "using",
     "virtual", "void", "volatile",
     "wchar_t", "while",
     "xor", "xor_eq"]

  type
    CppKeyword = enum
      NoKeyword,
      alignas,
      alignof, andQ = "and", and_eq, asmQ = "asm", atomic_cancel, atomic_commit,
      atomic_noexcept, auto,
      bitand, bitor, bool, breakQ = "break",
      catch, char, char8_t, char16_t, char32_t, class, compl,
      conceptQ = "concept",
      constQ = "const", consteval, constexpr, constinit, const_cast,
      continueQ = "continue",
      co_await, co_return, co_yield,
      decltype, default, delete, doQ = "do", double, dynamic_cast,
      goto,
      ifQ = "if", inline, int,
      long,
      main, mutable,
      namespace, new, noexcept, notQ = "not", not_eq, nullptr,
      operator, orQ = "or", or_eq,
      private, protected, public,
      reflexpr, register, reinterpret_cast, requires, returnQ = "return",
      short, signed, sizeof, staticQ = "static", static_assert, static_cast, struct,
      switch, synchronized,
      templateQ = "template", this, thread_local, throw, trueQ = "true", tryQ = "try", typedef,
      typeid, typename,
      union, unsigned, usingQ = "using",
      virtual, void, volatile,
      wchar_t, whileQ = "while",
      xorQ = "xor", xor_eq

  declareMatcher whichCppKeyword, CppKeyword, ord(alignas)

  var i = 1
  for k in AllKeys:
    assert whichCppKeyword(toStringViewUnsafe(k)) == cast[CppKeyword](i)
    inc i
  assert whichCppKeyword(toStringViewUnsafe("u")) == NoKeyword
  assert whichCppKeyword(toStringViewUnsafe("")) == NoKeyword
  assert whichCppKeyword(toStringViewUnsafe("wusel")) == NoKeyword

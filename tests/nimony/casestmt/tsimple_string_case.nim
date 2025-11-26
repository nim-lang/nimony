
import std / [syncio, assertions]

proc effect(s: string): string =
  echo s
  result = s

proc dispatch(x: string) =
  case effect(x)
  of "foo":
    echo "foo"
  of "bar":
    echo "bar"
  else:
    echo "unknown"

proc isCppKeyword(s: string): bool =
  case s
  of "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit",
     "atomic_noexcept", "auto",
     "bitand", "bitor", "bool", "break",
     "catch", "char", "char8_t", "char16_t", "char32_t", "class", "compl",
     "concept", "const", "consteval", "constexpr", "constinit", "const_cast", "continue",
     "co_await", "co_return", "co_yield",
     "decltype", "default", "delete", "do", "double", "dynamic_cast",
     "else", "enum", "explicit", "export", "extern",
     "false", "float", "for", "friend",
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
     "xor", "xor_eq":
    result = true
  else:
    result = false

dispatch("foo")
dispatch("bar")
dispatch("other")

assert isCppKeyword("alignas")
assert isCppKeyword("alignof")
assert isCppKeyword("and")
assert isCppKeyword("and_eq")
assert isCppKeyword("asm")
assert isCppKeyword("atomic_cancel")
assert isCppKeyword("atomic_commit")
assert isCppKeyword("atomic_noexcept")
assert isCppKeyword("auto")
assert isCppKeyword("bitand")
assert isCppKeyword("bitor")
assert isCppKeyword("bool")
assert isCppKeyword("break")
assert isCppKeyword("catch")
assert isCppKeyword("char")
assert isCppKeyword("char8_t")
assert isCppKeyword("char16_t")
assert isCppKeyword("char32_t")
assert isCppKeyword("class")
assert isCppKeyword("compl")
assert isCppKeyword("concept")
assert isCppKeyword("const")
assert isCppKeyword("consteval")
assert isCppKeyword("constexpr")
assert isCppKeyword("constinit")
assert isCppKeyword("const_cast")
assert isCppKeyword("continue")
assert isCppKeyword("co_await")
assert isCppKeyword("co_return")
assert isCppKeyword("co_yield")
assert isCppKeyword("decltype")
assert isCppKeyword("default")
assert isCppKeyword("delete")
assert isCppKeyword("do")
assert isCppKeyword("double")
assert isCppKeyword("dynamic_cast")
assert isCppKeyword("else")
assert isCppKeyword("enum")
assert isCppKeyword("explicit")
assert isCppKeyword("export")
assert isCppKeyword("extern")
assert isCppKeyword("false")
assert isCppKeyword("float")
assert isCppKeyword("for")
assert isCppKeyword("friend")
assert isCppKeyword("goto")
assert isCppKeyword("if")
assert isCppKeyword("inline")
assert isCppKeyword("int")
assert isCppKeyword("long")
assert isCppKeyword("main")
assert isCppKeyword("mutable")
assert isCppKeyword("namespace")
assert isCppKeyword("new")
assert isCppKeyword("noexcept")
assert isCppKeyword("not")
assert isCppKeyword("not_eq")
assert isCppKeyword("nullptr")
assert isCppKeyword("operator")
assert isCppKeyword("or")
assert isCppKeyword("or_eq")
assert isCppKeyword("private")
assert isCppKeyword("protected")
assert isCppKeyword("public")
assert isCppKeyword("reflexpr")
assert isCppKeyword("register")
assert isCppKeyword("reinterpret_cast")
assert isCppKeyword("requires")
assert isCppKeyword("return")
assert isCppKeyword("short")
assert isCppKeyword("signed")
assert isCppKeyword("sizeof")
assert isCppKeyword("static")
assert isCppKeyword("static_assert")
assert isCppKeyword("static_cast")
assert isCppKeyword("struct")
assert isCppKeyword("switch")
assert isCppKeyword("synchronized")
assert isCppKeyword("template")
assert isCppKeyword("this")
assert isCppKeyword("thread_local")
assert isCppKeyword("throw")
assert isCppKeyword("true")
assert isCppKeyword("try")
assert isCppKeyword("typedef")
assert isCppKeyword("typeid")
assert isCppKeyword("typename")
assert isCppKeyword("union")
assert isCppKeyword("unsigned")
assert isCppKeyword("using")
assert isCppKeyword("virtual")
assert isCppKeyword("void")
assert isCppKeyword("volatile")
assert isCppKeyword("wchar_t")
assert isCppKeyword("while")
assert isCppKeyword("xor")
assert isCppKeyword("xor_eq")

assert not isCppKeyword("foo")
assert not isCppKeyword("bar")
assert not isCppKeyword("baz")
assert not isCppKeyword("qux")
assert not isCppKeyword("quux")
assert not isCppKeyword("corge")
assert not isCppKeyword("grault")
assert not isCppKeyword("")

block:
  type Result = enum none, a, b, c, d, e, f

  block:
    proc foo1(x: cstring): Result =
      const y = cstring"hash"
      result = none
      case x
      of "Andreas", "Rumpf": result = a
      of cstring"aa", "bb": result = b
      of "cc", y, "when": result = c
      of "will", "be", "generated": result = d
      of "": result = f

    var s = "Andreas"
    assert foo1(s.toCString) == a

  block:
    proc foo1(): Result =
      const y = cstring"hash"
      result = none
      case "Andreas".cstring
      of "Andreas", "Rumpf": result = a
      of cstring"aa", "bb": result = b
      of "cc", y, "when": result = c
      of "will", "be", "generated": result = d
      of "": result = f

    assert foo1() == a

  block:
    proc foo1(): Result =
      const y = cstring"hash"
      var x = "Andreas"
      result = none
      case x.toCString
      of "Andreas", "Rumpf": result = a
      of cstring"aa", "bb": result = b
      of "cc", y, "when": result = c
      of "will", "be", "generated": result = d
      of "": result = f

    assert foo1() == a

block:
  proc foo(x: int): string =
    case x
    of 1: result = "digit"
    else: result = "number"


  var r = foo(10)
  assert r == "number"

block:
  type
    E = enum A, B, C

  let s = A
  echo s

block:
  type
    E = enum A, B, C

  let s = A
  echo s

type
  E = enum A, B, C

let s = A
echo s

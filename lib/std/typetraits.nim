## Compile-time reflection on types.
##
## Nim 2 implemented these as `{.magic: "TypeTrait".}` procs, i.e. as compiler
## built-ins. Nimony implements them as ordinary **template plugins**: the
## compiler hands the plugin the argument type as NIF, the plugin transforms
## that tree, and the result is re-checked in type position. Nothing here is
## known to the compiler.
##
## A plugin runs in its own process and so cannot resolve a symbol on its own.
## It does not have to: it asks. When the plugin meets a nominal type it has not
## been given, it returns `needTypes(sym)`, the compiler appends that
## declaration to its second input and runs it again. Nothing is shipped that
## was not asked for.
##
## Unstable API.

template distinctBase*[T](t: typedesc[T]): typedesc {.plugin: "deps/typetraits".}
  ## Returns the base type for `distinct` types, or the type itself otherwise.
  ## Peels every `distinct` layer; see `distinctBaseShallow` for a single step.
  ##
  ## .. code-block:: nim
  ##   type MyInt = distinct int
  ##   type MyOtherInt = distinct MyInt
  ##   var a: distinctBase(MyOtherInt)  # int

template distinctBaseShallow*[T](t: typedesc[T]): typedesc {.plugin: "deps/typetraits".}
  ## Like `distinctBase` but removes only the immediate `distinct` layer.
  ## (Nim 2 spelled this `distinctBase(T, recursive = false)`.)
  ##
  ## .. code-block:: nim
  ##   type MyInt = distinct int
  ##   type MyOtherInt = distinct MyInt
  ##   var a: distinctBaseShallow(MyOtherInt)  # MyInt

template genericHead*[T](t: typedesc[T]): typedesc {.plugin: "deps/typetraits".}
  ## Accepts an instantiated generic type and returns its uninstantiated form.
  ## It is a compile-time error if `T` is not a generic instance.
  ##
  ## .. code-block:: nim
  ##   type Foo[T] = object
  ##   var x: genericHead(Foo[int])[float]   # Foo[float]

template stripGenericParams*[T](t: typedesc[T]): typedesc {.plugin: "deps/typetraits".}
  ## Like `genericHead`, but returns non-generic types unmodified instead of
  ## producing an error.

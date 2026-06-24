template cBindings(header, spec: string): untyped {.plugin: "deps/mcbindings".}

cBindings("foo.h", """
  typedef unsigned long size_t;
  typedef void (__cdecl *Callback)(int code);
  enum Color { Red, Green = 5, Blue };
  struct Foo { int x; char *name; };
  extern int __stdcall puts2(const char *s);
  extern struct Foo currentFoo;
""")

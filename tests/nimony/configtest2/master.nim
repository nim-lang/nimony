import std / syncio

{.emit: """
#ifdef CDEFINE
#define FROMC 123
#else
#define FROMC 456
#endif
""".}

var fromc {.importc: "FROMC".}: int

echo fromc

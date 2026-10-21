# issue #2731
import std/syncio
proc g[T](x: varargs[T]): int = x.len
proc h[T](x: varargs[T]): T = x[x.len-1]
echo g(1), " ", g(1, 2, 3), " ", g("a", "b"), " ", h("x", "yz"), " ", h(4.5)

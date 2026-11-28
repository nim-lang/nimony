import std/[assertions, syncio]
import std/strmisc

assert expandTabs("\t", 4) == "    "
assert expandTabs("\tfoo\t", 4) == "    foo "
assert expandTabs("a\tb\n\txy\t", 3) == "a  b\n   xy "

let (a, b, c) = partition("foo:bar:baz", ":")
echo a, "|", b, "|", c

let (d, e, f) = partition("foo:bar:baz", ":", right = true)
echo d, "|", e, "|", f

let (g, h, i) = partition("foobar", ":")
echo g, "|", h, "|", i

let (j, k, l) = partition("foobar", ":", right = true)
echo j, "|", k, "|", l

let (m, n, o) = rpartition("foo:bar:baz", ":")
echo m, "|", n, "|", o

let (p, q, r) = rpartition("foobar", ":")
echo p, "|", q, "|", r

echo "strmisc ok"

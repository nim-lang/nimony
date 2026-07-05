import std/syncio
import std/strutils

echo toUpperAscii("hello")
echo toLowerAscii("WORLD")
echo strip("  padded  ")
echo "a,b,c,d".split(',').len
echo "one two three".contains("two")
echo repeat('x', 5)
echo repeat("ab", 3)
echo "  trim".strip(leading = true, trailing = false)
echo "Hello".startsWith("He")
echo "Hello".endsWith("lo")
echo "banana".replace('a', 'o')
echo "foo bar foo".replace("foo", "baz")
echo $12345

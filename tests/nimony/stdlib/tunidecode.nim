import std/[assertions, syncio]
import std/unidecode

assert unidecode("北京") == "Bei Jing "
assert unidecode("Äußerst") == "Ausserst"
assert unidecode("abc") == "abc"

echo unidecode("北京")
echo unidecode("Äußerst")
echo "unidecode ok"

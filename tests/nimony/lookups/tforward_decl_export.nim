import std / [assertions, syncio]
import deps / mforward_decl_export

# A forward decl + impl in the helper module must collapse to a single
# exported symbol. Before the fix, importers saw both and overload
# resolution reported "ambiguous call".
assert isEmpty(0)
assert not isEmpty(5)
assert useInternally(0)
echo "OK"

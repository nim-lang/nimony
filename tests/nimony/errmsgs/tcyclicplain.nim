# The cycle a <-> b is entered through `a`'s plain import of `b`: every import
# on a cycle has to be `{.cyclic.}`.
import deps/mcycplain_a

discard a()

from deps/mreexport import good, uvw, foo

# should work:
discard foo
discard uvw
discard good

# should fail:
discard bar
discard xyz
discard bad

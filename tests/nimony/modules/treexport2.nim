import deps/mreexport except uzw

# should work:
discard foo
discard xyz
discard good

# should fail:
discard bar
discard uzw
discard bad

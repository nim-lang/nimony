import deps/mprocvar

proc main() =
  procvar(123)

{.feature: "assumeSync".}  # test program: globals shared freely

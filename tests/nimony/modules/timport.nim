from deps/mprocvar import nil

proc main() =
  mprocvar.procvar(123)


{.feature: "assumeSync".}  # test program: globals shared freely

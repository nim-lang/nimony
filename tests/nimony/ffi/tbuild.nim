{.build("C", "foo.c").}
{.build("C", "foo1.c", "-fno-strict-aliasing -msse").}
{.build("C.passC", "-DNIMONY_BUILD_PASSC").}
{.build("C.passC", "-DHELLO", "-DWO").}
{.build("C.passL", "-s").}

{.emit: """
#ifdef NIMONY_BUILD_PASSC
  int myFuncPass2() {
    return 32;
  }
#endif
""".}

proc myFunc(): int {.importc: "myFunc".}
proc myFunc2(): int {.importc: "myFunc2".}
proc myFuncPass(): int {.importc: "myFuncPass".}
proc myFuncPass2(): int {.importc: "myFuncPass2", nodecl.}

let s = myFunc()
let s2 = myFunc2()
let s3 = myFuncPass()
let s4 = myFuncPass2()

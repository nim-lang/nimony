type
  A = object
    x: int
  B = object
    x: int

proc `=copy`(dest: var A; src: A) {.error.}
proc `=dup`(x: A): A = A(x: x.x)

proc `=dup`(x: B): B = B(x: x.x)
proc `=copy`(dest: var B; src: B) {.error.}

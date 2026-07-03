## Pointers: `addr` and store-through-deref, the core of the linear-memory
## model (a `ptr` is a byte offset into the shared ArrayBuffer).
import std/syncio

var x = 10
let p = addr x
echo p[]
p[] = 42
echo x                    # mutated through the pointer

var arr = [1, 2, 3]
let q = addr arr[1]
q[] = 99
echo arr[0]
echo arr[1]
echo arr[2]

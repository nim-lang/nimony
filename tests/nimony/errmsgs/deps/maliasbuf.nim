# A library type with a private pointer, and funcs of its own module that write
# through it: `taliaswrite`.
type
  Buf* = object
    p: ptr int
  GBuf*[T] = object
    p: ptr T
func initBuf*(x: ptr int): Buf = Buf(p: x)
func initBufV*(x: var int): Buf = Buf(p: addr x)
func poke*(b: Buf) =
  var v = 1000000000
  copyMem(b.p, addr v, sizeof(int))
func initG*[T](x: ptr T): GBuf[T] = GBuf[T](p: x)
func pokeG*(b: GBuf[int]) =
  var v = 1000000000
  copyMem(b.p, addr v, sizeof(int))

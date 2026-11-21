import std/syncio

type
  Box[N: static[int], T] = object
    c: array[N, T]
    tag: T

func make[N: static[int], T: HasDefault](): Box[N, T] = default(Box[N, T])
func partial[T: HasDefault](x: T): Box[2, T] = Box[2, T](tag: x)

echo make[3, int]().c[2], " ", partial("a").tag, partial("b").c[1].len

import std/syncio

# A `const` computed by a call is evaluated by a sub-compile (`exprexec`)
# whose program serializes the value with one loop per array, `set` and seq
# payload. Loops nested in one serializer routine (an array of arrays) or
# in sequence (a tuple of two arrays) need index variables of their own.

type
  Color = enum red, green, blue
  Grid = object
    m: array[2, array[3, int]]
    k: int

proc grid2(): array[2, array[3, int]] = [[1, 2, 3], [4, 5, 6]]

proc grid3(): array[2, array[2, array[2, int]]] =
  result = default(array[2, array[2, array[2, int]]])
  var v = 0
  for i in 0..1:
    for j in 0..1:
      for k in 0..1:
        result[i][j][k] = v
        inc v

proc table(): array[Color, array[Color, int]] =
  result = default(array[Color, array[Color, int]])
  for a in Color:
    for b in Color: result[a][b] = ord(a) * 10 + ord(b)

proc mkGrid(): Grid = Grid(m: [[1, 2, 3], [4, 5, 6]], k: 9)
proc twoArrays(): (array[2, int], array[3, int]) = ([1, 2], [3, 4, 5])
proc mixed(): (seq[int], array[2, int], set[char], array[2, array[2, int]]) =
  (@[1, 2, 3], [4, 5], {'k', 'm'}, [[6, 7], [8, 9]])
proc mixedRows(): array[2, (seq[int], set[char], array[2, int])] =
  [(@[1], {'a'}, [2, 3]), (@[4, 5], {'b', 'c'}, [6, 7])]
proc seqRows(): array[2, seq[int]] = [@[1, 2, 3], @[4]]
proc setRows(): array[2, set[char]] = [{'a', 'b'}, {'z'}]
proc strRows(): array[2, array[2, string]] = [["a", "b"], ["c", "d"]]
proc swapRows(x: array[2, array[3, int]]): array[2, array[3, int]] = [x[1], x[0]]

const
  g2 = grid2()
  g3 = grid3()
  tab = table()
  gr = mkGrid()
  ta = twoArrays()
  mx = mixed()
  mr = mixedRows()
  sr = seqRows()
  st = setRows()
  ss = strRows()
  swapped = swapRows(g2)

echo g2[0][0], g2[0][1], g2[0][2], g2[1][0], g2[1][1], g2[1][2]
echo g3[0][0][0], g3[0][0][1], g3[0][1][0], g3[0][1][1], g3[1][0][0], g3[1][0][1], g3[1][1][0], g3[1][1][1]
echo tab[red][blue], " ", tab[green][red], " ", tab[blue][green]
echo gr.m[0][2], gr.m[1][0], " ", gr.k
echo ta[0][0], ta[0][1], ta[1][0], ta[1][1], ta[1][2]
echo mx[0].len, mx[0][2], mx[1][0], mx[1][1], " ", 'k' in mx[2], " ", 'a' in mx[2], " ", mx[3][0][1], mx[3][1][0]
echo mr[0][0].len, mr[1][0].len, mr[1][0][1], " ", 'a' in mr[0][1], " ", 'c' in mr[1][1], " ", 'a' in mr[1][1], " ", mr[0][2][1], mr[1][2][0]
echo sr[0].len, sr[1].len, sr[0][2], sr[1][0]
echo 'b' in st[0], " ", 'z' in st[1], " ", 'a' in st[1]
echo ss[0][0], ss[0][1], ss[1][0], ss[1][1]
echo swapped[0][0], swapped[1][2]

proc inGeneric[T](x: T) =
  const k = grid2()
  echo k[1][0], k[0][2]

inGeneric(1)
inGeneric("s")

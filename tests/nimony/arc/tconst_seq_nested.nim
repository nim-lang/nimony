import std/syncio

const grid = @[@[1, 2], @[3, 4]]

var g = grid
g[0].add 5
echo grid[1][0], " ", g[0].len, " ", grid[0].len

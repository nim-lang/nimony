import std/syncio

for i in 0..3:
  for j in 0..3:
    if j == i: continue
    echo i, ", ", j


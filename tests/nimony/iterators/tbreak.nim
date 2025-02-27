
import std / [syncio]

for i in 0..<3:
  for j in 0..<4:
    if j == 2:
      echo "left the loop!"
      break
    echo "A i ", i, " j ", j

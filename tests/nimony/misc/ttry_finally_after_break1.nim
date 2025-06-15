import std / syncio

while true:
  try:
    try:
      echo "try"
      break
    finally:
      echo "finally..."
  finally:
    echo "after break..."

proc test2(a: string) =
  try:
    discard
  finally:
    echo a

test2("b2")
echo "done"

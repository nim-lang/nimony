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

import std / syncio

var x = "mutable"

echo "ifelif"
if x == "abc":
  echo "a"
elif x == "def":
  echo "b"
elif x == "mutable":
  if x == "abc":
    discard
  elif x == "def":
    echo "na"
  #else:
  #  echo "yes!"
else:
  echo "c"
echo "end"

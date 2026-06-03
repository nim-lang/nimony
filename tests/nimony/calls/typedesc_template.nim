import std/[assertions, syncio]

template five*[T: SomeInteger](x: typedesc[T]): T = 5.T

echo "int8.five: ", int8.five
echo "int16.five: ", int16.five
echo "int32.five: ", int32.five
echo "int64.five: ", int64.five

echo "uint8.five: ", uint8.five
echo "uint16.five: ", uint16.five
echo "uint32.five: ", uint32.five
echo "uint64.five: ", uint64.five

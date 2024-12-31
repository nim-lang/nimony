proc main() =
  var arr: array[3, int]
  var str: string
  var cstr: cstring
  let x = arr[1]
  let xt: int = x
  arr[1] = x
  let y = str[1]
  let yt: char = y
  str[1] = y
  let z = cstr[1]
  let zt: char = z
  cstr[1] = zt

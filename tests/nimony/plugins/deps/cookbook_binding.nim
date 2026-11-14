proc label(value: string): string =
  "library: " & value

template boundLabel*(value: string): string =
  bind label
  label(value)

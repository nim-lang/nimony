import mvtbase

type
  Dog* = object of Animal
    name*: string

# overrides speak and move, but NOT identify
method speak*(d: Dog): string = "Woof"
method move*(d: Dog): string = "run"

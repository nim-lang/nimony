# Regression: inheriting from a name that is declared in two modules must
# report the ambiguity, not "concept can only inherit from other concepts".

import deps/mconceptambiguous

type
  Foo = concept of Equatable

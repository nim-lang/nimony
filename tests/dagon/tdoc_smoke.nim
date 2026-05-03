## # Markdown smoke test
##
## This module exercises *italic*, **bold**, `inline code`, and
## [external links](https://example.com).
##
## A bullet list:
##
## - first
## - second with `code`
## - third
##
## A code block:
##
## ```nim
## proc square(x: int): int =
##   ## doc inside example
##   result = x * x
## ```
##
## A small table:
##
## | name | size |
## |------|------|
## | int  | 8    |
## | char | 1    |

proc square*(x: int): int =
  ## Returns `x * x`.
  ##
  ## Example:
  ##
  ## ```nim
  ## echo square(5)  # 25
  ## ```
  result = x * x

proc shout*(s: string): string =
  ## Uppercases `s`. See also [square](#square.0.) for a numeric counterpart.
  result = s

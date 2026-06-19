import bindsympkg/private/runtime

export runtime

template generatedAccept*(box: Box): untyped {.plugin: "mbindsympkg".}

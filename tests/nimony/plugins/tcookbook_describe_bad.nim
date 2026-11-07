template defineDescribe(T: typedesc) {.plugin: "deps/mcookbook_describe".}

type Color = enum red, green

defineDescribe(Color)

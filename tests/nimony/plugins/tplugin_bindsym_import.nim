# `bindSym` in a plugin refers to a module that the plugin imports but its
# user does not (#2639): the plugin's imports join the program.
import std / syncio

template show() {.plugin: "deps/mplugin_bindsym_import".}

show()

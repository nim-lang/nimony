template failWithPluginError(s: string) {.plugin: "deps/merrortreeapi".}

echo failWithPluginError("payload")

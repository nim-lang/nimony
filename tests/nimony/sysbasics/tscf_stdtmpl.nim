#? stdtmpl(subsChar = '$', metaChar = '%') | standard
%import std/assertions
%proc greeting(name: string): string =
%  result = ""
Hello ${name}!
%  if name.len > 3:
long name
%  end if
%end proc
%assert greeting("wo") == "Hello wo!\n"
%assert greeting("world") == "Hello world!\nlong name\n"

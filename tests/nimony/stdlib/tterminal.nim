import std/assertions
import std/terminal

# --- single foreground colour ---
assert "hi".red == "\e[31mhi\e[39m"
assert "hi".green == "\e[32mhi\e[39m"
assert "hi".blue == "\e[34mhi\e[39m"
assert "hi".black == "\e[30mhi\e[39m"
assert "hi".white == "\e[37mhi\e[39m"
assert "hi".gray == "\e[90mhi\e[39m"
assert "hi".brightCyan == "\e[96mhi\e[39m"

# --- styles ---
assert "hi".bold == "\e[1mhi\e[22m"
assert "hi".dim == "\e[2mhi\e[22m"
assert "hi".underline == "\e[4mhi\e[24m"
assert "hi".italic == "\e[3mhi\e[23m"
assert "hi".inverse == "\e[7mhi\e[27m"
assert "hi".strikethrough == "\e[9mhi\e[29m"

# --- background ("on<Colour>", not "bg<Colour>") ---
assert "hi".onWhite == "\e[47mhi\e[49m"
assert "hi".onRed == "\e[41mhi\e[49m"

# --- chaining ("x".red.bold == bold(red("x")), nested resets) ---
assert "hi".red.bold == "\e[1m\e[31mhi\e[39m\e[22m"
assert "x".green.underline.bold == "\e[1m\e[4m\e[32mx\e[39m\e[24m\e[22m"
assert "sel".black.onWhite == "\e[47m\e[30msel\e[39m\e[49m"

# --- alias ---
assert "hi".grey == "hi".gray

# --- ANSI parsing ---
assert stripAnsi("this is red and bold".red.bold) == "this is red and bold"
assert stripAnsi("plain") == "plain"
assert stripAnsi("a".red & "b".blue.bold) == "ab"
assert stripAnsi("") == ""
assert visibleLen("hi".red.bold) == 2
assert visibleLen("plain") == 5

# --- the existing File-oriented API still works alongside the new funcs ---
assert ansiForegroundColorCode(fgRed) == "\e[31m"
assert ansiStyleCode(styleBright) == "\e[1m"

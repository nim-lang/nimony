import std/assertions
import std/colors   # re-exports std/rgb (Color, rgb, colRed, ...)

# --- single foreground colour ---
assert "hi".red == "\e[31mhi\e[39m"
assert "hi".green == "\e[32mhi\e[39m"
assert "hi".blue == "\e[34mhi\e[39m"
assert "hi".gray == "\e[90mhi\e[39m"
assert "hi".brightCyan == "\e[96mhi\e[39m"

# --- styles ---
assert "hi".bold == "\e[1mhi\e[22m"
assert "hi".underline == "\e[4mhi\e[24m"
assert "hi".italic == "\e[3mhi\e[23m"

# --- background ---
assert "hi".bgWhite == "\e[47mhi\e[49m"
assert "hi".bgRed == "\e[41mhi\e[49m"

# --- chaining ("x".red.bold == bold(red("x")), nested resets) ---
assert "hi".red.bold == "\e[1m\e[31mhi\e[39m\e[22m"
assert "x".green.underline.bold == "\e[1m\e[4m\e[32mx\e[39m\e[24m\e[22m"
assert "sel".black.bgWhite == "\e[47m\e[30msel\e[39m\e[49m"

# --- alias ---
assert "hi".grey == "hi".gray

# --- truecolor bridge to std/rgb (re-exported) ---
assert "x".fg(rgb(255, 128, 0)) == "\e[38;2;255;128;0mx\e[39m"
assert "x".fg(colRed) == "\e[38;2;255;0;0mx\e[39m"
assert "x".bg(rgb(0, 0, 0)) == "\e[48;2;0;0;0mx\e[49m"
assert ansiFg(colGreen) == "\e[38;2;0;128;0m"
assert ansiSgr(31) == "\e[31m"
assert ansiReset == "\e[0m"

# --- ANSI parsing ---
assert stripAnsi("this is red and bold".red.bold) == "this is red and bold"
assert stripAnsi("plain") == "plain"
assert stripAnsi("a".red & "b".blue.bold) == "ab"
assert stripAnsi("") == ""
assert visibleLen("hi".red.bold) == 2
assert visibleLen("plain") == 5

# --- homage: rainbow ---
assert rainbow("ab") == "\e[31ma\e[39m\e[33mb\e[39m"
assert rainbow("a b") == "\e[31ma\e[39m \e[33mb\e[39m"

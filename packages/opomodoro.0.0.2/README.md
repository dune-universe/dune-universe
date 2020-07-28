# OPomodoro
A simple Pomodoro timer in OCaml.

![The timer in action](screenshot.jpg)

### Usage:
`space`: pause/un-pause\
`r`: reset\
`b`: begin break\
`ctrl-c`: exit

### Running:
`dune exec ./opomodoro.exe`

### Compiling:
`dune build @default`

### Notes:
Currently the countdown is off by a second or two,\
because the method used to generate clock ticks is not ideal.

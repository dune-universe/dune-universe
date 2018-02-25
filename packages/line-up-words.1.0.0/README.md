line-up-words - Align words in an intelligent way
-------------------------------------------------

`line-up-words` is a small command line tool that tries to align words
in a sequence of lines in an intelligent way. It comes as a command
line tool and an emacs mode.

### Example

```bash
$ cat file.ml
{ name : string
; dir : Path.t
; version : string
; description : string
; archives : Path.t list Mode.Dict.t
; plugins : Path.t list Mode.Dict.t
; jsoo_runtime : string list
; requires : t list
; ppx_runtime_deps : t list
$ line-up-words < file.ml
{ name             : string
; dir              : Path.t
; version          : string
; description      : string
; archives         : Path.t list Mode.Dict.t
; plugins          : Path.t list Mode.Dict.t
; jsoo_runtime     : string list
; requires         : t list
; ppx_runtime_deps : t list
```

### Emacs mode

The emacs mode defines the interactive function `line-up-words`. For
instance, to bind it to `C-c a`, add this to your `~/.emacs`:

```scheme
(require 'line-up-words)
(global-set-key "\C-c a" 'line-up-words)
```

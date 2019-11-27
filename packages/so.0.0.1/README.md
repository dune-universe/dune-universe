# SO [![builds.sr.ht status](https://builds.sr.ht/~zapashcanon/so.svg)](https://builds.sr.ht/~zapashcanon/so?)

SO (Simple Open) is a small command line tool to quickly open file depending on their extension.

## Usage

You should have a configuration file in `${XDG_CONFIG_HOME}/so/config`. It maps extensions to programs and should look like this:

```sh
mpv : mp4 avi
firefox : html
evince : pdf
eog : jpg png gif tif
```
There's a default fallback set to `xdg-open`.

Then it's as simple as:
```sh
so ~/img/hello.jpg
```
## Build
```sh
opam update
opam install dune ocaml-xdg-basedir
dune build @all
```
## Install
```sh
dune install
```
## License

See [LICENSE].

[LICENSE]: ./LICENSE.md

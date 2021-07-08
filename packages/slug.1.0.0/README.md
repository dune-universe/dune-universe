# ocaml-slug
Url safe slug generator for OCaml

This library uses algorithm and data from [node-slugify](https://github.com/simov/slugify)

## Installation

```
opam install slug
```

If you use [esy](https://esy.sh)

```
esy add @opam/slug
```

## Usage

- OCaml syntax:

```ocaml
Slug.slugify "my string";;
- : string = "my-string"

(* Custom separator *)
Slug.slugify ?sep: "_" "my string";;
- : string = "my_string" 

(* Retain uppercase *)
Slug.slugify ?lowercase: false "My String";;
- : string = "My-String" 

(* Use locale *)
let with_vi = Slug.(Charmap.mk_charmap [Slug_data.base; Slug_data.vi]);;
- : Charmap.t = <abstr>
Slug.slugify ?charmap: with_vi "Đ";;
- : string = "d" 
Slug.slugify "Đ";;
- : string = "dj"

(* Custom characters map *)
let custom_map = Slug.(Charmap.mk_charmap [Slug_data.base; [ ("M", "z"); ("m", "z") ]]);;
val custom_map : Charmap.t = <abstr>

Slug.slugify ?charmap: custom_map "Mm";;
- : string = "zz"
```

- ReasonML syntax

```reason
Slug.slugify("my string");
- : string = "my-string"

/* Custom separator */
Slug.slugify(~sep = "_", "my string");
- : string = "my_string" 

/* Retain uppercase */
Slug.slugify(~lowercase = false, "My String");
- : string = "My-String" 

/* Use locale */
let with_vi = Slug.(Charmap.mk_charmap([Slug_data.base, Slug_data.vi]));
let with_vi : Charmap.t = <abstr>
Slug.slugify(~charmap = with_vi, "Đ");
- : string = "d" 
Slug.slugify("Đ");
- : string = "dj"

/* Custom characters map */
let custom_map = Slug.(Charmap.mk_charmap([Slug_data.base, [ ("M", "z"), ("m", "z") ]]));;
let custom_map : Charmap.t = <abstr>

Slug.slugify(~charmap = custom_map, "Mm");
- : string = "zz"
```

## Notice

Please don't send PR to update `data/*`. They are auto-generated from [upstream library](https://github.com/simov/slugify).

Please send PRs about new locales to node-slugify.

If you really need it, you can use a `custom_map` instead.

## License
MIT. Data are downloaded from [node-slugify](https://github.com/simov/slugify) 
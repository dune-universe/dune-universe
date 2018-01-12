# ocaml-socialpeek [![Build Status](https://travis-ci.org/erizocosmico/ocaml-socialpeek.svg?branch=master)](https://travis-ci.org/erizocosmico/ocaml-socialpeek)

OCaml library to extract social information such as Twitter cards or OpenGraph data from webpages and HTML.

[Check out the documentation](http://erizocosmi.co/ocaml-socialpeek/socialpeek/Socialpeek/index.html)

## Install

```bash
opam install socialpeek
```

## Usage

### Get twitter card

```ocaml
open Socialpeek

let () =
  (** you can use `from_html` instead if you already have the HTML *)
  let twitter_card = from_url "http://something.cool" |> Twitter.get_card in
  match twitter_card with
  | Twitter.Summary data -> (** Do something with data *)
  | Twitter.Summary_large_image data -> (** Do something with data *)
  | Twitter.App data -> (** Do something with data *)
  | Twitter.Player data -> (** Do something with data *)
```

Twitter cards can have 4 different shapes: `summary`, `summary_large_image`, `app` or `player` (more info about [twitter cards](https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/markup)).
So the result of `Twitter.get_card` is a variant type that holds only the data a certain type of card can have.

### Get Opengraph data

```ocaml
open Socialpeek

let () =
  (** you can use `from_html` instead if you already have the HTML *)
  let og_data = from_url "http://something.cool" |> Opengraph.get_data in
  (** do something with the data *)
```

### Get twitter and opengraph data

```ocaml
open Socialpeek

let () =
  (** you can use `from_html` instead if you already have the HTML *)
  let tags = from_url "http://something.cool" in
  let og_data = Opengraph.get_data tags in
  let twitter_card = Twitter.get_card tags in
  (** do something with the data *)
```

## License

MIT, see [LICENSE](/LICENSE)

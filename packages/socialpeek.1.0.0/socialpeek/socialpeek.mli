open Base

module Twitter = Twitter
module Metatags = Metatags
module Opengraph = Opengraph

type error = Error_code of int | Missing_location_header

(** Get the grouped metatags from the given HTML string. The grouped metatags are not useful
    by themselves, but are the intermediate result to get the Twitter card and OpenGraph data
    without having to recompute everything again from the HTML in every step.
    Result of this function should be given as input to the corresponding methods in the Twitter and
    Opengraph modules. *)
val from_html : string -> Metatags.grouped_metatags

(** Get the grouped metatags from the given webpage URL. The grouped metatags are not useful
    by themselves, but are the intermediate result to get the Twitter card and OpenGraph data
    without having to recompute everything again from the HTML in every step.
    The operation may fail, so it returns a `Result` instead of just the metatags. *)
val from_url : string -> (Metatags.grouped_metatags, error) Result.t
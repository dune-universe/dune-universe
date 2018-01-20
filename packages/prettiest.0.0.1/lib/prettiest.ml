open Base

let const x _ = x

let replicate n x = List.init n ~f:(const x)

let replicate_char n c = String.init n ~f:(const c)

module type Layout = sig
  type t
  val text : string -> t
  val flush : t -> t
  val (<>) : t -> t -> t
  val render : t -> string
end

module type Poset = sig
  type t
  val (<<) : t -> t -> bool
end

module Measure : sig
  type t [@@deriving compare]
  val valid : int -> t -> bool
  include Layout with type t := t
  include Poset with type t := t
end = struct
  type t = {
    height: int;
    max_width: int;
    last_width: int;
  } [@@deriving compare]

  let valid w x = x.max_width <= w

  let text s = {
    height = 0;
    last_width = String.length s;
    max_width = String.length s;
  }

  let flush a = {
    height = a.height + 1;
    last_width = 0;
    max_width = a.max_width;
  }

  let (<>) a b = {
    height = a.height + b.height;
    last_width = a.last_width + b.last_width;
    max_width = max a.max_width (a.last_width + b.max_width);
  }

  let render m =
    String.concat ~sep:"\n"
      (replicate m.height (replicate_char m.max_width 'x') @
       [replicate_char m.last_width 'x'])

  let (<<) m1 m2 =
    m1.height <= m2.height &&
    m1.max_width <= m2.max_width &&
    m1.last_width <= m2.last_width
end

module Text : Layout = struct
  type t = string list lazy_t

  let render xs = String.concat ~sep:"\n" (force xs)

  let text s = lazy [s]

  let (<>) xs ys =
    lazy (
      match force ys with
      | [] -> failwith "<>: empty ys"
      | y :: ys ->
        let xs = force xs in
        match List.split_n xs (List.length xs - 1) with
        | xs0, [x] ->
          let indent = replicate_char (String.length x) ' ' in
          xs0 @ [x ^ y] @ List.map ~f:(fun y -> indent ^ y) ys
        | _ -> failwith "<>: empty xs"
    )

  let flush xs = lazy (force xs @ [""])
end

module MeasureText : sig
  type t [@@deriving compare]
  val valid : int -> t -> bool
  include Layout with type t := t
  include Poset with type t := t
end = struct
  type t = Measure.t * Text.t

  let compare (m, _) (m', _) = Measure.compare m m'

  let valid w (m, _) = Measure.valid w m

  let (<<) (m1, _) (m2, _) = Measure.(m1 << m2)

  let (<>) (m1, a) (m2, b) = (
    Measure.(m1 <> m2),
    Text.(a <> b)
  )

  let flush (m, a) = (Measure.flush m, Text.flush a)

  let text s = (Measure.text s, Text.text s)

  let render (_, a) = Text.render a
end

type t = int -> MeasureText.t list

let pareto =
  let rec go acc = function
    | [] -> acc
    | (x :: xs) ->
      if List.exists acc ~f:(fun y -> MeasureText.(y << x))
      then go acc xs
      else go (x :: List.filter acc ~f:(fun y -> not MeasureText.(x << y))) xs
  in go []

let (<>) xs ys = fun w ->
  List.cartesian_product (xs w) (ys w) |>
  List.map ~f:(fun (x, y) -> MeasureText.(x <> y)) |>
  List.filter ~f:(MeasureText.valid w) |>
  pareto

let flush xs = fun w -> pareto (List.map ~f:MeasureText.flush (xs w))

let text s = fun w ->
  List.filter ~f:(MeasureText.valid w)
    [MeasureText.text s]

let render w xs =
  List.min_elt (xs w) ~cmp:MeasureText.compare |>
  Option.map ~f:MeasureText.render

let fail = fun _ -> []

let choice xss = fun w -> List.concat_map ~f:(fun xs -> xs w) xss |> pareto

let empty = text ""

let nest n x = text (replicate_char n ' ') <> x

module Infix = struct
  let (!^) = text

  let (<>) = (<>)
  let ($$) a b = flush a <> b
  let (<|>) xs ys = choice [xs; ys]

  let (<+>) x y = x <> text " " <> y

  let (</>) a b = choice [
      a <+> b;
      a $$ b;
    ]

  let (<//>) a b = choice [
      a <+> b;
      a $$ nest 2 b;
    ]
end

open Infix

let fold f = function
  | [] -> empty
  | x :: xs -> List.fold ~init:x ~f:f xs

let vcat = fold ($$)

let hcat = fold (<>)

let hsep = fold (<+>)

let sep = function
  | [] -> empty
  | xs -> hsep xs <|> vcat xs

let intersperse ~sep:sep =
  let rec go acc = function
    | [] -> List.rev acc
    | [x] -> go (x :: acc) []
    | x :: xs -> go ((x <> sep) :: acc) xs
  in go []

let intersperse_map ~f:f ~sep:sep =
  let rec go acc = function
    | [] -> List.rev acc
    | [x] -> go (f x :: acc) []
    | x :: xs -> go ((f x <> sep) :: acc) xs
  in go []

module Characters = struct
  let qmark = text "?"
  let bang = text "!"
  let at = text "@"
  let pound = text "#"
  let dollar = text "$"
  let percent = text "%"
  let caret = text "^"
  let ampersand = text "&"
  let star = text "*"

  let comma = text ","
  let dot = text "."
  let bar = text "|"
  let colon = text ":"
  let equals = text "="

  let plus = text "+"
  let minus = text "-"
  let underscore = text "_"
  let tilde = text "~"

  let squote = text "'"
  let dquote = text "\""
  let bquote = text "`"

  let slash = text "/"
  let bslash = text "\\"

  let lt = text "<"
  let gt = text ">"
  let lbrack = text "["
  let rbrack = text "]"
  let lbrace = text "{"
  let rbrace = text "}"
  let lparen = text "("
  let rparen = text ")"

  let space = text " "
end

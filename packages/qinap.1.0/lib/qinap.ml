open Base

(* Helper functions for working with lists of characters. -------------------- *)

module Clist = struct
  type t = char list

  let to_string cs =
    List.map cs ~f:Char.to_string |> String.concat

  let of_string s =
    let rec loop idx acc =
      if idx < 0 then acc
      else loop (idx - 1) (s.[idx] :: acc)
    in
    loop (String.length s - 1) []
end

(* Helper functions for working with strings. -------------------------------- *)

module Ext_string = struct
  let hd s =
    if String.length s = 0 then None else Some s.[0]

  let tl s =
    let n = String.length s in
    if n = 0 then "" else String.suffix s (n - 1)
end

(* Type of parsers. ---------------------------------------------------------- *)

type 'a parser = string -> ('a * string) option

(* Primitive parsers. -------------------------------------------------------- *)

let result x = fun s -> Some (x, s)
let zero = fun _ -> None
let item = fun s -> match Ext_string.hd s with
  | None -> None
  | Some c -> Some (c, Ext_string.tl s)

(* Primitive combinators. ---------------------------------------------------- *)

let ( => ) p f = fun s -> match p s with
  | None -> None
  | Some (x, s') -> Some (f x, s')

let ( let* ) p f = fun s -> match p s with
  | None -> None
  | Some (x, s') -> f x s'

let ( <|> ) p q = fun s -> match p s with
  | None -> q s
  | Some _ as x -> x

(* Derived combinators. ------------------------------------------------------ *)

let ( >> ) p q = let* _ = p in q

let ( << ) p q =
  let* x = p in
  let* _ = q in
  result x

let ( <~> ) p ps =
  let* x = p in
  let* xs = ps in
  result (x :: xs)

(* Note: this can't be rewritten as (p <~> many p) <|> result [].
   OCaml is not lazy, so that generates an infinite loop. *)
let rec many p =
  let any =
    let* x = p in
    let* xs = many p in
    result (x :: xs)
  in
  any <|> result []

let many1 p = p <~> many p

(* Derived parsers. ---------------------------------------------------------- *)

let satisfy ~f =
  let* x = item in
  if f x then result x else zero

let char c = satisfy ~f:(Char.equal c)

let rec clist = function
  | [] -> result []
  | c :: cs -> char c <~> clist cs

let string s = Clist.of_string s |> clist => Clist.to_string

let rec one_of = function
  | [] -> zero
  | c :: cs -> char c <|> one_of cs

let between p ~l ~r = l >> p << r

(* Parsers and combinators for dealing with whitespace. ---------------------- *)

let sep_by1 p ~sep =
  let p' =
    let* _ = sep in p in
  let* x = p in
  let* xs = many p' in
  result (x :: xs)

let sep_by p ~sep = (sep_by1 p ~sep) <|> result []

let space = satisfy ~f:Char.is_whitespace
let spaces = many1 space

(* Parsers and combinators for dealing with numbers. ------------------------- *)

let digit = satisfy ~f:Char.is_digit
let digits = many digit => Clist.to_string
let digits1 = many1 digit => Clist.to_string
let natural = digits1 => Int.of_string

let integer =
  let un_op = ((char '-') >> result Int.neg)
              <|> (result (fun x -> x)) in
  let* op = un_op in
  let* x = natural in
  result (op x)

let float =
  let un_op = ((char '-') >> result Float.neg)
              <|> (result (fun x -> x))
  and maybe_dot = ((char '.') => Char.to_string)
                  <|> (result "") in
  let* op = un_op in
  let* whole = digits1 in
  let* sep = maybe_dot in
  let* decimal = digits1
  in
  result [whole; sep; decimal]
  => String.concat => Float.of_string => op

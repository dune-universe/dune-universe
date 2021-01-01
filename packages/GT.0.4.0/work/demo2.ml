open Format

type expr = Const of int | Var of string | Binop of string * expr * expr

let rec show = function
  | Const n -> sprintf "Const %d" n
  | Var x -> sprintf "Var %s" x
  | Binop (o, l, r) -> sprintf "Binop (%S , %s , %s)" o (show l) (show r)

let prio = function "*" | "/" -> 2 | "+" | "-" -> 1 | _ -> 0
let br s = asprintf "(%s)" s

let pretty e =
  let rec helper p = function
    | Const n -> string_of_int n
    | Var x -> x
    | Binop (o, l, r) ->
        let po = prio o in
        sprintf "%s %s %s" (helper po l) o (helper po r)
        |> if po <= p then br else Fun.id in
  helper min_int e

let gcata i tr = function
  | Const n -> tr#const i n
  | Var x -> tr#var i x
  | Binop (o, l, r) -> tr#binop i o l r

let rec pretty inh e =
  gcata inh
    (object
       method const _ n = string_of_int n

       method var _ x = x

       method binop p o l r =
         let po = prio o in
         sprintf "%s %s %s" (pretty po l) o (pretty po r)
         |> if po <= p then br else Fun.id
    end )
    e

(* let%test _ = pretty () (Var "x") = "x" *)

class virtual ['i, 'self, 's] show_expr =
  object
    method virtual const : 'i -> int -> 's

    method virtual var : 'i -> string -> 's

    method virtual binop : 'i -> string -> expr -> expr -> 's
  end

class pretty fself =
  object
    inherit [int, _, string] show_expr

    method const _ = sprintf "%d"

    method var _ = Fun.id

    method binop p o l r =
      let po = prio o in
      sprintf "%s %s %s" (fself po l) o (fself po r)
      |> if po <= p then br else Fun.id
  end

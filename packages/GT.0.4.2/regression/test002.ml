[@@@warning "-8"]

open GT

module Expr = struct
    @type 'self t =
    [ `Var   of string
    | `Const of int
    | `Binop of (int -> int -> int) * string * 'self * 'self
    ]

    class ['a] toString (fself: unit -> 'a -> string) =
      object
        inherit [unit, 'a, string, unit, 'a t, string] t_t
        method c_Var   _ _   s     = s
        method c_Const _ _   n     = string_of_int n
        method c_Binop _ _ _ s x y = "(" ^ (fself () x) ^ s ^ (fself () y) ^ ")"
      end

    class ['a] eval do_var (fself: _ -> 'a t -> _) =
      object
        inherit [ unit, 'a, int, unit, 'a t, int] t_t
        method c_Var   _ _ x       = do_var x
        method c_Const _ _ n       = n
        method c_Binop _ _ f _ x y = f (fself () x) (fself () y)
      end

  end

let _ =
  let toString () e = GT.transform(Expr.t) (new Expr.toString) () e in
  let eval    s i e = GT.transform(Expr.t) (new Expr.eval   s) i e in
  let e = `Binop ((+), "+", `Const 1, `Var "a") in

  let s = toString () e in
  let v = eval (fun "a" -> 2) () e in
  Printf.printf "%s\n" s;
  Printf.printf "%d\n" v

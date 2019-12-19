
(* how to merge computation results *)

type t = Null
       | Cat_into of Utls.filename
       | Sort_cat_into of Utls.filename

let of_string out_fn = function
  | "n" -> Null
  | "c" -> Cat_into out_fn
  | "s" -> Sort_cat_into out_fn
  | other -> failwith ("Mux.of_string: unknown mux mode: " ^ other)

(** Time-stamp: <modified the 29/08/2019 (at 14:55) by Erwan Jahier> *)

(* exported *)
type rhs = Soc.var_expr list
type lhs = Soc.var_expr list
type t = Lic.clock * rhs * lhs * Soc.atomic_operation * Lxm.t



(* exported *)
let to_string_msg: (t -> string) = 
  fun (c, i, o, p, _) ->
  (* Version surchargée de Soc.string_of_operation pour afficher les "=" *)
  let string_of_operation = function
    | Soc.Assign -> ""
    | op -> SocUtils.string_of_operation op
  in
  let string_of_params p = String.concat ", " (List.map SocUtils.string_of_filter p) in
    if o = [] then
      Format.sprintf "%s(%s)" (string_of_operation p) (string_of_params i)
    else
      Format.sprintf "%s = %s(%s) %s"
        (string_of_params o)
        (string_of_operation p) (string_of_params i) (Lic.string_of_clock c)

let to_string: (t -> string) = 
  fun (_c, i, o, p,_) ->
  (* Version surchargée de SocUtils.string_of_operation : l'objectif est d'afficher,
     en cas de cycle combinatoire, un message d'erreur qui parle le plus possible
     à l'utilisateur qui a programmé en V6... Pour cela le mieux (je pense) est 
     simplement de rendre la variable sur laquelle porte le cycle
*)
  let string_of_operation = function
    | Soc.Assign -> ""
    | Soc.Method((n, _sk),_sname) -> n
    | Soc.Procedure(name,_,_)   -> name

  in
  let string_of_params p = String.concat ", " (List.map SocUtils.string_of_filter p) in
    if o = [] then
      Format.sprintf "%s(%s)"
        (string_of_operation p)
        (string_of_params i)
    else
      Format.sprintf "%s = %s(%s)"
        (string_of_params o)
        (string_of_operation p)
        (string_of_params i)

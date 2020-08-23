open! Base
open! Import

let gen_symbol prefix ~loc =
  let sym = gen_symbol ~prefix () in
  pvar ~loc sym, evar ~loc sym
;;

let map_with_context list ~f =
  let rec loop prefix suffix acc =
    match suffix with
    | [] -> List.rev acc
    | x :: suffix ->
      loop (x :: prefix) suffix (f x ~context:(List.rev_append prefix suffix) :: acc)
  in
  loop [] list []
;;

let unsupported ~loc what =
  Location.raise_errorf
    ~loc
    "%s"
    (String.concat [ "ppx_accessor"; "unsupported"; what ] ~sep:": ")
;;

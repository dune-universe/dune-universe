open Batteries;;
open Jhupllib_multimap;;
open Jhupllib_pp_utils;;

module Make
    (M : Multimap_sig)
    (K_pp : Pp with type t = M.key)
    (V_pp : Pp with type t = M.value)
=
struct
  let pp formatter m =
    let pp_values formatter vs =
      pp_concat_sep_delim "{" "}" "," V_pp.pp formatter vs
    in
    let pp_mapping formatter (k,vs) =
      Format.fprintf formatter "%a =>@ %a" K_pp.pp k pp_values vs
    in
    pp_concat_sep_delim "{" "}" "," pp_mapping formatter
      (M.keys m |> Enum.map (fun k -> (k, M.find k m)))
  ;;
  let show = pp_to_string pp;;
end;;

(* This module contains utilities for pretty-printing using the Formatter
   module. *)
open Batteries;;
open Format;;

type 'a pretty_printer = (formatter -> 'a -> unit);;

let pp_concat_sep sep pp_item formatter items =
  pp_open_hvbox formatter 2;
  pp_print_cut formatter ();
  begin
    match Enum.get items with
    | None -> ()
    | Some first_item ->
      pp_item formatter first_item;
      items
      |> Enum.iter
        (fun cont_item ->
           pp_print_string formatter sep;
           pp_print_space formatter ();
           pp_item formatter cont_item
        )
  end;
  pp_close_box formatter ();
  pp_print_cut formatter ();
;;

let pp_concat_sep_delim start stop sep pp_item formatter items =
  pp_open_box formatter 0;
  pp_print_string formatter start;
  pp_concat_sep sep pp_item formatter items;
  pp_print_string formatter stop;
  pp_close_box formatter ();
;;

let pp_tuple pp_a pp_b formatter (a,b) =
  Format.fprintf formatter "@[(%a,@ %a)@]" pp_a a pp_b b
;;

let pp_triple pp_a pp_b pp_c formatter (a,b,c) =
  Format.fprintf formatter "@[(%a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c
;;

let pp_quadruple pp_a pp_b pp_c pp_d formatter (a,b,c,d) =
  Format.fprintf formatter "@[(%a,@ %a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c pp_d d
;;

let pp_quintuple pp_a pp_b pp_c pp_d pp_e formatter (a,b,c,d,e) =
  Format.fprintf formatter "@[(%a,@ %a,@ %a,@ %a,@ %a)@]"
    pp_a a pp_b b pp_c c pp_d d pp_e e
;;

let pp_list pp_item formatter lst =
  pp_concat_sep_delim "[" "]" ";" pp_item formatter @@ List.enum lst
;;

let pp_map pp_k pp_v enum formatter dict =
  let pp_kv_pair formatter (k,v) =
    Format.fprintf formatter "@[<hv 2>%a ->@ %a@]" pp_k k pp_v v
  in
  pp_concat_sep_delim "{" "}" "," pp_kv_pair formatter @@ enum dict
;;

let pp_set pp_el enum formatter set =
  pp_concat_sep_delim "{" "}" "," pp_el formatter @@ enum set
;;

let pp_option pp_el formatter el =
  match el with
  | Some x -> Format.fprintf formatter "Some(@[%a@])" pp_el x;
  | None -> Format.pp_print_string formatter "None"
;;

let pp_to_string pp x =
  let buffer = Buffer.create 80 in
  let formatter = formatter_of_buffer buffer in
  pp formatter x;
  pp_print_flush formatter ();
  Buffer.contents buffer
;;

let pp_suffix pp str formatter x =
  pp formatter x;
  Format.pp_print_string formatter str
;;

module type Pp =
sig
  type t
  val pp : t pretty_printer
end

module Set_pp(S : Set.S)(P : Pp with type t = S.elt) =
struct
  let pp = pp_set P.pp S.enum;;
  let show = pp_to_string pp;;
end;;

module Map_pp(M : Map.S)(P : Pp with type t = M.key) =
struct
  let pp pp_value = pp_map P.pp pp_value M.enum;;
  let show pp_value = pp_to_string @@ pp pp_value;;
end;;

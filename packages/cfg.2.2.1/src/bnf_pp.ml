open Format
open Bnf_spec.Bnf
open Spec

let before_first iterator f_begin f_end fall =
  let start = ref true in
  let do_el el =
    if !start then (start := false; fall el)
    else (
      f_begin ();
      let res = fall el in
      f_end ();
      res) in
  iterator do_el

let string_of_t t = Printf.sprintf "%S" t
let string_of_nt nt = Printf.sprintf "<%S>" nt

let string_of_symbol = function
  | T t -> string_of_t t
  | NT nt -> string_of_nt nt

let rec pp_prod ppf = function
  | [] -> ()
  | [h] -> fprintf ppf "%s" (string_of_symbol h)
  | h::t -> fprintf ppf "%s@ " (string_of_symbol h); pp_prod ppf t

let pp_prods ppf =
  before_first
    ProdSet.iter
    (fun _ -> fprintf ppf "@ @[| ")
    (pp_close_box ppf)
    (fun (_, sym_list) -> fprintf ppf "@[%a@]" pp_prod sym_list)

let pp_live_prods ppf =
  before_first
    ProdMap.iter
    (fun _ -> fprintf ppf "@ @[| ")
    (pp_close_box ppf)
    (fun (_, sym_list) n -> fprintf ppf "@[%a (%d)@]" pp_prod sym_list n)

let pp_nt ppf nt =
  fprintf ppf "@[<0>%s @[<1>:= %a .@]@]" (string_of_nt nt) pp_prods

let pp_live_nt ppf nt (d, prods) =
  fprintf ppf "@[<0>%s (%d) @[<1>:= %a .@]@]"
    (string_of_nt nt) d pp_live_prods prods

let nop _ = ()

let pp_nt_map ppf = before_first NTMap.iter force_newline nop (pp_nt ppf)
let pp_live_nts ppf = before_first NTMap.iter force_newline nop (pp_live_nt ppf)

let pp_ts ppf =
  before_first TSet.iter force_newline nop
    (fun t -> pp_print_string ppf (string_of_t t))

let pp_nts ppf =
  before_first NTSet.iter force_newline nop
    (fun nt -> pp_print_string ppf (string_of_nt nt))

let pp_prods ppf =
  before_first ProdSet.iter force_newline nop (fun (_, sl) -> pp_prod ppf sl)

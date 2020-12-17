open Ppxlib

let verbose = match Sys.getenv_opt "PPX_JSOO_DEBUG" with
  | None | Some "0" | Some "false" | Some "no" -> 0
  | Some s ->
    match s with
    | "true" -> Format.printf "ppx_jsoo_debug activated\n@."; 1
    | s -> match int_of_string_opt s with
      | Some i -> Format.printf "ppx_jsoo_debug activated\n@."; i
      | None -> 0

let debug ?(v=1) fmt =
  if verbose >= v then Format.ksprintf (fun s -> Format.eprintf "%s@." s) fmt
  else Printf.ifprintf () fmt

let jsoo = "jsoo"
let _jsoo = "_jsoo"
let to_jsoo = "to" ^ _jsoo
let of_jsoo = "of" ^ _jsoo
let jsoo_conv = jsoo ^ "_conv"
let _jsoo_conv = _jsoo ^ "_conv"
let _to_jsoo = "_" ^ to_jsoo
let _of_jsoo = "_" ^ of_jsoo
let tuple_jsoo = "_tupjsoo"
let jsoo_mod s = "Ezjs_min." ^ s
let js_mod s = jsoo_mod s

let mkl ~loc txt = { Asttypes.txt; loc }
let mknol txt = mkl ~loc:!Ast_helper.default_loc txt

let lid s = Longident.parse s
let llid ~loc s = mkl ~loc (lid s)

let jsoo_name name = if name = "t" then jsoo else name ^ _jsoo
let jsoo_name_to name = if name = "t" then to_jsoo else name ^ _to_jsoo
let jsoo_name_of name = if name = "t" then of_jsoo else name ^ _of_jsoo
let jsoo_name_conv name = if name = "t" then jsoo_conv else name ^ _jsoo_conv

let new_var = let i = ref (-1) in fun () -> incr i; "v" ^ string_of_int !i

let get_string_attr = function
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _)); _}, _); _}] -> Some s
  | _ -> None

type field_attributes = {
  fa_meth : bool;
  fa_meth_cb : bool option;
  fa_cb : bool option;
  fa_prop : string option;
  fa_opt : string option;
  fa_key : string;
  fa_ignore : bool;
  fa_array : bool option;
  fa_assoc : bool option;
  fa_number : bool option;
  fa_case : bool;
}

let field_attributes ~key l =
  List.fold_left (fun fa a ->
      match a.attr_name.txt with
      (* field only *)
      | "meth" -> {fa with fa_meth = true; fa_prop = None}
      | "mutable" -> {fa with fa_prop = Some "prop"}
      | "readonly" -> {fa with fa_prop = Some "readonly_prop"}
      | "writeonly" -> {fa with fa_prop = Some "writeonly_prop"}
      | "key" ->
        let fa_key = match get_string_attr a.attr_payload with
          | None -> key | Some s -> s in
        {fa with fa_key}
      | "case" -> {fa with fa_case = true}
      (* field or core *)
      | "callback" -> {fa with fa_cb = Some true}
      | "meth_callback" -> {fa with fa_meth_cb = Some true}
      | "opt" -> {fa with fa_opt = Some "opt"}
      | "aopt" -> {fa with fa_opt = Some "aopt"}
      | "optdef" -> {fa with fa_opt = Some "optdef"}
      | "ignore" -> {fa with fa_ignore = true}
      | "array" -> {fa with fa_array = Some true}
      | "assoc" -> {fa with fa_assoc = Some true}
      | "number" -> {fa with fa_number = Some true}
      | _ -> fa) {
    fa_meth=false; fa_meth_cb=None; fa_cb=None; fa_prop=Some "readonly_prop";
    fa_opt=None; fa_key=key; fa_ignore=false; fa_array=None; fa_assoc=None;
    fa_number=None; fa_case=false } l

type core_attributes = {
  ca_opt : string option;
  ca_ignore : bool;
  ca_cb : bool option;
  ca_meth_cb : bool option;
  ca_array : bool option;
  ca_assoc : bool option;
  ca_number : bool option;
  ca_case : bool;
}

let core_attributes ?opt ?number ?array_tup ?callback ?meth_callback ?assoc l =
  List.fold_left (fun ca a ->
      match a.attr_name.txt with
      | "opt" -> {ca with ca_opt = Some "opt"}
      | "optdef" -> {ca with ca_opt = Some "optdef"}
      | "aopt" -> {ca with ca_opt = Some "aopt"}
      | "ignore" -> {ca with ca_ignore = true}
      | "callback" -> {ca with ca_cb = Some true}
      | "meth_callback" -> {ca with ca_meth_cb = Some true}
      | "array" -> {ca with ca_array = Some true}
      | "assoc" -> {ca with ca_assoc = Some true}
      | "number" -> {ca with ca_number = Some true}
      | "case" -> {ca with ca_case = true}
      | _ -> ca) {
    ca_opt=opt; ca_ignore=false; ca_cb=callback; ca_meth_cb=meth_callback;
    ca_array=array_tup; ca_assoc=assoc; ca_number=number; ca_case=false } l

let field_name ?(case=false) name =
  let name = if not case && String.contains name '_' then name ^ "_" else name in
  let code = Char.code @@ String.get name 0 in
  if code = 95 || (code >= 97 && code <= 122) then name
  else "_" ^ name

let prop_kind s = match s with
  | Some "writeonly_prop" -> `Writeonly
  | Some "prop" -> `Readwrite
  | Some "case_prop" -> `Case
  | _ -> `Readonly

let get_new_name ?(suffix="_tmp") =
  let tmp_names = ref ([] : (string * int) list) in fun name ->
  let base = "_" ^ name ^ suffix in
  match List.assoc_opt name !tmp_names with
  | None -> tmp_names := (name, 0) :: !tmp_names; base
  | Some i ->  tmp_names := (name, i+1) :: (List.remove_assoc name !tmp_names);
    base ^ string_of_int (i+1)

let get_tuple_name name = get_new_name ~suffix:"_tup" name
let get_variant_name name = get_new_name ~suffix:"_var" name

let str_of_expr e = Pprintast.string_of_expression e
let str_of_structure e = Pprintast.string_of_structure e

let str_of_pat e =
  Pprintast.pattern Format.str_formatter e;
  Format.flush_str_formatter ()

let str_of_ct e =
  Pprintast.class_type Format.str_formatter e;
  Format.flush_str_formatter ()

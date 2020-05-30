open Base
open Obj

type tag = 
  | Abstract
  | Closure
  | Custom
  | Double
  | Double_array
  | Forward
  | Infix
  | Int (* this is a virtual one *)
  | Lazy
  | No_scan
  | Object
  | Out_of_heap
  | String
  | Unaligned
  | Unknown of int 

let parse_tag t = 
  if t = int_tag              then Int
  else if t = double_tag      then Double
  else if t = double_array_tag then Double_array
  else if t = string_tag      then String
  else if t = lazy_tag        then Lazy
  else if t = closure_tag     then Closure
  else if t = object_tag      then Object
  else if t = infix_tag       then Infix
  else if t = forward_tag     then Forward
  else if t = no_scan_tag     then No_scan
  else if t = abstract_tag    then Abstract
  else if t = custom_tag      then Custom
(*
  else if t = final_tag       then Final
*)
  else if t = out_of_heap_tag then Out_of_heap
  else if t = unaligned_tag   then Unaligned
  else Unknown t

let tag_name = function
  | Int ->         "int"
  | Double ->      "double"
  | Double_array -> "double_array"
  | String ->      "string"
  | Lazy ->        "lazy"
  | Closure ->     "closure"
  | Object ->      "object"
  | Infix ->       "infix"
  | Forward ->     "forward"
  | No_scan ->     "no_scan"
  | Abstract ->    "abstract"
  | Custom ->      "custom"
  | Out_of_heap -> "out_of_heap"
  | Unaligned ->   "unaligned"
  | Unknown x ->   string_of_int x 

let dump o = 
  let open Format in
  let visited = ref [] in
  let rec dump ppf o =
    let t = parse_tag & tag o in
    match t with
    | Int -> fprintf ppf "%d" & obj o
    | Double -> fprintf ppf "%.4f" & obj o
    | String -> fprintf ppf "%S" & obj o
    | Out_of_heap -> pp_print_string ppf "<OOH>"
    | _ ->
        if is_block o then begin
          if List.memq o !visited then fprintf ppf "<VISITED>"
          else begin
            visited := o :: !visited;
          (* CR jfuruse: I believe there are some cases not covered by this *)
          (* Double_array for example *)
            fprintf ppf "[%s: @[" & tag_name t;
          let s = size o in
          for i = 0 to s - 1 do
            dump ppf (field o i);
            fprintf ppf "@ "
          done;
          fprintf ppf "@]]"
          end
        end else if is_int o then begin
          fprintf ppf "%d" & obj o
        end else pp_print_string ppf "neither_block_nor_int"
            
  in
  eprintf "%a@." dump o

[%%TEST
  let x_ =        
    let o = object method x = 1 method y = 2 end in
    let oo = Obj.repr o in
    dump oo
]

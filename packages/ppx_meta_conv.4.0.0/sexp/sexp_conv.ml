open Printf
open Sexp
open Meta_conv.Open

(* encoders ***************************************************************)

let errorf fmt =
  Printf.ksprintf (fun s -> failwith (Printf.sprintf "Sexp_conv: %s" s)) fmt

include Meta_conv.Coder.Make(struct 
    
  type target = Sexp.t

  let format = Sexp.format

  module Constr = struct
    let tuple ts       = List ts
    let variant _tyname tag = function
      | [] -> Atom tag
      | ts -> List (Atom tag :: ts)
    let record _tyname tag_ts  = 
      List (List.map (fun (tag,t) -> List [Atom tag; t]) tag_ts)
    let poly_variant = variant
    let object_ = record
  end
  
  module Deconstr = struct
    let tuple = function 
      | List ts -> ts
      | _ -> errorf "List expected for tuple"
  
    let variant _tyname = function 
      | Atom tag -> tag, [] 
      | List (Atom tag :: ts) -> tag, ts
      | _ -> errorf "invalid for variant"
  
    let record _tyname = function
      | List ts ->
          List.map (function
            | List [Atom l; x] -> (l, x)
            | _ -> errorf "Invalid list for record") ts
      | Atom _ -> errorf "List expected for record"
  
    let poly_variant = variant
    let object_ = record
  end
end)

module Default = struct
  let sexp_of_int n       = Atom (string_of_int n)
  let sexp_of_int32 n     = Atom (Int32.to_string n)
  let sexp_of_int64 n     = Atom (Int64.to_string n)
  let sexp_of_nativeint n = Atom (Nativeint.to_string n)
  let sexp_of_char c      = Atom (String.make 1 c)
  let sexp_of_string s    = Atom s
  let sexp_of_float n     = Atom (string_of_float n)
  let sexp_of_list f xs   = List (List.map f xs)
  let sexp_of_array f xs  = List (List.map f (Array.to_list xs))
  let sexp_of_bool b      = Atom (if b then "true" else "false")
  let sexp_of_lazy_t f v  = f (Lazy.force v)
  let sexp_of_unit ()     = List []
  let sexp_of_option f = function
    | None -> Atom "none"
    | Some v -> List [Atom "some"; f v]
  
  (* decoders ***************************************************************)
  
  let errorff fmt = kprintf (fun s -> raise (Failure s)) fmt
  
  let string_of_sexp = Helper.of_deconstr (function
    | Atom s -> s
    | _ -> errorf "string_of_sexp: Atom expected")
  
  let char_of_sexp = Helper.of_deconstr (function
    | Atom s when String.length s = 1 -> s.[0]
    | _ -> errorf "char_of_sexp: a char expected")
  
  let int_check name conv = Helper.of_deconstr (function 
    | Atom s -> begin try conv s with Failure _ -> errorff "%s_of_sexp: overflow" name end
    | _ -> errorff "%s_of_sexp: Atom expected" name)
  
  let int_of_sexp = int_check "int" int_of_string
  
  let int64_of_sexp = int_check "int64" Int64.of_string
        
  let int32_of_sexp = int_check "int32" Int32.of_string
        
  let nativeint_of_sexp = int_check "nativeint" Nativeint.of_string
        
  let float_of_sexp = Helper.of_deconstr (function
    | Atom s -> float_of_string s (* CR jfuruse: Maybe wrong *)
    | _ -> errorf "float_of_sexp: Atom expected")
  
  let bool_of_sexp = Helper.of_deconstr (function
    | Atom "true" -> true
    | Atom "false" -> false
    | _ -> errorf "bool_of_sexp: true/false expected")
  
  let unit_of_sexp = Helper.of_deconstr (function
    | List [] -> ()
    | _ -> errorf "unit_of_sexp: () expected")
    
  let option_of_sexp f = Helper.option_of (function
    | Atom "none" -> Some None
    | List [Atom "some"; v] -> Some (Some v)
    | _ -> None) f
  
  let list_of_sexp f = 
    Helper.list_of (function List xs -> Some xs | _ -> None) f
  
  let array_of_sexp f = 
    Helper.array_of (function List xs -> Some xs | _ -> None) f
  
  let lazy_t_of_sexp d = Helper.lazy_t_of (fun e -> raise (Exception e)) d
  let mc_lazy_t_of_sexp (d : 'a decoder) = (Helper.mc_lazy_t_of d : ('a, Sexp.t) mc_lazy_t decoder)
  
  let sexp_of_mc_fields enc xs = 
    List (List.map (fun (name, a) -> List [Atom name; enc a]) xs)
  let mc_fields_of_sexp dec = Helper.mc_fields_of (function 
    | List ts -> 
        begin try
          Some (List.map (function 
            | List [Atom l; x] -> (l, x)
            | _ -> raise Exit) ts)
        with
        | Exit -> None
        end
    | _ -> None) dec
end

include Default

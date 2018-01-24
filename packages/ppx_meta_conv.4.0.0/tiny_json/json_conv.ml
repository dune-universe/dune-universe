open Printf
open Tiny_json
open Json
open Meta_conv.Open

(* encoders ***************************************************************)

let errorf fmt =
  Printf.ksprintf (fun s -> failwith (Printf.sprintf "Json_conv: %s" s)) fmt

include Meta_conv.Coder.Make(struct 
    
  type target = Json.t

  let format = Json.format

  module Constr = struct
    let tuple ts       = Array ts
    let variant _tyname tag = function
      | [] -> String tag
      | ts -> Object [tag, Array ts]
    let record _tyname tag_ts  = Object tag_ts
    let poly_variant = variant
    let object_ = record
  end
  
  module Deconstr = struct
    let tuple = function 
      | Array ts -> ts
      | _ -> errorf "Array expected for tuple"
  
    let variant _tyname = function 
      | String tag -> tag, [] 
      | Object [tag, Array ts] -> tag, ts
      | _ -> errorf "String or Object expected for variant"
  
    let record _tyname = function
      | Object alist -> alist
      | _ -> errorf "Object expected for record"
  
    let poly_variant = variant
    let object_ = record
  end
end)

module Default = struct
  let json_of_int n       = Number (string_of_int n)
  let json_of_int32 n     = Number (Int32.to_string n)
  let json_of_int64 n     = Number (Int64.to_string n)
  let json_of_nativeint n = Number (Nativeint.to_string n)
  let json_of_char c      = String (String.make 1 c)
  let json_of_string s    = String s
  let json_of_float n     = Number (string_of_float n)
  let json_of_list f xs   = Array (List.map f xs)
  let json_of_array f xs  = Array (List.map f (Array.to_list xs))
  let json_of_bool b      = Bool b
  let json_of_lazy_t f v  = f (Lazy.force v)
  let json_of_unit ()     = Null
  (* jfuruse: This 'a option^n maps to 'a option *)
  let json_of_option f    = function
    | None -> Null
    | Some v -> f v
  
  (* decoders ***************************************************************)
  
  let errorff fmt = kprintf (fun s -> raise (Failure s)) fmt
  
  let string_of_json = Helper.of_deconstr (function
    | String s -> s
    | _ -> errorf "string_of_json: String expected")
  
  let char_of_json = Helper.of_deconstr (function
    | String s when String.length s = 1 -> s.[0]
    | _ -> errorf "char_of_json: a char expected")
  
  let int_check name conv = Helper.of_deconstr (function 
    | Number s -> begin try conv s with Failure _ -> errorff "%s_of_json: overflow" name end
    | _ -> errorff "%s_of_json: Number expected" name)
  
  let int_of_json = int_check "int" int_of_string
  
  let int64_of_json = int_check "int64" Int64.of_string
        
  let int32_of_json = int_check "int32" Int32.of_string
        
  let nativeint_of_json = int_check "nativeint" Nativeint.of_string
        
  let float_of_json = Helper.of_deconstr (function
    | Number s -> float_of_string s (* CR jfuruse: Maybe wrong *)
    | _ -> errorf "float_of_json: Number expected")
  
  let bool_of_json = Helper.of_deconstr (function
    | Bool b -> b
    | _ -> errorf "bool_of_json: Bool expected")
  
  let unit_of_json = Helper.of_deconstr (function
    | Null -> ()
    | _ -> errorf "unit_of_json: Null expected")
    
  let list_of_json f = 
    Helper.list_of (function Array xs -> Some xs | _ -> None) f
  
  let array_of_json f = 
    Helper.array_of (function Array xs -> Some xs | _ -> None) f
  
  (* jfuruse: 'a option^n mapped to 'a option *)
  let option_of_json f = Helper.option_of 
    (function Null -> Some None | v -> Some (Some v))
    f
  
  let lazy_t_of_json d = Helper.lazy_t_of (fun e -> raise (Exception e)) d
  let mc_lazy_t_of_json (d : 'a decoder) = (Helper.mc_lazy_t_of d : ('a, Json.t) mc_lazy_t decoder)
  
  let json_of_mc_fields enc xs = Object (List.map (fun (name, a) -> (name, enc a)) xs)
  let mc_fields_of_json dec = Helper.mc_fields_of (function Object js -> Some js | _ -> None) dec
end

include Default


open Meta_conv.Open
open Meta_conv.Types
open Camlon
open Camlon.Ocaml

include Meta_conv.Coder.Make(struct
  type target = Ocaml.t
  let format fmt = Ocaml.format fmt

  module Constr = struct
    let tuple ts = Tuple ts
    let variant _tyname tag ts = Variant (tag, ts)
    let poly_variant _tyname tag ts = Poly_variant (tag, ts)
    let record _tyname fields = Record fields
    let object_ _tyname fields = Object fields
  end

  module Deconstr = struct
    let tuple = function 
      | Tuple ts -> ts
      | _ -> failwith "Tuple expected for tuple"

    (* Variants and poly variants, and records and objects 
       are compatible each other, resp.ly. *)
    let variant _tyname = function 
      | Variant (tag, ts) -> tag, ts
      | Poly_variant (tag, ts) -> tag, ts
      | _ -> failwith "Variant expected for variant"

    let poly_variant _tyname = function 
      | Variant (tag, []) -> tag, []
      | Poly_variant (tag, []) -> tag, []
      | Variant (tag, [t]) -> tag, [t]
      | Poly_variant (tag, [t]) -> tag, [t]
      | Variant (tag, ts) -> tag, [Tuple ts]
      | Poly_variant (tag, ts) -> tag, [Tuple ts]
      | _ -> failwith "Poly_variant expected for poly_variant"

    let record _tyname = function
      | Record alist -> alist
      | Object alist -> alist
      | _ -> failwith "Record expected for record"

    let object_ _tyname = function 
      | Record alist -> alist
      | Object alist -> alist
      | _ -> failwith "Object expected for object"
  end
end)

module Default = struct
  let ocaml_of_unit () = Unit
  let ocaml_of_bool b = Bool b
  let ocaml_of_int32 n = Int32 n
  let ocaml_of_int64 n = Int64 n
  let ocaml_of_float n = Float n
  let ocaml_of_char c = Char c
  let ocaml_of_string s = String s
  let ocaml_of_list f xs = List (List.map f xs)
  let ocaml_of_array f xs = Array (List.map f (Array.to_list xs))
  let ocaml_of_option f = function
    | None -> Variant ("None", [])
    | Some v -> Variant ("Some", [f v])
  let ocaml_of_ref f = function
    | { contents=v } -> Record [ "contents", f v ]
  let ocaml_of_lazy_t f z = f (Lazy.force z)
  
  let ocaml_of_mc_lazy_t (f : 'a encoder) (v : ('a, 'target) mc_lazy_t) = match Lazy.force v with
    | Ok v -> f v
    | Error e -> raise (Exception e)
  
  let ocaml_of_mc_fields f fields = Record ( List.map (fun (k,v) -> k, f v) fields )
  
  let unit_of_ocaml = Helper.of_deconstr (function
    | Unit -> ()
    | _ -> failwith "unit_of trace: Unit expected")
    
  let bool_of_ocaml = Helper.of_deconstr (function
    | Bool b -> b
    | v -> Printf.ksprintf failwith "bool_of_ocaml trace: Bool expected (%s)" 
  	Obj.(let o = repr v in
  	     if is_int o then Printf.sprintf "int=%d" (Obj.obj o)
  	     else Printf.sprintf "tag=%d" (tag o)))
  
  let string_of_ocaml = Helper.of_deconstr (function
    | String s -> s
    | _ -> failwith "string_of trace: String expected")
  
  let char_of_ocaml = Helper.of_deconstr (function
    | Char c -> c
    | _ -> failwith "char_of trace: a char expected")
  
  let int32_of_ocaml = Helper.of_deconstr (function
    | Int32 n -> n
    | _ -> failwith "int32_of trace: int32 expected")
  
  let int64_of_ocaml = Helper.of_deconstr (function
    | Int64 n -> n
    | _ -> failwith "int64_of trace: int64 expected")
  
  let float_of_ocaml = Helper.of_deconstr (function
    | Float n -> n
    | _ -> failwith "float_of trace: float expected")
  
  let list_of_ocaml f = 
    (* It must accept both [ a; b; c ] and a :: b :: c :: [] *)
    let rec get_list revs = function
      | List xs -> Some (List.rev_append revs xs)
      | Variant ("[]", []) -> Some (List.rev revs)
      | Variant ("::", [x; xs]) -> get_list (x::revs) xs
      | _ -> None
    in
    Helper.list_of (get_list []) f
  
  let array_of_ocaml f = Helper.array_of (function Array xs -> Some xs | _ -> None) f
  
  let option_of_ocaml f = Helper.option_of (function
    | Variant ("None", []) -> Some None 
    | Variant ("Some", [v]) -> Some (Some v)
    | Variant ("Some", vs) -> Some (Some (Tuple vs))
    | _ -> None) f
  
  let ref_of_ocaml f = Helper.ref_of (function
    | Record [ "contents", v ] -> Some v
    | _ -> None) f
  
  let lazy_t_of_ocaml (d : ('a, Ocaml.t) Decoder.t) : ('a lazy_t, Ocaml.t) Decoder.t = 
    Helper.lazy_t_of (fun (e : Ocaml.t Meta_conv.Error.t) -> raise (Exception e)) d
  
  
  (** Arch dependent enc/decoders *)
    
  module type SArch = sig
    val ocaml_of_int       : (int, Ocaml.t) Encoder.t
    val ocaml_of_nativeint : (nativeint, Ocaml.t) Encoder.t
    val int_of_ocaml       : (int, Ocaml.t) Decoder.t
    val nativeint_of_ocaml : (nativeint, Ocaml.t) Decoder.t
  end
    
  module Arch32 = struct
    let ocaml_of_int n = Int31 n
    let ocaml_of_nativeint n = Nativeint32 (Nativeint.to_int32 n)
  
    let int_of_ocaml = Helper.of_deconstr (function
      | Int31 n -> n
      | Int63 n ->
          let n' = Int64.to_int n in
          if n <> Int64.of_int n' then failwith "int_of_ocaml: overflow"
          else n'
      | _ -> failwith "int_of_ocaml: int expected")
  
    let nativeint_of_ocaml = Helper.of_deconstr (function
      | Nativeint32 n -> Nativeint.of_int32 n
      | Nativeint64 n ->
          let n' = Int64.to_nativeint n in
          if n <> Int64.of_nativeint n' then failwith "nativeint_of_ocaml: overflow"
          else n'
      | _ -> failwith "int_of_ocaml: nativeint expected")
  end
  
  module Arch64 = struct
    let ocaml_of_int n = Int63 (Int64.of_int n)
    let ocaml_of_nativeint n = Nativeint64 (Int64.of_nativeint n)
  
    let int_of_ocaml = Helper.of_deconstr (function
      | Int31 n -> n
      | Int63 n -> (Int64.to_int n)
      | _ -> failwith "int_of_ocaml: int expected")
  
    let nativeint_of_ocaml = Helper.of_deconstr (function
      | Nativeint32 n -> Nativeint.of_int32 n
      | Nativeint64 n -> Int64.to_nativeint n
      | _ -> failwith "int_of_ocaml: nativeint expected")
  
  end
  
  let arch = 
    match Sys.word_size with
    | 32 -> (module Arch32 : SArch)
    | 64 -> (module Arch64 : SArch)
    | _ -> assert false
  
  include (val arch)
  
  let ocaml_of_hashtbl x = Helper.of_hashtbl ocaml_of_list x
  
  let hashtbl_of_ocaml x = Helper.hashtbl_of list_of_ocaml x
end

include Default


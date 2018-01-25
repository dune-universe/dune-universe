open Spotlib.Spot
open Typerep_lib.Std
open Typerep

type obj = Obj.t [@@deriving typerep ~abstract ]
let typename_of_obj = Typename.create ~name:"Obj.t" ()
let typerep_of_obj =
  let module M = struct
    type named = obj (* ? *)
    type t = obj
    let typename_of_named = typename_of_obj
    let typename_of_t = typename_of_obj
    let witness = Type_equal.T
  end in
  let named = Named.T0 (module M) in
  Named (named, None)
    
module O = Obj

(** [fields o] get the fields of a block [o]. 
    Note that there is no block check of [o]: applying this function to 
    non blocks is unsafe.
*)
let fields o =
  let sz = O.size o in
  let rec f st = function
    | -1 -> st
    | i -> f (O.field o i :: st) (i-1)
  in
  f [] (sz-1)

let typename tyrep = Typename.name & Typerep.typename_of_t tyrep

exception Ill_typed of Typerep.packed * Obj.t * string
exception Not_supported of Typerep.packed * Obj.t * string

let failf tyrep o fmt =
  Printf.ksprintf (fun mes -> raise (Ill_typed (Typerep.T tyrep, o, mes))) fmt

let not_supportedf tyrep o fmt =
  Printf.ksprintf (fun mes -> raise (Not_supported (Typerep.T tyrep, o, mes))) fmt

let range_out_int tr o i =
  failf tr o "Unexpected int %d found for type %s" i (typename tr)

let range_out_tag tr o tg =
  failf tr o "Unexpected block tag %d found for variant type %s" tg (typename tr)

let illegal_tag tr o tg =
  failf tr o "Unexpected block tag %d found for type %s" tg (typename tr)

let size_mismatch tr o =
  failf tr o "Unexpected block size %d found for type %s" (O.size o) (typename tr)

let function_type tr o = not_supportedf tr o "Function type %s is not supported" (typename tr)

let named_without_manifest tr o = not_supportedf tr o "Manifestless named type %s is not supported" (typename tr)


type hash = int

type cache = (hash, (Obj.t * Typerep.packed list ref) list ref) Hashtbl.t
(** Cache for sharing/cycle detection *)

(** Block size of int64.

   sz = 1 + (4 + size_of(value) - 1) / sizeof(value)
      64bit arch: 1 + (8 + 8 - 1) / 8 = 2
      32bit arch: 1 + (8 + 4 - 1) / 4 = 3

  The block sizes of the other integer types (int32, natint) are all
  constants and independent from archtecture bit sizes.
*)
let sz_of_int64 = match Sys.word_size with
  | 32 -> 3
  | 64 -> 2
  | n -> failwithf "Unexpected Sys.word_size = %d" n
    
(* Unfortunately, sharing+cycle detection mode is VERY slow. 
   We should pre-scan the type and list the types which can be recursive,
   and record objects only of these recursive types.
*)  
let rec tag_check : type a . cache option -> a Typerep.t -> Obj.t -> unit = fun cache tr o -> 
  if O.is_int o then tag_check_non_block tr o
  else tag_check_block cache tr o

and tag_check_non_block : type a . a Typerep.t -> Obj.t -> unit = fun tr o -> 
  let int : int = O.obj o in
  let unexpected () = failf tr o "Unexpected int %d found for a block of type %s" int (typename tr) in
  match tr with
  | Int -> 
      (* input_value fails in 32bit arch when it tries to read int63,
         therefore we need not to care about the size here.
      *)
      ()

  | Char when int >= 0 && int < 256 -> ()
  | Char -> range_out_int tr o int

  | Bool when int = 0 || int = 1 -> ()
  | Bool -> range_out_int tr o int

  | Unit -> ()

  | Variant (v : a Variant.t) when not & Variant.is_polymorphic v ->
      if int < 0 then range_out_int tr o int;
      let num_cstrs = Variant.length v in
      let rec f ctag i =
        if i = num_cstrs then range_out_int tr o int
        else
          match Variant.tag v i with
          | Variant.Tag t when Tag.arity t = 0 ->
              if int = ctag then () (* found *) else  f (ctag+1) (i+1)
          | _ -> f ctag (i+1)
      in
      f 0 0

  | Variant (v : a Variant.t) ->
      let num_cstrs = Variant.length v in
      let rec f i =
        if i = num_cstrs then failf tr o "Unexpected int %d for polymorphic variant type %s" int (typename tr)
        else
          match Variant.tag v i with
          | Variant.Tag t when Tag.arity t = 0 && int = Typerep_obj.hash_variant (Tag.label t) -> () (* found *)
          | _ -> f (i+1)
      in
      f 0

  | Option _ when int = 0 -> () (* None *)
  | Option _ -> range_out_int tr o int

  | List _ when int = 0 -> () (* [] *)
  | List _ -> range_out_int tr o int

  | Named _ when tr == Obj.magic typerep_of_obj -> () (* Obj.t *)
  | Named (_, Some z) -> tag_check_non_block (Lazy.force z) o (* manifest *)
  | Named _ -> named_without_manifest tr o

  | Lazy tr' -> tag_check_non_block tr' o (* Lazy.from_val <int> *)

  | Function _ -> function_type tr o

  | Int32     -> unexpected ()
  | Int64     -> unexpected ()
  | Nativeint -> unexpected ()
  | Float     -> unexpected ()
  | String    -> unexpected ()
  | Bytes     -> unexpected ()
  | Array _   -> unexpected ()
  | Ref _     -> unexpected ()
  | Tuple _   -> unexpected ()
  | Record _  -> unexpected ()

and tag_check_block : type a . ?skip_cache: bool -> cache option -> a Typerep.t -> Obj.t -> unit = fun ?(skip_cache=false) cache tr o ->
  (* quickly determin worth cache checking *)
  let worth_caching =
    cache <> None
    && not skip_cache
    (* CR jfuruse: not good idea for -rectypes? *)
    && match tr with (* We should have a quick check is the type is recursive or not *)
       | Record r  -> not & Record.has_double_array_tag r (* it cannot have cycles *)
       | String    -> false
       | Int32     -> false
       | Int64     -> false
       | Nativeint -> false
       | Float     -> false
       | Option _  -> false
       | Array _   -> false
       | Tuple _   -> false
       | Named _   -> false (* manifest types are cached as their expanded form *)
       | _ -> true
  in
  
  (* Visited this [o] already with the same expected type.
     The same [o] can be visited as different types because of polymorphism:
     let x = [[]] in (x : int list list, x : float list list)
  *)
  let visited =
    worth_caching
    && match cache with
      | None -> assert false
      | Some cache -> 
          let h = Hashtbl.hash o in
          let packed_tr = Typerep.T tr in
          match Hashtbl.find cache h with
          | exception Not_found -> 
              Hashtbl.replace cache h (ref [o, ref [packed_tr]]); false
          | slot ->
              match List.assq o !slot with
              | exception Not_found -> 
                  slot := (o, ref [packed_tr]) :: !slot; false
              | packs when List.exists (fun ptr -> Typerep.same (Obj.magic packed_tr) (Obj.magic ptr)) !packs -> true (* Ouch we have no Typerep.same for packed *)
              | packs -> packs := packed_tr :: !packs; false
  in
  if visited then ()
  else
  let tg = O.tag o in
  match tr with
  | Variant (v : a Variant.t) when not & Variant.is_polymorphic v -> 
      let sz = O.size o in
      if tg > O.last_non_constant_constructor_tag then range_out_tag tr o tg
      else
        let num_cstrs = Variant.length v in
        let rec find_tag ctag i =
          if i = num_cstrs then range_out_tag tr o tg
          else
            let Variant.Tag t = Variant.tag v i in
            let arity = Tag.arity t in
            if arity = 0 then find_tag ctag (i+1)
              else if tg <> ctag then find_tag (ctag+1) (i+1)
              else i
          in
        let i = find_tag 0 0 in
        let Variant.Tag t = Variant.tag v i in
        let arity = Tag.arity t in
        if sz <> arity then failf tr o "Unexpected block size %d found for constructor %s of type %s whose arity is %d" sz (Tag.label t) (typename tr) arity 
        else if sz = 1 then tag_check cache (Tag.traverse t) (O.field o 0)
        else
          (* This is not to be cached *)
          tag_check_block ~skip_cache:true cache (Tag.traverse t) o

  | Variant (v : a Variant.t) ->
      let sz = O.size o in
      if sz = 1 then failf tr o "Unexpected block size %d found for polymorphic variant type %s" sz (typename tr);
      let num_cstrs = Variant.length v in
      let rec f i =
        if i = num_cstrs then failf tr o "Unexpected block tag %d for polymorphic variant type %s" tg (typename tr)
        else
          let otg = O.field o 0 in
          if O.is_block otg then failf tr o "Unexpected block for polymoprhic variant tag of type %s" (typename tr);
          let tg = (O.obj otg : int) in
          match Variant.tag v i with
          | Variant.Tag t when Tag.arity t > 0 && tg = Typerep_obj.hash_variant (Tag.label t) ->
              if sz <> 2 then failf tr o "Unexpected block size %d found for polymorphic variant constructor `%s of type %s"
                sz (Tag.label t) (typename tr);
              let o' = O.field o 1 in
              tag_check cache (Tag.traverse t) o'
          | _ -> f (i+1)
      in
      f 0

  | Record r when Record.has_double_array_tag r && tg = O.double_array_tag ->
      let sz = O.size o in
      let rsz = Record.length r in
      if rsz <> sz then failf tr o "Unexpected block size %d found for float record type %s whose size is %d" sz (typename tr) rsz 
  | Record r when Record.has_double_array_tag r ->
      failf tr o "Unexpected block tag %d found for float record type %s" tg (typename tr)
  | Record _r when tg = O.double_array_tag ->
      failf tr o "Unexpected double array tag found for non float record type %s" (typename tr)
  | Record r ->
      let sz = O.size o in
      let rsz = Record.length r in
      if rsz <> sz then failf tr o "Unexpected block size %d found for record type %s whose size is %d" sz (typename tr) rsz 
      else
        for i = 0 to sz - 1 do
          let o' = O.field o i in
          let Record.Field f = Record.field r i in
          tag_check cache (Field.traverse f) o'
        done

  | String when tg = O.string_tag -> ()
  | String -> illegal_tag tr o tg

  | Bytes when tg = O.string_tag -> ()
  | Bytes -> illegal_tag tr o tg

  | Int32 when tg = O.custom_tag -> 
      (* sz = 1 + (4 + size_of(value) - 1) / sizeof(value)
         64bit arch: 1 + (4 + 8 - 1) / 8 = 2
         32bit arch: 1 + (4 + 4 - 1) / 4 = 2
         (x*8)bit arch: 1 + (4 + x - 1) / x = 2
      *)
      let sz = O.size o in
      if sz = 2 then () else size_mismatch tr o
  | Int32 -> illegal_tag tr o tg

  | Int64 when tg = O.custom_tag ->
      (* sz = 1 + (8 + size_of(value) - 1) / sizeof(value)
         64bit arch: 1 + (8 + 8 - 1) / 8 = 2
         32bit arch: 1 + (8 + 4 - 1) / 4 = 3
      *)
      let sz = O.size o in
      if sz = sz_of_int64 then () else size_mismatch tr o
  | Int64 -> illegal_tag tr o tg
      
  | Nativeint when tg = O.custom_tag ->
      (* sz = 1 + (size_of_(intnat) + size_of(value) - 1) / sizeof(value)
         64bit arch: 1 + (8 + 8 - 1) / 8 = 2
         32bit arch: 1 + (4 + 4 - 1) / 4 = 2
      *)
      let sz = O.size o in
      if sz = 2 then () else size_mismatch tr o
  | Nativeint -> illegal_tag tr o tg

  | Float when tg = O.double_tag -> ()
  | Float -> illegal_tag tr o tg

  | Bool  -> illegal_tag tr o tg
  | Int   -> illegal_tag tr o tg
  | Char  -> illegal_tag tr o tg
  | Unit  -> illegal_tag tr o tg

  | Option tr when tg = 0 && O.size o = 1 -> tag_check cache tr (O.field o 0)
  | Option _ -> illegal_tag tr o tg

  | List tr' when tg = 0 && O.size o = 2 -> 
      tag_check cache tr' (O.field o 0);
      tag_check cache tr  (O.field o 1)
  | List _ -> illegal_tag tr o tg

  | Array Float when tg = O.double_array_tag -> ()
  | Array _ when tg = O.double_array_tag ->
      failf tr o "Unexpected block tag %d found for float array type %s"
        tg (typename tr) 
  | Array tr -> List.iter (tag_check cache tr) & fields o

  | Tuple (Tuple.T2 (tr1, tr2)) when O.size o = 2 ->
      let o1 = O.field o 0
      and o2 = O.field o 1
      in
      tag_check cache tr1 o1; tag_check cache tr2 o2
  | Tuple (Tuple.T3 (tr1, tr2, tr3)) when O.size o = 3 ->
      let o1 = O.field o 0
      and o2 = O.field o 1
      and o3 = O.field o 2
      in
      tag_check cache tr1 o1; tag_check cache tr2 o2; tag_check cache tr3 o3
  | Tuple (Tuple.T4 (tr1, tr2, tr3, tr4)) when O.size o = 4 -> 
      let o1 = O.field o 0
      and o2 = O.field o 1
      and o3 = O.field o 2
      and o4 = O.field o 3
      in
      tag_check cache tr1 o1; tag_check cache tr2 o2; tag_check cache tr3 o3; tag_check cache tr4 o4
  | Tuple (Tuple.T5 (tr1, tr2, tr3, tr4, tr5)) when O.size o = 5 ->
      let o1 = O.field o 0
      and o2 = O.field o 1
      and o3 = O.field o 2
      and o4 = O.field o 3
      and o5 = O.field o 4
      in
      tag_check cache tr1 o1; tag_check cache tr2 o2; tag_check cache tr3 o3; tag_check cache tr4 o4; tag_check cache tr5 o5
  | Tuple (Tuple.T2 _) -> size_mismatch tr o
  | Tuple (Tuple.T3 _) -> size_mismatch tr o
  | Tuple (Tuple.T4 _) -> size_mismatch tr o
  | Tuple (Tuple.T5 _) -> size_mismatch tr o

  | Lazy _ when tg = O.lazy_tag -> not_supportedf tr o "lazy closure is not supported"
  | Lazy tr' when tg = O.forward_tag ->
      let o' = O.field o 0 in
      if O.is_block o' then
        let tg' = O.tag o' in
        if tg' = O.forward_tag || tg' = O.lazy_tag || tg' = O.double_tag then
          tag_check_block cache tr' o'
        else
          failf tr o "Unexpected block tag %d found for a forwarded block of type %s" tg' (typename tr)
      else
        failf tr o "Unexpected int %d found for a forwarded block of type %s" (O.obj o' : int) (typename tr)
  | Lazy tr' ->
      if tg = O.double_tag then 
        failf tr o "Unexpected block tag %d found for a forced lazy value of type %s" (O.double_tag) (typename tr);
      tag_check_block cache tr' o

  | Ref tr' ->
      let sz = O.size o in
      if sz = 1 then tag_check cache tr' (O.field o 0)
      else size_mismatch tr o 

  | Named _ when tr == Obj.magic typerep_of_obj -> () (* Obj.t *)
  | Named (_, Some z) -> tag_check_block cache (Lazy.force z) o
  | Named _ -> named_without_manifest tr o

  | Function _ -> function_type tr o

let tag_check : type a . sharing:bool -> a Typerep.t -> Obj.t -> unit = fun ~sharing:b tr o ->
  tag_check (if b then Some (Hashtbl.create 107) else None) tr o

let obj ~sharing tyrep o =
  tag_check ~sharing tyrep o;
  Obj.obj o

(* Time-stamp: <modified the 11/03/2020 (at 16:51) by Erwan Jahier> *)

open Data

module OrderedData = struct
  type t = Data.t
  let compare = compare
end

module DataSet = Set.Make(OrderedData)

let rec (update_set : DataSet.t -> Data.t -> DataSet.t) =
fun acc t -> 
  match t with 
    | Data.Array(st,_) -> update_set (DataSet.add t acc) st
    | Data.Extern _ -> DataSet.add t acc 
    | Struct(_,fl)  -> List.fold_left (fun acc (_,t) -> update_set acc t) acc fl
    | Alias(_n,t) -> update_set acc t
    | _ -> acc

open Soc

let (update_set_soc_var : DataSet.t -> Soc.var -> DataSet.t) =
  fun acc (_,datat) -> 
    update_set acc datat

let (update_set_step:  DataSet.t -> Soc.step_method -> DataSet.t) =
  fun acc sm -> 
    match sm.impl with
      | Gaol(vl,_) -> List.fold_left update_set_soc_var acc vl
      | _ -> acc


(* exported *)
let (gen_used_types : Soc.t list -> Data.t list) =
  fun socs -> 
    let aux acc soc =
      let ins, outs = soc.profile in
      let acc = List.fold_left update_set_soc_var acc ins in
      let acc = List.fold_left update_set_soc_var acc outs in
      let acc = List.fold_left update_set_step acc soc.step in
      acc
    in
    let acc = List.fold_left aux DataSet.empty socs in
    DataSet.elements acc


(* exported *)
let (f: Data.t -> string) =
  fun t -> 
    let t_str = Soc2cIdent.type_to_short_string t in
    let t_def =
      match t with
      | Bool | Int | Real | Enum _ -> 
                      Printf.sprintf "
#ifndef _assign_%s 
#define _assign_%s(dest, source, size) dest = source 
#endif
" t_str t_str
      | String
      | Extern _ -> 
         Printf.sprintf "
#ifndef _assign_%s 
#define _assign_%s(dest, source, size) dest = source // XXX fixme?  
#endif
" t_str t_str
        
  | Struct _
  | Array  _
  | Alpha _
  | Alias _ -> 
        Printf.sprintf "
#ifndef _assign_%s 
#define _assign_%s(dest, source, size) memcpy(dest, source, size)
#endif
" t_str t_str
    in
    t_def

let (f_forloop: Data.t -> string) =
  fun t ->
  let rec (aux: int -> string -> Data.t -> string) = fun i index t ->
    match t with
    | Array(t, size) ->
       let index = Printf.sprintf "%s[_j%i]" index i in
       let body = aux (i+1) index t in
       Printf.sprintf "{ int _j%i; for (_j%i=0; _j%i<%i; _j%i++) { %s }}"
                      i                     i       i size   i      body
    | _ ->
       Printf.sprintf " dest%s=source%s;" index index
  in
  let t_str = Soc2cIdent.type_to_short_string t in
  let loop_def = aux 0 "" t in
  Printf.sprintf "#ifndef _assign_%s 
#define _assign_%s(dest, source, _dummy) %s 
#endif
                  " t_str t_str loop_def
                 

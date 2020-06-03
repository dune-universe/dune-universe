open Types

(** {6 Misc functions } *)

let list_filter_map f xs = List.fold_right (fun x st -> 
  match f x with
  | None -> st
  | Some v -> v::st) xs []

let list_mapi f xs =
  let rec mapi f pos = function
    | [] -> []
    | x::xs -> f pos x :: mapi f (pos+1) xs
  in
  mapi f 0 xs

let (~?) = function
  | Some trace -> trace
  | None -> []

(** {6 Error decoders } *)

let tuple_arity_error exp_len act_len ?(trace=[]) v = 
  Error (`Wrong_arity (exp_len, act_len, None), v, trace)
let variant_arity_error type_name constructor_name exp_len act_len ?(trace=[]) v = 
  Error (`Wrong_arity (exp_len, act_len, Some (type_name, constructor_name)), v, trace)
let variant_unknown_tag_error type_name tag_name ?(trace=[]) v =
  Error (`Unknown_tag (type_name, tag_name), v, trace)
let primitive_decoding_failure mes ?(trace=[]) v = 
  Error (`Primitive_decoding_failure mes, v, trace)
let sub_decoders_failed_for_one_of name ?(trace=[]) v =
  Error (`Sub_decoders_failed_for_one_of name, v, trace)

(** {6 Tools used by generated code } *)

let field_assoc_exn name key alist 
    (throw : 'target Error.t -> 'host) 
    (decode_exn : ('host, 'target) Decoder.t_exn) 
    : ('host, 'target) Decoder.t_exn
  = fun ?(trace=[]) v ->
    match List.assoc key alist with
    | v' ->
        let trace = `Field key :: `Node v :: trace in
        decode_exn ?trace:(Some trace) v'
    | exception Not_found ->
        throw (`Required_field_not_found (name, key), v, trace)

let field_assoc_optional_exn _name key alist decode_exn ?(trace=[]) v =
  match List.assoc key alist with
  | v' ->
      let trace = `Field key :: `Node v :: trace in
      Some (decode_exn ?trace:(Some trace) v')
  | exception Not_found -> None

let filter_fields type_system actual =
  List.partition (fun (f,_) -> List.mem f type_system) actual 

let embeded_decoding_helper secondary_fields v = function
  | Ok v -> Ok (v, [])
  (* Having this function in Internal is mainly to avoid seeing
     bunch of Warnings 4 due to the next line, if it would be inlined
     in the generalted code. *) 
  | Error (`Unknown_fields (_, keys, o), v', _) when v == v' ->
       (* extract the accepted fields and the decoded value, and rest fields
          - Obj.magic
          - inefficient
       *)
      let secondary_fields = List.filter (fun (k,_) -> List.mem k keys) secondary_fields in
      Ok (Obj.obj o, secondary_fields)
  | Error e -> Error e

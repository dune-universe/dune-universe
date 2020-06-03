open Types
open Error
open Result
open Open
open Internal

module Make(A : Min) = struct
  include A

  exception Exception of target Error.t

  type 'a encoder     = ('a, target) Encoder.t
  type 'a decoder     = ('a, target) Decoder.t
  type 'a decoder_exn = ('a, target) Decoder.t_exn
  
  let decoder_of_deconstr f = fun ?(trace=([] : target Error.trace)) t -> 
    try Ok (f t) with exn -> Error (`Exception exn, t, trace)

  let decoder_exn_of_deconstr f = fun ?(trace=([] : target Error.trace)) t -> 
    try f t with exn -> raise (Exception (`Exception exn, t, trace))

  module DeconstrDecoder = struct

    let tuple ?trace        = decoder_of_deconstr Deconstr.tuple ?trace
    let variant tyname      ?trace = decoder_of_deconstr (Deconstr.variant tyname) ?trace
    let poly_variant tyname ?trace = decoder_of_deconstr (Deconstr.poly_variant tyname) ?trace
    let record tyname    ?trace   = decoder_of_deconstr (Deconstr.record tyname) ?trace
    let object_ tyname    ?trace  = decoder_of_deconstr (Deconstr.object_ tyname) ?trace
      
    let tuple_exn ?trace        = decoder_exn_of_deconstr Deconstr.tuple ?trace
    let variant_exn tyname  ?trace    = decoder_exn_of_deconstr (Deconstr.variant tyname) ?trace
    let poly_variant_exn tyname ?trace = decoder_exn_of_deconstr (Deconstr.poly_variant tyname) ?trace
    let record_exn tyname   ?trace     = decoder_exn_of_deconstr (Deconstr.record tyname) ?trace
    let object_exn   tyname ?trace     = decoder_exn_of_deconstr (Deconstr.object_ tyname) ?trace
      
  end

  let exn f ?trace v = match f ?trace v with
    | Ok v -> v
    | Error e -> raise (Exception e)

  let throw e = raise (Exception e)

  let catch f v = try Ok (f v) with Exception e -> Error e

  let result f ?trace t = 
    try Ok (f ?trace t) with 
    | Exception e -> Error e
    | exn -> Error (`Exception exn, t, ~?trace)

  let from_Ok = function
    | Ok v -> v
    | Error e -> raise (Exception e)

  let format_error ppf (desc,_,_) = Error.format_desc ppf desc
  let format_full_error = Error.format A.format

  let format_with encoder ppf t = format ppf (encoder t)

  module Helper = struct    
    (** {6 Useful tool functions for writing encoders+decoders of primitive types } *)

    (* This is not really target dependent, but included here for easier access *)
    let integer_of_float min max conv n =
      if floor n <> n then Error "not an integer"
      else if min <= n && n <= max then Ok (conv n)
      else Error "overflow"
    
    let list_of (type target) gets (d : (_,target) Decoder.t) ?(trace=[]) v = match gets v with
      | None -> 
          primitive_decoding_failure 
            "Meta_conv.Internal.generic_list_of: listable expected" 
            ~trace
            v
      | Some xs -> 
          let trace = `Node v::trace in 
          let module E = LocalException(struct type t = target end) in
          E.catch begin fun () -> 
            list_mapi (fun pos x -> E.exn (d ~trace:(`Pos pos :: trace)) x) xs
          end () 
    
    let array_of gets d ?trace v =
      fmap Array.of_list (list_of gets d ?trace v)
    
    let option_of extract f ?trace v =
      match extract v with 
      | Some None -> Ok None
      | Some (Some v) -> f ?trace v >>= fun x -> Ok (Some x)
      | None -> 
          primitive_decoding_failure 
            "Meta_conv.Internal.option_of: option expected"
            ?trace v
    
    let ref_of extract f ?trace v =
      match extract v with 
      | Some v -> f ?trace v >>= fun x -> Ok { contents= x }
      | None -> 
          primitive_decoding_failure 
            "Meta_conv.Internal.ref_of: option expected"
            ?trace v
    
    let lazy_t_of (errorf : 'target Error.t -> 'exn) f ?trace:_ v = 
      Ok (lazy (
        (* trace is reset to avoid leak *)
        match f ?trace:None v with
        | Ok v -> v
        | Error e -> errorf e
      ))
    
    let of_mc_lazy_t e = fun v ->
      match Lazy.force v with
      | Ok a -> e a
      | Error (_, a, _) -> a
    
    let mc_lazy_t_of f ?trace:_ v = 
      Ok (lazy (f ?trace:None v)) (* trace is reset, to avoid leak *)
    
    let mc_fields_of get_fields f ?(trace=[]) target =
      let open Result in
      match get_fields target with
      | None -> primitive_decoding_failure "mc_fields expected" ~trace target
      | Some fields ->
          let trace = `Node target :: trace in
          map (fun (name, target) -> f ?trace:(Some (`Field name :: trace)) target >>= fun host -> Ok (name, host)) fields
    
    let of_deconstr f = fun ?(trace=[]) v -> 
      try Ok (f v) with 
      | Failure mes -> Error (`Primitive_decoding_failure mes, v, trace)
  
    (** Hashtbl coders via list *)
  
    let of_hashtbl of_list of_a of_b tbl =
      of_list 
        (fun x -> x)
        (Hashtbl.fold (fun k v st -> Constr.tuple [of_a k; of_b v]::st) tbl [])
  
    let hashtbl_of list_of a_of b_of = fun ?trace v ->
      let ab_of ?(trace=[]) v = 
        DeconstrDecoder.tuple ~trace v >>= function
          | [a; b] -> 
              a_of ?trace:(Some (`Pos 0 :: `Node v :: trace)) a >>= fun a ->
              b_of ?trace:(Some (`Pos 0 :: `Node v :: trace)) b >>= fun b ->
              Ok (a,b)
          | xs ->
              Error (`Wrong_arity (2, List.length xs, None), v, trace)
      in
      list_of ab_of ?trace v >>= fun abs ->
      let tbl = Hashtbl.create 101 in (* CR jfuruse: size fixed *)
      List.iter (fun (k,v) -> Hashtbl.add tbl k v) abs;
      Ok tbl
        
    let of_result embed_ok embed_error of_ok of_error = function
      | Ok e -> embed_ok @@ of_ok e
      | Error e -> embed_error @@ of_error e

    let result_of divider ok_of error_of ?trace v =
      divider ?trace v >>= function
      | (Ok v) -> ok_of ?trace v >>= fun x -> Ok (Ok x)
      | (Error v) -> error_of ?trace v >>= fun x -> Ok (Error x)
  end

end

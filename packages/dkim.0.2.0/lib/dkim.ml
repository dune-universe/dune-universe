module Refl = struct
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Body = Body
module Sigs = Sigs
open Sigs

type (+'a, 'err) or_err = ('a, ([> Rresult.R.msg ] as 'err)) result

type map = Map.t

let ( <.> ) f g x = f (g x)

let src = Logs.Src.create "dkim" ~doc:"logs dkim's event"

module Log = (val Logs.src_log src : Logs.LOG)

let trim unstrctrd =
  let fold acc = function
    | `WSP _ | `FWS _ | `CR | `LF -> acc
    | elt -> elt :: acc in
  Unstrctrd.fold ~f:fold [] unstrctrd
  |> List.rev
  |> Unstrctrd.of_list
  |> Rresult.R.get_ok

let parse_dkim_field_value unstrctrd =
  let str = Unstrctrd.(to_utf_8_string (trim unstrctrd)) in
  match Angstrom.parse_string ~consume:All Decoder.mail_tag_list str with
  | Ok v -> Ok v
  | Error _err -> Rresult.R.error_msgf "Invalid DKIM Signature: %S" str
  | exception _ -> Rresult.R.error_msgf "Invalid DKIM Signature: %S" str

let parse_dkim_server_value str =
  let open Rresult in
  let _, unstrctrd = Unstrctrd.safely_decode str in
  let unstrctrd = trim unstrctrd in
  match
    Angstrom.parse_string ~consume:All Decoder.server_tag_list
      (Unstrctrd.to_utf_8_string unstrctrd)
  with
  | Ok _ as v -> v
  | Error _ ->
      Log.warn (fun m -> m "Invalid DKIM server value: %S" str) ;
      Rresult.R.error_msgf "Invalid DKIM value"
  | exception _ ->
      Log.warn (fun m -> m "Invalid DKIM server value: %S" str) ;
      Rresult.R.error_msgf "Invalid DKIM value"

type newline = CRLF | LF

let sub_string_and_replace_newline chunk len =
  let count = ref 0 in
  String.iter
    (function '\n' -> incr count | _ -> ())
    (Bytes.sub_string chunk 0 len) ;
  let plus = !count in
  let pos = ref 0 in
  let res = Bytes.create (len + plus) in
  for i = 0 to len - 1 do
    match Bytes.unsafe_get chunk i with
    | '\n' ->
        Bytes.unsafe_set res !pos '\r' ;
        Bytes.unsafe_set res (!pos + 1) '\n' ;
        pos := !pos + 2
    | chr ->
        Bytes.unsafe_set res !pos chr ;
        incr pos
  done ;
  Bytes.unsafe_to_string res

let sanitize_input newline chunk len =
  match newline with
  | CRLF -> Bytes.sub_string chunk 0 len
  | LF -> sub_string_and_replace_newline chunk len

let field_dkim_signature = Mrmime.Field_name.v "DKIM-Signature"

type extracted = {
  dkim_fields : (Mrmime.Field_name.t * Unstrctrd.t * Map.t) list;
  fields : (Mrmime.Field_name.t * Unstrctrd.t) list;
  prelude : string;
}

let to_unstrctrd unstructured =
  let fold acc = function #Unstrctrd.elt as elt -> elt :: acc | _ -> acc in
  let unstrctrd = List.fold_left fold [] unstructured in
  Rresult.R.get_ok (Unstrctrd.of_list (List.rev unstrctrd))

let p =
  let open Mrmime in
  let unstructured = Field.(Witness Unstructured) in
  let open Field_name in
  Map.empty
  |> Map.add date unstructured
  |> Map.add from unstructured
  |> Map.add sender unstructured
  |> Map.add reply_to unstructured
  |> Map.add (v "To") unstructured
  |> Map.add cc unstructured
  |> Map.add bcc unstructured
  |> Map.add subject unstructured
  |> Map.add message_id unstructured
  |> Map.add comments unstructured
  |> Map.add content_type unstructured
  |> Map.add content_encoding unstructured

let extract_dkim :
    type flow backend.
    ?newline:newline ->
    flow ->
    backend state ->
    (module FLOW with type flow = flow and type backend = backend) ->
    ((extracted, _) or_err, backend) io =
  fun (type flow backend) ?(newline = LF) (flow : flow) (state : backend state)
      (module Flow : FLOW with type flow = flow and type backend = backend) ->
   let open Mrmime in
   let ( >>= ) = state.bind in
   let return = state.return in

   let chunk = 0x1000 in
   (* XXX(dinosaure): be aware about [lwt] implementaton if you
      change this value. *)
   let raw = Bytes.create chunk in
   let buffer = Bigstringaf.create (2 * chunk) in
   let decoder = Hd.decoder ~p buffer in
   let rec go others acc =
     match Hd.decode decoder with
     | `Field field -> (
         let (Field.Field (field_name, w, v)) = Location.prj field in
         match (Field_name.equal field_name field_dkim_signature, w) with
         | true, Field.Unstructured -> (
             (* TODO(dinosaure): we should add [DKIM-Signature] into
              * [others] when it's possible for a given [DKIM-Signature] to
              * sign an old one. *)
             let v = to_unstrctrd v in
             match parse_dkim_field_value v with
             | Ok dkim -> go others ((field_name, v, dkim) :: acc)
             | Error (`Msg err) ->
                 Log.warn (fun m -> m "Ignore DKIM-Signature: %s." err) ;
                 go others acc)
         | false, Field.Unstructured ->
             let v = to_unstrctrd v in
             go ((field_name, v) :: others) acc
         (* TODO(dinosaure): [mrmime] tries to parse some specific fields
          * such as [Date:] with their formats. [p] enforces to parse all
          * of these fields with [Unstructured].
          *
          * So, we can not have something else than [Unstructured] - however,
          * from the POV of the API, it's not so good to do that (so an update
          * of [mrmime] should be done). *)
         | _ -> assert false)
     | `Malformed _err ->
         Log.err (fun m -> m "The given email is malformed.") ;
         return (Rresult.R.error_msg "Invalid email")
     | `End rest ->
         return
           (Rresult.R.ok
              {
                prelude = rest;
                fields = List.rev others;
                dkim_fields = List.rev acc;
              })
     | `Await -> (
         Flow.input flow raw 0 (Bytes.length raw) >>= fun len ->
         let raw = sanitize_input newline raw len in
         match Hd.src decoder raw 0 (String.length raw) with
         | Ok () -> go others acc
         | Error _ as err -> return err) in
   go [] []

type whash = V : 'k Digestif.hash -> whash

let pp_hash ppf (V hash) =
  let open Digestif in
  match hash with
  | SHA1 -> Fmt.string ppf "sha1"
  | SHA256 -> Fmt.string ppf "sha256"
  | _ -> assert false

let equal_hash :
    type a b. a Digestif.hash -> b Digestif.hash -> (a, b) Refl.t option =
 fun a b ->
  let open Digestif in
  match (a, b) with
  | SHA1, SHA1 -> Some Refl.Refl
  | SHA256, SHA256 -> Some Refl.Refl
  | _, _ -> None

type vhash = H : 'k Digestif.hash * 'k Digestif.t -> vhash

type signed = string * vhash

type unsigned = unit

type 'signature dkim = {
  v : int;
  a : Value.algorithm * whash;
  c : Value.canonicalization * Value.canonicalization;
  d : [ `raw ] Domain_name.t;
  h : Mrmime.Field_name.t list;
  i : Value.auid option;
  l : int option;
  q : Value.query list;
  s : [ `raw ] Domain_name.t;
  t : int64 option;
  x : int64 option;
  z : (Mrmime.Field_name.t * string) list;
  signature : 'signature;
}

let fields { h; _ } = h

type algorithm = [ `RSA ]

type hash = [ `SHA1 | `SHA256 ]

type canonicalization = [ `Simple | `Relaxed ]

type query = [ `DNS of [ `TXT ] ]

let v ?(version = 1) ?(fields = [ Mrmime.Field_name.from ]) ~selector
    ?(algorithm = `RSA) ?(hash = `SHA256)
    ?(canonicalization = (`Relaxed, `Relaxed)) ?length ?(query = `DNS `TXT)
    ?timestamp ?expiration domain =
  if version <> 1 then Fmt.invalid_arg "Invalid version number: %d" version ;
  if List.length fields = 0
  then Fmt.invalid_arg "Require at last one field to sign an email" ;
  let a =
    match (algorithm, hash) with
    | `RSA, `SHA1 -> (Value.RSA, V Digestif.SHA1)
    | `RSA, `SHA256 -> (Value.RSA, V Digestif.SHA256) in
  let c =
    match canonicalization with
    | `Relaxed, `Relaxed -> (Value.Relaxed, Value.Relaxed)
    | `Relaxed, `Simple -> (Value.Relaxed, Value.Simple)
    | `Simple, `Relaxed -> (Value.Simple, Value.Relaxed)
    | `Simple, `Simple -> (Value.Simple, Value.Simple) in
  let q = [ ((query, None) :> Value.query) ] in
  let d = domain in
  let t = timestamp in
  let x = expiration in
  let h =
    if List.exists Mrmime.Field_name.(equal from) fields
    then fields
    else Mrmime.Field_name.from :: fields in
  let l = length in
  let s = selector in
  { v = version; a; c; d; t; x; h; l; s; i = None; z = []; q; signature = () }

let selector { s; _ } = s

let domain { d; _ } = d

module Encoder = struct
  open Prettym

  let tag pvalue ppf (key, value) =
    eval ppf
      [ box; !!string; cut; char $ '='; !!pvalue; cut; char $ ';'; close ]
      key value

  let version ppf v =
    let int ppf v = eval ppf [ !!string ] (string_of_int v) in
    tag int ppf ("v", v)

  let fields ppf lst =
    let sep = ((fun ppf () -> eval ppf [ cut; char $ ':'; cut ]), ()) in
    let field_name ppf (v : Mrmime.Field_name.t) =
      eval ppf [ !!string ] (String.lowercase_ascii (v :> string)) in
    eval ppf [ !!(tag (list ~sep field_name)) ] ("h", lst)

  let query ppf v =
    (* TODO(dinosaure): optional quoted-printable? *)
    let query ppf = function
      | `DNS `TXT, _ -> eval ppf [ string $ "dns/txt" ]
      | `Query_ext v, _ -> eval ppf [ !!string ] v in
    let sep = ((fun ppf () -> eval ppf [ cut; char $ ':'; cut ]), ()) in
    match v with
    | List.[] -> ppf
    | queries -> eval ppf [ !!(tag (list ~sep query)); fws ] ("q", queries)

  let length ppf v =
    let int ppf v = eval ppf [ !!string ] (string_of_int v) in
    tag int ppf ("l", v)

  let timestamp ppf v =
    let int64 ppf v = eval ppf [ !!string ] (Int64.to_string v) in
    tag int64 ppf ("t", v)

  let expiration ppf v =
    let int64 ppf v = eval ppf [ !!string ] (Int64.to_string v) in
    tag int64 ppf ("x", v)

  let domain ppf v =
    let domain ppf v = eval ppf [ !!string ] (Domain_name.to_string v) in
    tag domain ppf ("d", v)

  let selector ppf v =
    let domain ppf v = eval ppf [ !!string ] (Domain_name.to_string v) in
    tag domain ppf ("s", v)

  let canonicalization ppf v =
    let c ppf = function
      | Value.Relaxed, Value.Relaxed -> string ppf "relaxed/relaxed"
      | Value.Simple, Value.Simple -> string ppf "simple/simple"
      | Value.Relaxed, Value.Simple -> string ppf "relaxed/simple"
      | Value.Simple, Value.Relaxed -> string ppf "simple/relaxed"
      | Value.Simple, Value.Canonicalization_ext v ->
          eval ppf [ string $ "simple"; char $ '/'; !!string ] v
      | Value.Relaxed, Value.Canonicalization_ext v ->
          eval ppf [ string $ "relaxed"; char $ '/'; !!string ] v
      | Value.Canonicalization_ext v, Value.Simple ->
          eval ppf [ !!string; char $ '/'; string $ "simple" ] v
      | Value.Canonicalization_ext v, Value.Relaxed ->
          eval ppf [ !!string; char $ '/'; string $ "relaxed" ] v
      | Value.Canonicalization_ext a, Value.Canonicalization_ext b ->
          eval ppf [ !!string; char $ '/'; !!string ] a b in
    tag c ppf ("c", v)

  let algorithm ppf v =
    let algorithm ppf = function
      | Value.RSA, hash ->
          let hash = Fmt.strf "%a" pp_hash hash in
          eval ppf [ string $ "rsa"; cut; char $ '-'; cut; !!string ] hash
      | Value.ED25519, hash ->
          let hash = Fmt.strf "%a" pp_hash hash in
          eval ppf [ string $ "ed25519"; cut; char $ '-'; cut; !!string ] hash
      | Value.Algorithm_ext v, hash ->
          let hash = Fmt.strf "%a" pp_hash hash in
          eval ppf [ !!string; cut; char $ '-'; cut; !!string ] v hash in
    tag algorithm ppf ("a", v)

  let body_hash ppf v =
    let hash ppf (H (k, hash)) =
      let str = Base64.encode_exn ~pad:true (Digestif.to_raw_string k hash) in
      let rec go ppf idx =
        if idx = String.length str
        then ppf
        else
          let ppf = eval ppf [ cut; !!char; cut ] str.[idx] in
          go ppf (succ idx) in
      go ppf 0 in
    tag hash ppf ("bh", v)

  let signature ppf v =
    let signature ppf = function
      | "" -> ppf
      | signature ->
          let str = Base64.encode_exn ~pad:true signature in
          let rec go ppf idx =
            if idx = String.length str
            then ppf
            else
              let ppf = eval ppf [ cut; !!char; cut ] str.[idx] in
              go ppf (succ idx) in
          go ppf 0 in
    tag signature ppf ("b", v)

  let option_with_fws fmt ppf = function
    | None -> ppf
    | Some v -> eval ppf [ !!fmt; fws ] v

  let dkim_signature ppf (dkim : signed dkim) =
    let b, bh = dkim.signature in
    eval ppf
      [
        !!version;
        fws;
        !!algorithm;
        fws;
        !!canonicalization;
        fws;
        !!domain;
        fws;
        !!selector;
        fws;
        !!(option_with_fws timestamp);
        !!(option_with_fws expiration);
        !!query;
        !!(option_with_fws length);
        !!body_hash;
        fws;
        !!fields;
        fws;
        !!signature;
        fws;
      ]
      dkim.v dkim.a dkim.c dkim.d dkim.s dkim.t dkim.x dkim.q dkim.l bh dkim.h b

  let as_field ppf dkim =
    eval ppf
      [
        string $ "DKIM-Signature";
        char $ ':';
        tbox 1;
        spaces 1;
        !!dkim_signature;
        close;
        new_line;
      ]
      dkim
end

let digesti_of_hash (V hash) f =
  let v = Digestif.digesti_string hash f in
  H (hash, v)

type server = {
  v : Value.server_version;
  h : whash list;
  k : Value.algorithm;
  n : string option;
  p : string;
  s : Value.service list;
  t : Value.name list;
}

let pp_signature (V hash) ppf (H (hash', value)) =
  match equal_hash hash hash' with
  | Some Refl.Refl -> Digestif.pp hash ppf value
  | None -> assert false

(* XXX(dinosaure): should never occur. *)

let pp_hex ppf str =
  for i = 0 to String.length str - 1 do
    Fmt.pf ppf "%02x" (Char.code str.[i])
  done

let pp_dkim : type a. a dkim Fmt.t =
 fun ppf t ->
  Fmt.pf ppf
    "{ @[<hov>v = %d;@ a = %a;@ c = %a;@ d = %a;@ h = @[<hov>%a@];@ i = \
     @[<hov>%a@];@ l = %a;@ q = @[<hov>%a@];@ s = %a;@ t = %a;@ x = %a;@ z = \
     @[<hov>%a@];@] }"
    t.v
    Fmt.(Dump.pair Value.pp_algorithm pp_hash)
    t.a
    Fmt.(Dump.pair Value.pp_canonicalization Value.pp_canonicalization)
    t.c Domain_name.pp t.d
    Fmt.(Dump.list Mrmime.Field_name.pp)
    t.h
    Fmt.(Dump.option Value.pp_auid)
    t.i
    Fmt.(Dump.option int)
    t.l
    Fmt.(Dump.list Value.pp_query)
    t.q Domain_name.pp t.s
    Fmt.(Dump.option int64)
    t.t
    Fmt.(Dump.option int64)
    t.x
    Fmt.(Dump.list Value.pp_copy)
    t.z

let pp_server ppf (t : server) =
  Fmt.pf ppf
    "{ @[<hov>v = %s;@ h = @[<hov>%a@];@ k = %a;@ n = %a;@ p = %a;@ s = \
     @[<hov>%a@];@ t = @[<hov>%a@];@] }"
    t.v
    Fmt.(Dump.list pp_hash)
    t.h Value.pp_algorithm t.k
    Fmt.(Dump.option string)
    t.n pp_hex t.p
    Fmt.(Dump.list Value.pp_service)
    t.s
    Fmt.(Dump.list Value.pp_name)
    t.t

let expected : signed dkim -> vhash = fun { signature = _, bh; _ } -> bh

let hash = function
  | Value.SHA1 -> V Digestif.SHA1
  | Value.SHA256 -> V Digestif.SHA256
  | Value.Hash_ext x ->
  match String.lowercase_ascii x with
  | "sha512" -> V Digestif.SHA512
  | x -> Fmt.invalid_arg "Invalid kind of hash <%s>" x

let string_of_quoted_printable x =
  let decoder = Pecu.Inline.decoder (`String x) in
  let res = Buffer.create 0x800 in
  let rec go () =
    match Pecu.Inline.decode decoder with
    | `Await -> assert false
    | `Char chr ->
        Buffer.add_char res chr ;
        go ()
    | `End -> Rresult.R.ok (Buffer.contents res)
    | `Malformed err -> Rresult.R.error_msg err in
  go ()

module SSet = Set.Make (Mrmime.Field_name)

let post_process_dkim hmap =
  let v =
    match Map.find Map.K.v hmap with
    | Some v -> v
    | None -> Fmt.failwith "Version is required" in
  let a =
    match Map.find Map.K.a hmap with
    | Some (alg, x) -> (alg, hash x)
    | None -> Fmt.failwith "Algorithm is required" in
  let b =
    match Option.map (Base64.decode ~pad:false) (Map.find Map.K.b hmap) with
    | Some (Ok v) -> v
    | Some (Error (`Msg err)) -> failwith err
    | None -> Fmt.failwith "Signature data is required" in
  let bh =
    match Option.map (Base64.decode ~pad:false) (Map.find Map.K.bh hmap) with
    | Some (Error (`Msg err)) -> failwith err
    | None -> Fmt.failwith "Hash of canonicalized body part is required"
    | Some (Ok v) -> (
        let _, V k = a in
        match Digestif.of_raw_string_opt k v with
        | Some v -> H (k, v)
        | None -> Fmt.failwith "Invalid hash") in
  let c =
    match Map.find Map.K.c hmap with
    | Some v -> v
    | None -> (Value.Simple, Value.Simple) in
  let d =
    match Map.find Map.K.d hmap with
    | Some v ->
        Rresult.R.failwith_error_msg
          (Domain_name.of_string (String.concat "." v))
    | None -> Fmt.failwith "SDID is required" in
  let h =
    match Map.find Map.K.h hmap with
    | Some v -> v
    | None -> Fmt.failwith "Signed header fields required" in
  let i = Map.find Map.K.i hmap in
  let l = Map.find Map.K.l hmap in
  let q =
    List.map
      (fun (q, x) ->
        match Option.map string_of_quoted_printable x with
        | None -> (q, None)
        | Some (Ok x) -> (q, Some x)
        | Some (Error (`Msg err)) -> failwith err)
      (Option.value ~default:[] (Map.find Map.K.q hmap)) in
  let s =
    match Map.find Map.K.s hmap with
    | Some v ->
        Rresult.R.failwith_error_msg
          (Domain_name.of_string (String.concat "." v))
    | None -> Fmt.failwith "Selector is required" in
  let t = Map.find Map.K.t hmap in
  let x = Map.find Map.K.x hmap in
  let z =
    List.map
      (fun (f, x) ->
        match string_of_quoted_printable x with
        | Ok x -> (f, x)
        | Error (`Msg err) -> failwith err)
      Option.(value ~default:[] (Map.find Map.K.z hmap)) in
  { v; a; c; d; h; i; l; q; s; t; x; z; signature = (b, bh) }

let post_process_dkim hmap =
  try Rresult.R.ok (post_process_dkim hmap)
  with Failure err -> Rresult.R.error_msg err

let post_process_server hmap =
  let v = Option.value ~default:"DKIM1" (Map.find Map.K.sv hmap) in
  let h =
    Option.value
      ~default:[ V Digestif.SHA1; V Digestif.SHA256 ]
      (Option.map (List.map hash) (Map.find Map.K.sh hmap)) in
  let k = Option.value ~default:Value.RSA (Map.find Map.K.k hmap) in
  let n = Map.find Map.K.n hmap in
  let p =
    match Option.map (Base64.decode ~pad:false) (Map.find Map.K.p hmap) with
    | Some (Ok p) -> p
    | Some (Error (`Msg err)) -> invalid_arg err
    | None -> Fmt.invalid_arg "Public-key is required" in
  let s = Option.value ~default:[ Value.All ] (Map.find Map.K.ss hmap) in
  let t = Option.value ~default:[] (Map.find Map.K.st hmap) in
  { v; h; k; n; p; s; t }

let post_process_server hmap =
  try Rresult.R.ok (post_process_server hmap)
  with Invalid_argument err -> Rresult.R.error_msg err

let simple_field_canonicalization (field_name : Mrmime.Field_name.t) unstrctrd f
    =
  (* TODO: delete trailing CRLF. *)
  f (field_name :> string) ;
  f ":" ;
  f (Unstrctrd.to_utf_8_string unstrctrd)

let simple_dkim_field_canonicalization (dkim_field : Mrmime.Field_name.t) raw f
    =
  f (dkim_field :> string) ;
  f ":" ;
  f Unstrctrd.(to_utf_8_string raw)

let trim unstrctrd =
  let space = Unstrctrd.wsp ~len:1 in
  let fold (acc, state) elt =
    match elt with
    | (`WSP _ | `FWS _) when state -> (acc, true)
    | `WSP _ | `FWS _ -> (space :: acc, state)
    | elt -> (elt :: acc, false) in
  Unstrctrd.fold ~f:fold ([], true) unstrctrd |> fun (lst, _) ->
  List.fold_left fold ([], true) lst |> fun (lst, _) ->
  Unstrctrd.of_list lst |> Rresult.R.get_ok

let uniq unstrctrd =
  let fold (acc, state) elt =
    match elt with
    | (`FWS _ | `WSP _) when state -> (acc, true)
    | `FWS _ | `WSP _ -> (elt :: acc, true)
    | elt -> (elt :: acc, false) in
  Unstrctrd.fold ~f:fold ([], false) unstrctrd |> fun (lst, _) ->
  Unstrctrd.of_list (List.rev lst) |> Rresult.R.get_ok

let relaxed_field_canonicalization (field_name : Mrmime.Field_name.t) unstrctrd
    f =
  f (String.lowercase_ascii (field_name :> string)) ;
  f ":" ;
  let unstrctrd = (uniq <.> trim) unstrctrd in
  f (Unstrctrd.to_utf_8_string unstrctrd) ;
  Log.debug (fun m ->
      m "Digest %s:%s."
        (String.lowercase_ascii (field_name :> string))
        (Unstrctrd.to_utf_8_string unstrctrd)) ;
  f "\r\n"

let relaxed_dkim_field_canonicalization (field_name : Mrmime.Field_name.t)
    unstrctrd f =
  f (String.lowercase_ascii (field_name :> string)) ;
  f ":" ;
  (*
  let buf = Buffer.create 8 in
  let iter : Unstrctrd.elt -> unit = function
    | `CR -> f "\r"
    | `FWS _ | `WSP _ -> f " "
    | `Invalid_char chr -> f (String.make 1 (chr :> char))
    | `LF -> f "\n"
    | `OBS_NO_WS_CTL chr -> f (String.make 1 (chr :> char))
    | `Uchar uchar ->
        Uutf.Buffer.add_utf_8 buf uchar ;
        f (Buffer.contents buf) ;
        Buffer.reset buf
    | `d0 -> f "\000" in
  *)
  let unstrctrd = (uniq <.> trim) unstrctrd in
  f (Unstrctrd.to_utf_8_string unstrctrd)

let crlf digest n =
  let rec go = function
    | 0 -> ()
    | n ->
        digest "\r\n" ;
        go (pred n) in
  if n < 0 then Fmt.invalid_arg "Expect at least 0 <crlf>" else go n

type iter = string Digestif.iter

type body = { relaxed : iter; simple : iter }

let extract_body :
    type flow backend.
    ?newline:newline ->
    flow ->
    backend state ->
    (module FLOW with type flow = flow and type backend = backend) ->
    prelude:string ->
    (body, backend) io =
 fun ?(newline = LF) (type flow backend) (flow : flow) (state : backend state)
     (module Flow : FLOW with type flow = flow and type backend = backend)
     ~prelude ->
  let ( >>= ) = state.bind in
  let return = state.return in

  let decoder = Body.decoder () in
  let chunk = 0x1000 in
  let raw = Bytes.create (max chunk (String.length prelude)) in
  let qr = Queue.create () in
  let qs = Queue.create () in
  let fr x = Queue.push x qr in
  let fs x = Queue.push x qs in

  Bytes.blit_string prelude 0 raw 0 (String.length prelude) ;

  (* XXX(dinosaure): [prelude] comes from [extract_dkim] and should be [<= 0x1000].
     Seems to be not true. *)
  let digest_stack ?(relaxed = false) f l =
    let rec go = function
      | [] -> ()
      | [ `Spaces x ] -> f (if relaxed then " " else x)
      | `CRLF :: r ->
          f "\r\n" ;
          go r
      | `Spaces x :: r ->
          if not relaxed then f x ;
          go r in
    go (List.rev l) in
  let rec go stack =
    match Body.decode decoder with
    | `Await ->
        Flow.input flow raw 0 (Bytes.length raw) >>= fun len ->
        let raw = sanitize_input newline raw len in
        Body.src decoder (Bytes.of_string raw) 0 (String.length raw) ;
        go stack
    | `End ->
        crlf fr 1 ;
        crlf fs 1 ;
        return ()
    | `Spaces _ as x -> go (x :: stack)
    | `CRLF -> go (`CRLF :: stack)
    | `Data x ->
        digest_stack ~relaxed:true fr stack ;
        fr x ;
        digest_stack fs stack ;
        fs x ;
        go [] in
  Body.src decoder raw 0 (String.length prelude) ;
  go [] >>= fun () ->
  return
    {
      relaxed = (fun f -> Queue.iter f qr);
      simple = (fun f -> Queue.iter f qs);
    }

(* XXX(dinosaure): seriously, going to hell DKIM! From [dkimpy]:
   re.compile(br[\s]b'+FWS+br'=) (?:'+FWS+br'[a-zA-Z0-9+/=])*(?:\r?\n\Z)?' this is does
   NOT MEAN ANYTHING BOY. *)

let remove_signature_of_raw_dkim unstrctrd =
  let fold (acc, state) elt =
    match (elt, state) with
    | `Uchar uchar, _ -> (
        if Uchar.is_char uchar
        then
          match (Uchar.to_char uchar, state) with
          | 'b', `_0 -> (elt :: acc, `_1)
          | '=', `_1 -> (elt :: acc, `_2)
          | ';', `_2 -> (elt :: acc, `_3)
          | _, `_0 -> (elt :: acc, `_0)
          | _, `_1 -> (elt :: acc, `_0)
          | _, `_2 -> (acc, `_2)
          | _, `_3 -> (elt :: acc, `_3)
        else
          match state with
          | `_0 | `_1 -> (elt :: acc, `_0)
          | `_3 -> (elt :: acc, `_3)
          | `_2 -> (acc, `_2))
    | elt, (`_0 | `_1) -> (elt :: acc, `_0)
    | _, `_2 -> (acc, `_2)
    | elt, `_3 -> (elt :: acc, `_3) in
  let res, _ = Unstrctrd.fold ~f:fold ([], `_0) unstrctrd in
  Rresult.R.get_ok (Unstrctrd.of_list (List.rev res))

let body_hash_of_dkim body dkim =
  let digesti = digesti_of_hash (snd dkim.a) in
  match snd dkim.c with
  | Value.Simple -> digesti body.simple
  | Value.Relaxed -> digesti body.relaxed
  | Value.Canonicalization_ext x ->
      Fmt.invalid_arg "%s canonicalisation is not supported" x

let extract_server :
    type t backend.
    t ->
    backend state ->
    (module DNS with type t = t and type backend = backend) ->
    _ dkim ->
    ((Map.t, _) or_err, backend) io =
  fun (type t backend) (t : t) (state : backend state)
      (module Dns : DNS with type t = t and type backend = backend)
      (dkim : _ dkim) ->
   let ( >>= ) = state.bind in
   let return = state.return in
   let ( >>? ) x f =
     x >>= function Ok x -> f x | Error err -> return (Error err) in

   let domain_name =
     let open Rresult in
     Domain_name.prepend_label dkim.d "_domainkey" >>= Domain_name.append dkim.s
   in
   return domain_name >>? fun domain_name ->
   Dns.getaddrinfo t `TXT domain_name >>? fun lst ->
   (* XXX(dinosaure): RFC 6376 said: Strings in a TXT RR MUST be concatenated
      together before use with no intervening whitespace. *)
   let lst = List.map (String.concat "" <.> Astring.String.cuts ~sep:" ") lst in
   let str = String.concat "" lst in
   return (parse_dkim_server_value str)

let assoc field_name fields =
  let res = ref None in
  List.iter
    (fun ((field_name', _) as v) ->
      if Mrmime.Field_name.equal field_name field_name' && Option.is_none !res
      then res := Some v)
    fields ;
  !res

let remove_assoc field_name fields =
  let fold (res, deleted) ((field_name', _) as v) =
    if Mrmime.Field_name.equal field_name field_name' && not deleted
    then (res, true)
    else (v :: res, deleted) in
  let res, _ = List.fold_left fold ([], false) fields in
  List.rev res

let data_hash_of_dkim fields ((field_dkim : Mrmime.Field_name.t), raw_dkim) dkim
    =
  (* In hash step 2, the Signer/Verifiers MUST pass the following to the hash
     algorithm in the indicated order. *)
  let digesti = digesti_of_hash (snd dkim.a) in
  let canonicalization =
    match fst dkim.c with
    | Value.Simple -> simple_field_canonicalization
    | Value.Relaxed -> relaxed_field_canonicalization
    | Value.Canonicalization_ext x ->
        Fmt.invalid_arg "%s canonicalisation is not supported" x in
  let dkim_field_canonicalization =
    match fst dkim.c with
    | Value.Simple -> simple_dkim_field_canonicalization
    | Value.Relaxed -> relaxed_dkim_field_canonicalization
    | Value.Canonicalization_ext x ->
        Fmt.invalid_arg "%s canonicalisation is not supported" x in
  let q = Queue.create () in
  (* The header fields specified by the "h=" tag, in the order specified in that
     tag, and canonicalized using the header canonicalization algorithm
     specified in the "c=" tag. Each field MUST be terminated with a single
     CRLF. *)
  let _ =
    List.fold_left
      (fun fields requested ->
        Log.debug (fun m -> m "Field %a." Mrmime.Field_name.pp requested) ;
        match assoc requested fields with
        | Some (field_name, unstrctrd) ->
            canonicalization field_name unstrctrd (fun x -> Queue.push x q) ;
            remove_assoc field_name fields
        | None -> fields)
      (List.rev fields) dkim.h in
  (* The DKIM-Signature header field that exists (verifying) or will be inserted
     (signing) in the message, with the value of the "b=" tag (including all
     surrounding whitespace) deleted (i.e., treated as the empty string),
     canonicalized using the header canonicalization algorithm specified in the
     "c=" tag, and without a trailing CRLF. *)
  let raw_dkim = remove_signature_of_raw_dkim raw_dkim in
  dkim_field_canonicalization field_dkim raw_dkim (fun x -> Queue.push x q) ;
  digesti (fun f -> Queue.iter f q)

let verify_body dkim body =
  let (H (k, v)) = body_hash_of_dkim body dkim in
  let (H (k', v')) = expected dkim in
  match equal_hash k k' with
  | Some Refl.Refl ->
      Log.debug (fun m ->
          m "Hash of body (expect: %a): %a." (Digestif.pp k) v' (Digestif.pp k)
            v) ;
      Digestif.equal k v v'
  | None -> false

let verify fields (dkim_signature : Mrmime.Field_name.t * Unstrctrd.t) dkim
    server body =
  let (H (k, hash)) = data_hash_of_dkim fields dkim_signature dkim in
  Log.debug (fun m ->
      m "Hash of fields: %a." (pp_signature (V k)) (H (k, hash))) ;
  (* DER-encoded X.509 RSAPublicKey. *)
  let hashp a =
    match (a, k) with
    | `SHA1, Digestif.SHA1 -> true
    | `SHA224, Digestif.SHA224 -> true
    | `SHA256, Digestif.SHA256 -> true
    | `SHA384, Digestif.SHA384 -> true
    | `SHA512, Digestif.SHA512 -> true
    | `MD5, Digestif.MD5 -> true
    | _, _ -> false in

  match
    (X509.Public_key.decode_der (Cstruct.of_string server.p), fst dkim.a)
  with
  | Ok (`RSA key), Value.RSA ->
      let digest = `Digest (Cstruct.of_string (Digestif.to_raw_string k hash)) in
      let r0 =
        let b, _ = dkim.signature in
        Mirage_crypto_pk.Rsa.PKCS1.verify ~hashp ~key
          ~signature:(Cstruct.of_string b) digest in
      Log.debug (fun m -> m "Header fields verified: %b." r0) ;
      let r1 = verify_body dkim body in
      Log.debug (fun m -> m "Body verified: %b." r1) ;

      r0 && r1
  | Ok (`ED25519 key), Value.ED25519 ->
      let msg = Cstruct.of_string (Digestif.to_raw_string k hash) in
      let r0 =
        let b, _ = dkim.signature in
        Mirage_crypto_ec.Ed25519.verify ~key (Cstruct.of_string b) ~msg in
      Log.debug (fun m -> m "Header fields verified: %b." r0) ;
      let r1 = verify_body dkim body in
      Log.debug (fun m -> m "Body verified: %b." r1) ;

      r0 && r1
  | Ok _, _ ->
      Log.err (fun m -> m "We handle only RSA & ED25519 algorithms.") ;
      false
  | Error (`Msg err), _ ->
      Log.err (fun m -> m "Invalid DER-encoded X.509 RSA public-key: %s" err) ;
      false

let dkim_field_and_value =
  let open Angstrom in
  let open Mrmime in
  let buf = Bytes.create 0x7f in
  let is_wsp = function ' ' | '\t' -> true | _ -> false in
  Field_name.Decoder.field_name >>= fun _ ->
  skip_while is_wsp *> char ':' *> Unstrctrd_parser.unstrctrd buf

let server_of_dkim : key:Mirage_crypto_pk.Rsa.priv -> 'a dkim -> server =
 fun ~key dkim ->
  let pub = Mirage_crypto_pk.Rsa.pub_of_priv key in
  let p = Cstruct.to_string (X509.Public_key.encode_der (`RSA pub)) in
  let k, h = dkim.a in
  { v = "DKIM1"; h = [ h ]; n = None; k; p; s = []; t = [] }

(* TODO(dinosaure): [s] and [t]. *)

let server_to_string server =
  let k_to_string = function
    | Value.RSA -> "rsa"
    | Value.ED25519 -> "ed25519"
    | Value.Algorithm_ext v -> v in
  let h_to_string lst =
    let h_to_string = function
      | V Digestif.SHA1 -> "sha1"
      | V Digestif.SHA256 -> "sha256"
      | _ -> assert false in
    let buf = Buffer.create 0x7f in
    let rec go = function
      | [] -> Buffer.contents buf
      | [ x ] ->
          Buffer.add_string buf (h_to_string x) ;
          Buffer.contents buf
      | x :: r ->
          Buffer.add_string buf (h_to_string x) ;
          Buffer.add_char buf ':' ;
          go r in
    go lst in
  let lst =
    [
      ("v", server.v);
      ("p", Base64.encode_exn ~pad:true server.p);
      ("k", k_to_string server.k);
    ] in
  let lst = match server.h with [] -> lst | h -> ("h", h_to_string h) :: lst in
  let lst = Option.fold ~none:lst ~some:(fun n -> ("n", n) :: lst) server.n in
  let buf = Buffer.create 0x7f in
  let ppf = Format.formatter_of_buffer buf in
  let rec go ppf = function
    | [] -> Format.fprintf ppf "%!"
    | [ (k, v) ] -> Format.fprintf ppf "%s=%s;" k v
    | (k, v) :: r ->
        Format.fprintf ppf "%s=%s; " k v ;
        go ppf r in
  go ppf lst ;
  Buffer.contents buf

let domain_name :
    'a dkim -> ([ `raw ] Domain_name.t, [> `Msg of string ]) result =
 fun dkim ->
  let open Rresult in
  Domain_name.prepend_label dkim.d "_domainkey" >>= Domain_name.append dkim.s

let sign :
    type flow backend.
    key:Mirage_crypto_pk.Rsa.priv ->
    ?newline:newline ->
    flow ->
    backend state ->
    (module FLOW with type flow = flow and type backend = backend) ->
    unsigned dkim ->
    (signed dkim, backend) io =
 fun ~key ?(newline = LF) flow ({ bind; return } as state) (module Flow) dkim ->
  let digesti = digesti_of_hash (snd dkim.a) in
  let canon =
    match fst dkim.c with
    | Value.Simple -> simple_field_canonicalization
    | Value.Relaxed -> relaxed_field_canonicalization
    | Value.Canonicalization_ext x ->
        Fmt.invalid_arg "%s canonicalisation is not supported" x in
  let q = Queue.create () in

  let open Mrmime in
  let ( >>= ) = bind in
  let chunk = 0x1000 in
  let raw = Bytes.create chunk in
  let buffer = Bigstringaf.create (2 * chunk) in
  let decoder = Hd.decoder ~p buffer in
  let rec go fields =
    match Hd.decode decoder with
    | `Field field -> (
        let (Field.Field (field_name, w, v)) = Location.prj field in
        match w with
        | Field.Unstructured ->
            let v = to_unstrctrd v in
            go ((field_name, v) :: fields)
            (* XXX(dinosaure): [assert false] is safe when the decoder can only emit
               [Unstructured] value. *)
        | _ -> assert false)
    | `Malformed err -> Fmt.invalid_arg "Invalid e-mail: %s" err
    | `End rest -> return (rest, fields)
    | `Await -> (
        Flow.input flow raw 0 (Bytes.length raw) >>= fun len ->
        let raw = sanitize_input newline raw len in
        match Hd.src decoder raw 0 (String.length raw) with
        | Ok () -> go fields
        | Error (`Msg err) -> Fmt.invalid_arg "Invalid e-mail: %s" err) in
  go [] >>= fun (prelude, fields) ->
  let _ =
    List.fold_left
      (fun fields requested ->
        match assoc requested fields with
        | Some (field_name, unstrctrd) ->
            canon field_name unstrctrd (fun x -> Queue.push x q) ;
            remove_assoc field_name fields
        | None -> fields)
      fields dkim.h in
  extract_body ~newline flow state (module Flow) ~prelude >>= fun body ->
  let bh =
    match snd dkim.c with
    | Value.Simple -> digesti body.simple
    | Value.Relaxed -> digesti body.relaxed
    | Value.Canonicalization_ext x ->
        Fmt.invalid_arg "%s canonicalisation is not supported" x in
  let dkim' = { dkim with signature = ("", bh) } in
  let () =
    let canon =
      match fst dkim.c with
      | Value.Simple -> simple_dkim_field_canonicalization
      | Value.Relaxed -> relaxed_dkim_field_canonicalization
      | Value.Canonicalization_ext x ->
          Fmt.invalid_arg "%s canonicalisation is not supported" x in
    let str = Prettym.to_string ~new_line:"\r\n" Encoder.as_field dkim' in
    match Angstrom.parse_string ~consume:All dkim_field_and_value str with
    | Ok unstrctrd ->
        canon field_dkim_signature unstrctrd (fun x -> Queue.push x q)
    | Error _ -> assert false
    (* XXX(dinosaure): must parse! *) in
  let (H (k, vhash)) = digesti (fun f -> Queue.iter f q) in
  let message = `Digest (Cstruct.of_string (Digestif.to_raw_string k vhash)) in
  let hash =
    match k with
    | Digestif.SHA1 -> `SHA1
    | Digestif.SHA224 -> `SHA224
    | Digestif.SHA256 -> `SHA256
    | Digestif.SHA384 -> `SHA384
    | Digestif.SHA512 -> `SHA512
    | Digestif.MD5 -> `MD5
    | _ -> Fmt.invalid_arg "Unrecognized hash" in
  let signature = Mirage_crypto_pk.Rsa.PKCS1.sign ~hash ~key message in
  let signature = Cstruct.to_string signature in
  return { dkim' with signature = (signature, bh) }

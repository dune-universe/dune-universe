type ty      = Rfc2045.ty
type subty   = Rfc2045.subty
type value   = Rfc2045.value

type content = Rfc2045.content =
  { ty         : Rfc2045.ty
  ; subty      : Rfc2045.subty
  ; parameters : (string * Rfc2045.value) list }

let pp = Format.fprintf

let pp_ty fmt = function
  | `Text         -> pp fmt "text"
  | `Image        -> pp fmt "image"
  | `Audio        -> pp fmt "audio"
  | `Video        -> pp fmt "video"
  | `Application  -> pp fmt "application"
  | `Message      -> pp fmt "message"
  | `Multipart    -> pp fmt "multipart"
  | `Ietf_token s -> pp fmt "ietf:\"%s\"" s
  | `X_token s    -> pp fmt "x:\"%s\"" s

let pp_subty fmt = function
  | `Ietf_token s -> pp fmt "ietf:\"%s\"" s
  | `X_token s    -> pp fmt "x:\"%s\"" s
  | `Iana_token s -> pp fmt "iana:%s" s

let pp_value fmt = function
  | `String s -> pp fmt "%S" s
  | `Token s  -> pp fmt "\"%s\"" s

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_parameter fmt (key, value) =
  pp fmt "%s = %a" key pp_value value

let pp fmt { ty; subty; parameters; } =
  pp fmt "{@[<hov>type = %a/%a;@ parameters = [@[<hov>%a@]]@]}"
    pp_ty ty
    pp_subty subty
    (pp_lst ~sep:(fun fmt () -> pp fmt ";@ ") pp_parameter) parameters

let make ?(parameters = []) ty subty =
  let subty =
    try `Iana_token (Iana.Set.find (String.lowercase_ascii subty) (Iana.Map.find (Rfc2045.ty_to_string ty) Iana.iana))
    with _ -> `X_token subty
  in
  let parameters = List.map (fun (k, v) -> (String.lowercase_ascii k, v)) parameters in
  { ty; subty; parameters }

let equal_ty a b = match a, b with
  | `Ietf_token a, `Ietf_token b
  | `X_token a, `X_token b ->
    String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)
  | a, b -> a = b

let equal_subty a b = match a, b with
  | `Ietf_token a, `Ietf_token b
  | `Iana_token a, `Iana_token b
  | `X_token a, `X_token b ->
    String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)
  | _, _ -> false

let equal_parameter ?(insensitive = `All) a b =
  let insensitive = match insensitive with
    | `Parameters keys -> `Parameters (List.map String.lowercase_ascii keys) | x -> x in

  match a, b with
  | (key_a, `Token value_a), (key_b, `Token value_b)
  | (key_a, `Token value_a), (key_b, `String value_b)
  | (key_a, `String value_a), (key_b, `Token value_b)
  | (key_a, `String value_a), (key_b, `String value_b) ->
    if String.equal
        (String.lowercase_ascii key_a)
        (String.lowercase_ascii key_b)
    then match insensitive with
      | `Parameters keys when List.exists (String.equal (String.lowercase_ascii key_a)) keys ->
        String.equal
          (String.lowercase_ascii value_a)
          (String.lowercase_ascii value_b)
      | `All ->
        String.equal
          (String.lowercase_ascii value_a)
          (String.lowercase_ascii value_b)
      | _ ->
        String.equal value_a value_b
    else false

let equal_parameters ?insensitive a b =
  List.for_all2 (equal_parameter ?insensitive) a b

let equal ?(insensitive = `Parameters [ "boundaries" ]) a b =
  equal_ty a.ty b.ty
  && equal_subty a.subty b.subty
  && equal_parameters ~insensitive a.parameters b.parameters

let default =
  { ty = `Text
  ; subty = `Iana_token "plain"
  ; parameters = ["charset", `Token "us-ascii"] }

open Angstrom.Unbuffered

type error =
  [ `Invalid of (string * string list)
  | `Incomplete ]

let parser = Rfc2045.content

let of_string_with_crlf s =
  let bs = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout (String.length s) in

  for i = 0 to String.length s - 1
  do Bigarray.Array1.set bs i (String.get s i) done;

  let rec aux state = function
    | Fail (_, path, err) -> Error (`Invalid (err, path))
    | Partial { continue; _ } ->
      if state = `third
      then Error `Incomplete
      else
        let x = Bigarray.Array1.dim bs in
        let off, len, state = match state with
          | `first -> 0, x, `second
          | `second -> x, x, `third
          | `third -> assert false in
        aux state @@ continue ~off ~len bs Complete (* avoid the CFWS token *)
    | Done (_, v) -> Ok v
  in

  aux `first @@ parse Angstrom.(Rfc2045.content <* option () Rfc822.cfws <* Rfc822.crlf <* commit)

  (* XXX(dinosaure): the last CFWS was due to: 'Content-Type: CFWS content-value
                     CFWS CRLF'. the [content] handles the CFWS token inside the
                     Content-Type's value but not in the field value (as we can
                     find in RFC2045). with this, we can put a comment after the
                     Content-Type's value. Otherwise, we expect directly the
                     CRLF line-break without any comment.
   *)

let of_string s = of_string_with_crlf (s ^ "\r\n")

let of_string_raw s off len =
  let s = String.sub s off len in

  let bs = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout (String.length s) in

  for i = 0 to String.length s - 1
  do Bigarray.Array1.set bs i (String.get s i) done;

  let rec aux state = function
    | Fail (_, path, err) -> Error (`Invalid (err, path))
    | Partial {  continue; _ } ->
      if state = `third
      then Error `Incomplete
      else
        let x = Bigarray.Array1.dim bs in
        let off, len, state = match state with
          | `first -> 0, x, `second
          | `second -> x, x, `third
          | `third -> assert false in
        aux state @@ continue ~off ~len bs Complete (* avoid the CFWS token *)
    | Done (committed, v) -> Ok (v, committed)
  in

  aux `first @@ parse Angstrom.(Rfc2045.content <* option () Rfc822.cfws <* Rfc822.crlf <* commit)

open Core

module Params = struct
  type t = (Headers.Name.t * string) list [@@deriving sexp]

  let last t name =
    let name = Headers.Name.of_string name in
    List.fold t ~init:None ~f:(fun r (k,v) ->
      if Headers.Name.equal name k then Some v else r)
end

type t = {
  mime_type : Rfc.RFC2045.Token.t;
  mime_subtype : Rfc.RFC2045.Token.t;
  params : Params.t;
} [@@deriving sexp]

let is ?mime_type ?mime_subtype t =
  Option.value_map mime_type ~default:true
    ~f:(fun mime_type ->
      Rfc.RFC2045.Token.equal t.mime_type (Rfc.RFC2045.Token.of_string mime_type))
  &&
  Option.value_map mime_subtype ~default:true
    ~f:(fun mime_subtype ->
      Rfc.RFC2045.Token.equal t.mime_subtype (Rfc.RFC2045.Token.of_string mime_subtype))
;;

(* Some convenience functions for working with mime types *)
let is_multipart t = is ~mime_type:"multipart" t;;
let is_digest t = is ~mime_type:"multipart" ~mime_subtype:"digest" t;;

let multipart_boundary t =
  if is_multipart t
  then Option.map ~f:Boundary.create
         (Params.last t.params "boundary")
  else None
;;

let of_grammar (mime_type, mime_subtype, params) =
  { mime_type    = Rfc.RFC2045.Token.of_string mime_type
  ; mime_subtype = Rfc.RFC2045.Token.of_string mime_subtype
  ; params
  }
;;

let of_string x =
  of_grammar (Media_type_grammar.content_type
                Media_type_lexer.content_type (Lexing.from_string x))
;;

let last headers =
  Option.bind (Headers.last ~whitespace:`Raw headers "Content-Type")
    ~f:(fun field -> Option.try_with (fun () -> of_string field))

let default_default =
  { mime_type = Rfc.RFC2045.Token.of_string "text";
    mime_subtype = Rfc.RFC2045.Token.of_string "plain";
    params = [(Headers.Name.of_string "charset","us-ascii")]
  }
;;

let default_digest =
  { mime_type = Rfc.RFC2045.Token.of_string "message";
    mime_subtype = Rfc.RFC2045.Token.of_string "rfc2822";
    params = []
  }
;;

let default ~parent =
  if Option.value_map parent ~f:is_digest ~default:false
  then default_digest
  else default_default
;;

let%test_module "Media_type" =
  (module struct

    let headers =
      ["Content-Type", "multipart/mixed;\nboundary=\"BOUNDARY\""]
      |> Headers.of_list ~whitespace:`Normalize

    let%test_unit _ =
      [%test_result: t]
        (last headers |> Option.value_exn)
        ~expect:{ mime_type = Rfc.RFC2045.Token.of_string "multipart"
                ; mime_subtype = Rfc.RFC2045.Token.of_string "mixed"
                ; params = ["boundary", "BOUNDARY"] }
  end)

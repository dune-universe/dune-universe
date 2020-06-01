open Core

module Params = struct
  type t = (Headers.Name.t * string) list [@@deriving sexp_of, compare]

  let last t name =
    let name = Headers.Name.of_string name in
    List.fold t ~init:None ~f:(fun r (k, v) ->
      if Headers.Name.equal name k then Some v else r)
  ;;

  let to_key_value_string (k, v) =
    Headers.Name.to_string k ^ "=" ^ Rfc.RFC2045.Token.is_valid_or_quote v
  ;;
end

type t =
  { mime_type : Rfc.RFC2045.Token.t
  ; mime_subtype : Rfc.RFC2045.Token.t
  ; params : Params.t
  }
[@@deriving compare, fields, sexp_of]

let mime_type t = Rfc.RFC2045.Token.to_lowercase_string t.mime_type
let mime_subtype t = Rfc.RFC2045.Token.to_lowercase_string t.mime_subtype

let create mime_type mime_subtype =
  { mime_type = Rfc.RFC2045.Token.of_string mime_type
  ; mime_subtype = Rfc.RFC2045.Token.of_string mime_subtype
  ; params = []
  }
;;

let set_param t ~name ~value =
  { t with
    params =
      List.filter t.params ~f:(fun (k, _) -> not (Headers.Name.is name k))
      @ [ name, value ]
  }
;;

let is ?mime_type ?mime_subtype t =
  let module T = Rfc.RFC2045.Token in
  Option.value_map mime_type ~default:true ~f:(fun mime_type ->
    T.equal t.mime_type (T.of_string mime_type))
  && Option.value_map mime_subtype ~default:true ~f:(fun mime_subtype ->
    T.equal t.mime_subtype (T.of_string mime_subtype))
;;

(* Some convenience functions for working with mime types *)
let is_text t = is ~mime_type:"text" t
let is_multipart t = is ~mime_type:"multipart" t
let is_multipart_report = is ~mime_type:"multipart" ~mime_subtype:"report"
let is_message_rfc822 = is ~mime_type:"message" ~mime_subtype:"rfc822"
let is_digest t = is ~mime_type:"multipart" ~mime_subtype:"digest" t

let multipart_boundary t =
  if is_multipart t
  then Option.map ~f:Boundary.of_string (Params.last t.params "boundary")
  else None
;;

let set_multipart_boundary t boundary =
  set_param t ~name:"boundary" ~value:(Boundary.to_string boundary)
;;

let of_grammar (mime_type, mime_subtype, params) =
  { mime_type = Rfc.RFC2045.Token.of_string mime_type
  ; mime_subtype = Rfc.RFC2045.Token.of_string mime_subtype
  ; params
  }
;;

let of_string x =
  of_grammar
    (Media_type_grammar.content_type Media_type_lexer.content_type (Lexing.from_string x))
;;

let to_string t =
  String.concat
    ~sep:"; "
    ((Rfc.RFC2045.Token.to_lowercase_string t.mime_type
      ^ "/"
      ^ Rfc.RFC2045.Token.to_lowercase_string t.mime_subtype)
     :: List.map t.params ~f:Params.to_key_value_string)
;;

let from_headers headers =
  Option.bind
    (Headers.last ~normalize:`None headers "Content-Type")
    ~f:(fun field -> Option.try_with (fun () -> of_string field))
;;

let set_headers headers t =
  match from_headers headers with
  | Some t' when [%compare.equal: t] t t' -> headers
  | _ -> Headers.set_at_bottom headers ~name:"Content-Type" ~value:(to_string t)
;;

let message_rfc822 = create "message" "rfc822"

let text_plain ?(charset = "us-ascii") () =
  create "text" "plain" |> set_param ~name:"charset" ~value:charset
;;

let create_multipart mime_subtype ~boundary =
  set_multipart_boundary (create "multipart" mime_subtype) boundary
;;

let default_default = text_plain ()
let default_digest = message_rfc822

let default ?parent () =
  if Option.value_map parent ~f:is_digest ~default:false
  then default_digest
  else default_default
;;

let%expect_test _ =
  let test t =
    let s = to_string t in
    print_endline s;
    let t' = of_string s in
    [%test_eq: t] t t'
  in
  test message_rfc822;
  [%expect {| message/rfc822 |}];
  test (text_plain ());
  [%expect {| text/plain; charset=us-ascii |}];
  test (text_plain () ~charset:"nonsense charset");
  [%expect {| text/plain; charset="nonsense charset" |}];
  test default_default;
  [%expect {| text/plain; charset=us-ascii |}];
  test default_digest;
  [%expect {| message/rfc822 |}];
  test (create "application" "json");
  [%expect {| application/json |}];
  test (create_multipart "related" ~boundary:(Boundary.of_string "--::_BOUNDARY_::--"));
  [%expect {| multipart/related; boundary="--::_BOUNDARY_::--" |}];
  test (create_multipart "related" ~boundary:(Boundary.of_string "boundary"));
  [%expect {| multipart/related; boundary=boundary |}];
  test
    (create_multipart
       "related"
       ~boundary:(Boundary.of_string "questionable boundary string"));
  [%expect {| multipart/related; boundary="questionable boundary string" |}];
  let test s =
    let t = of_string s in
    printf !"%{sexp:t}\n" t;
    test t
  in
  test {|text/plain|};
  [%expect {|
    ((mime_type text) (mime_subtype plain) (params ()))
    text/plain |}];
  test {|test/plain; charset="us-ascii"|};
  [%expect
    {|
    ((mime_type test) (mime_subtype plain) (params ((charset us-ascii))))
    test/plain; charset=us-ascii |}];
  test {|multipart/related;
                boundary="--::FOO"|};
  [%expect
    {|
    ((mime_type multipart) (mime_subtype related) (params ((boundary --::FOO))))
    multipart/related; boundary="--::FOO" |}]
;;

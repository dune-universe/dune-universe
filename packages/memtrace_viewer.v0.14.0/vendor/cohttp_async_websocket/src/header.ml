open! Core
open! Async
open! Import
include Cohttp.Header

(** Host Header: https://tools.ietf.org/html/rfc7230#section-5.4

    The Host header field (defined at [2]) has the following ABNF grammar:
    {[
      Host = uri-host [ ":" port ] ; Section 2.7.1
    ]}

    Following what "uri-host" is defined as:
    which can be expanded as follows (https://tools.ietf.org/html/rfc7230#appendix-B ):
    {[
      uri-host = <host, see [RFC3986], Section 3.2.2>
    ]}
    which is explained by (https://tools.ietf.org/html/rfc3986#section-3.2.2 ) as

    {[
      host        = IP-literal / IPv4address / reg-name
    ]}

    so "uri-host" is an IP address or what is colloquially referred to as a hostname.

    Therefore, a host header is a host and optional port.

    Example:

    For example, a GET request to the origin server for
    <http://www.example.org/pub/WWW/> would begin with:

    GET /pub/WWW/ HTTP/1.1
    Host: www.example.org

*)
let host_header_name = "host"

(** From https://tools.ietf.org/html/rfc6454 "The Web Origin Concept"
    {v
           3.2.1.  Examples

              All of the following resources have the same origin:

              http://example.com/
              http://example.com:80/
              http://example.com/path/file

              Each of the URIs has the same scheme, host, and port components.

              Each of the following resources has a different origin from the
              others.

              http://example.com/
              http://example.com:8080/
              http://www.example.com/
              https://example.com:80/
              https://example.com/
              http://example.org/
              http://ietf.org/

              In each case, at least one of the scheme, host, and port component
              will differ from the others in the list.

         v}
    {v
           5.  Comparing Origins

              Two origins are "the same" if, and only if, they are identical.  In
              particular:

              o  If the two origins are scheme/host/port triples, the two origins
                 are the same if, and only if, they have identical schemes, hosts,
                 and ports.

              o  An origin that is a globally unique identifier cannot be the same
                 as an origin that is a scheme/host/port triple.

              Two URIs are same-origin if their origins are the same.

                 NOTE: A URI is not necessarily same-origin with itself.  For
                 example, a data URI [RFC2397] is not same-origin with itself
                 because data URIs do not use a server-based naming authority and
                 therefore have globally unique identifiers as origins.
         v}
    {v
         6.1.  Unicode Serialization of an Origin

            The unicode-serialization of an origin is the value returned by the
            following algorithm:

            1.  If the origin is not a scheme/host/port triple, then return the
                string

                   null

                (i.e., the code point sequence U+006E, U+0075, U+006C, U+006C)
                and abort these steps.

            2.  Otherwise, let result be the scheme part of the origin triple.

            3.  Append the string "://" to result.

            4.  Append each component of the host part of the origin triple
                (converted as follows) to the result, separated by U+002E FULL
                STOP code points ("."):

                1.  If the component is an A-label, use the corresponding U-label
                    instead (see [RFC5890] and [RFC5891]).

                2.  Otherwise, use the component verbatim.

            5.  If the port part of the origin triple is different from the
                default port for the protocol given by the scheme part of the
                origin triple:

                1.  Append a U+003A COLON code point (":") and the given port, in
                    base ten, to result.

            6.  Return result.

         v}
    {v
           7.1.  Syntax

              The Origin header field has the following syntax:

              origin              = "Origin:" OWS origin-list-or-null OWS
              origin-list-or-null = %x6E %x75 %x6C %x6C / origin-list
              origin-list         = serialized-origin *( SP serialized-origin )
              serialized-origin   = scheme "://" host [ ":" port ]
                                  ; <scheme>, <host>, <port> from RFC 3986
         v}
*)
let origin_header_name = "origin"

module Web_host_and_port : sig
  type t = private
    { header : string
    ; host : string
    ; port : int option
    }
  [@@deriving sexp_of]

  val of_origin_header : string -> ([ `Scheme of string ] * t) Or_error.t
  val of_host_header : string -> scheme:string -> t Or_error.t
  val validate_equal : t -> t -> unit Or_error.t
end = struct
  type t =
    { header : string
    ; host : string
    ; port : int option
    }
  [@@deriving fields, sexp_of]

  let validate_equal t1 t2 =
    let matches compare acc field =
      let f1 = Field.get field t1 in
      let f2 = Field.get field t2 in
      let result =
        if not (compare f1 f2)
        then error_s [%message "parts do not match" ~part:(Field.name field)]
        else Ok ()
      in
      result :: acc
    in
    let just_for_debugging acc (_ : _ Field.t) = acc in
    Fields.fold
      ~init:[]
      ~host:(matches [%compare.equal: string])
      ~port:(matches [%compare.equal: int option])
      ~header:just_for_debugging
    |> Or_error.all_unit
    |> Or_error.tag_s ~tag:[%message "" ~_:(t1 : t) ~_:(t2 : t)]
  ;;

  let remove_superfluous_port_specification ~scheme ~port =
    let default_port =
      match scheme with
      | "http" | "ws" -> Ok 80
      | "https" | "wss" -> Ok 443
      | (_ : string) -> error_s [%message "Unknown scheme" ~_:scheme]
    in
    Or_error.map default_port ~f:(fun default_port ->
      let open Option.Let_syntax in
      let%bind specified_port = port in
      if default_port <> specified_port then Some specified_port else None)
  ;;

  let of_host_header value ~scheme =
    let open Or_error.Let_syntax in
    let header = host_header_name in
    Or_error.try_with_join (fun () ->
      match String.lsplit2 ~on:':' value with
      | Some (host, port) ->
        let%map port =
          remove_superfluous_port_specification
            ~scheme
            ~port:(Some (Int.of_string port))
        in
        { header; host; port }
      | None -> Ok { header; host = value; port = None })
  ;;

  let of_origin_header origin =
    let open Or_error.Let_syntax in
    Or_error.try_with_join (fun () ->
      let uri = Uri.of_string origin in
      match Uri.scheme uri with
      | None -> error_s [%message "No scheme" origin]
      | Some scheme ->
        let%bind port =
          remove_superfluous_port_specification ~scheme ~port:(Uri.port uri)
        in
        (match Uri.host uri with
         | None -> error_s [%message "No host" origin]
         | Some host -> Ok (`Scheme scheme, { header = origin_header_name; host; port })))
  ;;
end

let origin_and_host_headers_match ~origin ~host =
  let open Or_error.Let_syntax in
  let%bind `Scheme scheme, origin = Web_host_and_port.of_origin_header origin in
  let%bind host = Web_host_and_port.of_host_header host ~scheme in
  Web_host_and_port.validate_equal origin host
;;

module Expect_test_config = Core.Expect_test_config

let%test_module _ =
  (module struct
    let check ~host ~origin =
      print_s [%sexp (origin_and_host_headers_match ~origin ~host : unit Or_error.t)]
    ;;

    let%expect_test "Host matching" =
      check ~host:"ontology" ~origin:"https://ontology";
      [%expect {| (Ok ()) |}];
      check ~host:"bond-webs:8443" ~origin:"https://bond-webs:8443";
      [%expect {| (Ok ()) |}];
      check ~host:"site-without-port" ~origin:"https://site-without-port:1337";
      [%expect
        {|
      (Error
       ((((header origin) (host site-without-port) (port (1337)))
         ((header host) (host site-without-port) (port ())))
        ("parts do not match" (part port)))) |}]
    ;;

    let%expect_test "Host matching fails on unicode URIs, no work has been put into \
                     supporting them"
      =
      check
        ~host:"internal-siteä.attacker.co.uk"
        ~origin:"https://internal-siteä.attacker.co.uk";
      [%expect
        {|
        (Error
         ((((header origin) (host internal-site) (port ()))
           ((header host) (host "internal-site\195\164.attacker.co.uk") (port ())))
          ("parts do not match" (part host)))) |}]
    ;;

    (* https://tools.ietf.org/html/rfc6454#section-3.2.1 explains that all of the
       following have the same origin:

       {v http://example.com/
          http://example.com:80/
          http://example.com/path/file
       v}

       since the origin only compares the scheme, host, and port; and the default port for
       protocol http is 80. *)

    let%expect_test "Implicit port" =
      check ~host:"example.com" ~origin:"http://example.com:80";
      check ~host:"example.com:80" ~origin:"http://example.com:80";
      check ~host:"example.com" ~origin:"https://example.com:443";
      check ~host:"example.com:443" ~origin:"https://example.com:443";
      check ~host:"example.com" ~origin:"ws://example.com:80";
      check ~host:"example.com" ~origin:"wss://example.com:443";
      [%expect
        {|
          (Ok ())
          (Ok ())
          (Ok ())
          (Ok ())
          (Ok ())
          (Ok ()) |}]
    ;;
  end)
;;

let origin_and_host_match t =
  let host = get t host_header_name in
  let origin = get t origin_header_name in
  match Option.both host origin with
  | None ->
    error_s
      [%message
        "Missing one of origin or host header"
          (origin : string option)
          (host : string option)]
  | Some (host, origin) -> origin_and_host_headers_match ~origin ~host
;;

let origin_matches_host_or_is_one_of t ~origins =
  match origin_and_host_match t with
  | Ok () -> Ok ()
  | Error (_ : Error.t) as host_match_error ->
    (match get t origin_header_name with
     | None -> error_s [%message "No origin header present"]
     | Some origin ->
       if List.exists ~f:(String.equal origin) origins
       then Ok ()
       else
         Or_error.combine_errors_unit
           [ host_match_error
           ; error_s
               [%message "origin not in inclusion list" origin (origins : string list)]
           ])
;;

let%test_module _ =
  (module struct
    let maybe_add value ~name headers =
      match value with
      | None -> headers
      | Some value -> add headers name value
    ;;

    let init_header ~origin ~host =
      init ()
      |> maybe_add host ~name:host_header_name
      |> maybe_add origin ~name:origin_header_name
    ;;

    let check ~host ~origin ~f =
      let result = f (init_header ~origin ~host) in
      print_s [%sexp (result : unit Or_error.t)]
    ;;

    let%expect_test "Full parse of header" =
      let check = check ~f:origin_and_host_match in
      check ~host:None ~origin:(Some "http://somehost");
      [%expect
        {|
    (Error
     ("Missing one of origin or host header" (origin (http://somehost))
      (host ()))) |}];
      check ~host:(Some "asdf") ~origin:None;
      [%expect
        {| (Error ("Missing one of origin or host header" (origin ()) (host (asdf)))) |}];
      check ~host:(Some "asdf") ~origin:(Some "https://somehost");
      [%expect
        {|
      (Error
       ((((header origin) (host somehost) (port ()))
         ((header host) (host asdf) (port ())))
        ("parts do not match" (part host)))) |}];
      check ~host:(Some "somehost") ~origin:(Some "https://somehost:994");
      [%expect
        {|
      (Error
       ((((header origin) (host somehost) (port (994)))
         ((header host) (host somehost) (port ())))
        ("parts do not match" (part port)))) |}];
      check ~host:(Some "wrong") ~origin:(Some "https://somehost:994");
      [%expect
        {|
      (Error
       ((((header origin) (host somehost) (port (994)))
         ((header host) (host wrong) (port ())))
        ("parts do not match" (part port)) ("parts do not match" (part host)))) |}];
      check ~host:(Some "somehost:994") ~origin:(Some "https://somehost:994");
      [%expect {| (Ok ()) |}]
    ;;

    let%expect_test "origin_matches_host_or_is_one_of" =
      let check ~origins = check ~f:(origin_matches_host_or_is_one_of ~origins) in
      check ~host:None ~origin:(Some "http://somehost") ~origins:[];
      [%expect
        {|
      (Error
       (("Missing one of origin or host header" (origin (http://somehost))
         (host ()))
        ("origin not in inclusion list" http://somehost (origins ())))) |}];
      let host = Some "somehost" in
      check ~host ~origin:(Some "http://somehost") ~origins:[];
      [%expect {| (Ok ()) |}];
      check ~host ~origin:(Some "http://somehost") ~origins:[ "http://host" ];
      [%expect {| (Ok ()) |}];
      check
        ~host:(Some "somehost")
        ~origin:(Some "http://host")
        ~origins:[ "http://host" ];
      [%expect {| (Ok ()) |}]
    ;;
  end)
;;

let websocket_subprotocol_header = "sec-websocket-protocol"

let websocket_subprotocols t =
  get_multi t websocket_subprotocol_header |> List.concat_map ~f:(String.split ~on:',')
;;

let add_websocket_subprotocol t ~subprotocol:value =
  add t websocket_subprotocol_header value
;;

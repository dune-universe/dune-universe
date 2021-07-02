(** Encoding primitives for Dream.

    The main use case for this library is to compress the response body of a
    given list of endpoints. To do this, you can simply add
    [Dream_encoding.compress] to your list of middlewares:

    {[
      let () =
        Dream.run
        @@ Dream.logger
        @@ Dream_encoding.compress
        @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html "Hello World!") ]
        @@ Dream.not_found
    ]}

    For more advanced use cases, some utility functions are also exposed. In
    particular, functions to retrieve the [Content-Encoding] and
    [Accept-Encoding] headers (respectively, [content_encoding] and
    [accept_encoding]), as well as [with_encoded_body], a function to compress
    the body of a response.

    As of now, the supported encoding directives are [deflate] and [gzip]. More
    directive will be supported when their support land in [decompress], the
    underlying compression library used by Dream_encoding. *)

val compress : Dream.middleware
(** Middleware that reads the [Accept-Encoding] header of the request and
    compresses the responses with the preferred supported algorithm. *)

val decompress : Dream.middleware
(** Middleware that reads the [Content-Encoding] of the request and decompresses
    the body if all of the directives of the header are supported.

    If one or more of the directive is not supported, an HTTP response
    [415 Unsupported Media Type] is returned to the client.

    Note that although HTTP supports encoding requests, it is rarely used in
    practice. See [compress] to for a middleware that compresses the responses
    instead. *)

val with_encoded_body
  :  ?algorithm:[ `Deflate | `Gzip ]
  -> string
  -> Dream.response
  -> Dream.response
(** [with_encoded_body ?algorithm body response] replaces the body of the
    response with [body] compressed with [algorithm] and adds the corresponding
    [Content-Encoding] header.

    [algorithm] defaults to [`Deflate]. *)

val accepted_encodings
  :  'a Dream.message
  -> [ `Gzip | `Compress | `Deflate | `Identity | `Any | `Unknown of string ]
     list
     option
(** Retrieve the list of accepted encoding directives from the [Accept-Encoding]
    header, ordered by quality weight in decreasing order.

    If the request does not have an [Accept-Encoding] header, this returns
    [None]. *)

val accepted_encodings_with_weights
  :  'a Dream.message
  -> ([ `Gzip | `Compress | `Deflate | `Identity | `Any | `Unknown of string ]
     * int)
     list
     option
(** Same as [accepted_encoding], but returns the quality weights associated to
    the encoding directive. *)

val content_encodings
  :  'a Dream.message
  -> [ `Gzip | `Compress | `Deflate | `Identity | `Any | `Unknown of string ]
     list
     option
(** Retrieve the list of content encoding directives from the [Content-Encoding]
    header.

    If the request does not have an [Content-Encoding] header, this returns
    [None]. *)

val preferred_content_encoding : 'a Dream.message -> [ `Deflate | `Gzip ] option
(** Retrieve preferred encoding directive from the [Accept-Encoding].

    The preferred encoding directive is the first supported algorithm in the
    list of accepted directives sorted by quality weight.

    If [*] is given as the preferred encoding, [`Gzip] is returned. This is to
    be on par with the behavior of [compress].

    If no algorithm is supported, or if the request does not have an
    [Accept-Encoding] header, this returns [None]. *)

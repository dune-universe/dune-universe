open Core

type t =
  | Error of string
  | Success of string
  | Missing   

module Environment : sig
  val system : String.t
end

module Cache : sig

  (* Currently unused *)
  val download_location : String.t

  (* The timeout age of the cache *)
  val max_age : Float.t

  (* The directory of the cache *)
  val directory : String.t

  (* Gets the location of a command in the cache*)
  val get_file_path : String.t -> String.t -> String.t

  (* Load a page from the cache *)
  val load_page : String.t -> String.t -> t

  (* Store a page in the cache *)
  val store_page : String.t -> String.t -> String.t -> unit
end

module Remote : sig
  (* An address to the files *)
  val default_remote : String.t

  val get_page_url
    : ?remote: String.t ->
    ?platform: String.t ->
    String.t ->
    String.t

  val get_page
    : ?remote: String.t ->
    ?platform: String.t ->
    String.t -> t
end

val get_page : String.t -> String.t -> t
